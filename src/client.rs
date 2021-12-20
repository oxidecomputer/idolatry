// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::{common, syntax};
use std::env;
use std::fs::File;
use std::path::PathBuf;

pub fn build_client_stub(
    source: &str,
    stub_name: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let out = &PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let stub_file = File::create(out.join(stub_name)).unwrap();

    generate_client_stub_from_file(source, stub_file)?;
    println!("cargo:rerun-if-changed={}", source);
    Ok(())
}

pub fn generate_client_stub_from_file(
    source: impl AsRef<std::path::Path>,
    out: impl std::io::Write,
) -> Result<(), Box<dyn std::error::Error>> {
    let text = std::fs::read_to_string(source)?;
    let iface: syntax::Interface = ron::de::from_str(&text)?;
    generate_client_stub(&iface, out)
}

pub fn generate_client_stub(
    iface: &syntax::Interface,
    mut out: impl std::io::Write,
) -> Result<(), Box<dyn std::error::Error>> {
    common::generate_op_enum(iface, &mut out)?;

    writeln!(out, "#[derive(Clone, Debug)]")?;
    writeln!(out, "pub struct {} {{", iface.name)?;
    writeln!(out, "    current_id: core::cell::Cell<userlib::TaskId>,")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(out, "impl From<userlib::TaskId> for {} {{", iface.name)?;
    writeln!(out, "    fn from(x: userlib::TaskId) -> Self {{")?;
    writeln!(
        out,
        "        Self {{ current_id: core::cell::Cell::new(x) }}"
    )?;
    writeln!(out, "    }}")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(out, "impl {} {{", iface.name)?;
    for (idx, (name, op)) in iface.ops.iter().enumerate() {
        writeln!(out, "    // operation: {} ({})", name, idx)?;
        if op.idempotent {
            writeln!(out, "    // idempotent - will auto-retry")?;
        } else {
            writeln!(
                out,
                "    // not idempotent, error type must represent death"
            )?;
        }

        // Let's do some checks

        if op.idempotent {
            // An idempotent operation with a read-write lease seems like a
            // problem, since it could forward half-initialized state from one
            // instance of the server to the next, potentially propagating the
            // crash; we could potentially handle it but for now, we'll reject
            // it.
            if op.leases.values().any(|lease| lease.read && lease.write) {
                return Err("idempotent operation with read/write lease".into());
            }
        } else {
            // A non-idempotent operation had better have an error type.
            match &op.reply {
                syntax::Reply::Result { .. } => (),
            }
        }

        writeln!(out, "    pub fn {}(", name)?;
        writeln!(out, "        &self,")?;
        for (name, arg) in &op.args {
            writeln!(out, "        {}: {},", name, arg.ty.0)?;
        }
        for (name, lease) in &op.leases {
            let reftype = if lease.write {
                "&mut "
            } else if lease.read {
                "&"
            } else {
                panic!("lease {} grants no access", name);
            };
            writeln!(out, "        {}: {}{},", name, reftype, lease.ty.0)?;
        }
        write!(out, "    )")?;
        match &op.reply {
            syntax::Reply::Result { ok, err } => {
                write!(out, " -> Result<{}, ", ok.display())?;
                match err {
                    syntax::Error::CLike(ty) => {
                        write!(out, "{}", ty.0)?;
                    }
                }
                write!(out, ">")?;
            }
        }
        writeln!(out, " {{")?;

        // Map the args with user-chosen names, which are good for rustdoc, into
        // args with names that are guaranteed not to collide with our generated
        // identifiers.
        write!(out, "        let (")?;
        for argname in op.args.keys() {
            write!(out, "arg_{},", argname)?;
        }
        for leasename in op.leases.keys() {
            write!(out, "arg_{},", leasename)?;
        }
        write!(out, ") = (")?;
        for argname in op.args.keys() {
            write!(out, "{},", argname)?;
        }
        for leasename in op.leases.keys() {
            write!(out, "{},", leasename)?;
        }
        writeln!(out, ");")?;

        // Perform lease validation.
        for (leasename, lease) in &op.leases {
            if let Some(n) = lease.max_len {
                writeln!(out, "        if arg_{}.len() > {} {{", leasename, n)?;
                // Note: we're not generating a panic message in the client to
                // save ROM space. If the user chases the line number into the
                // client stub source file the error should be clear.
                writeln!(out, "            panic!();")?;
                writeln!(out, "        }}")?;
            }
        }

        // Define args struct.
        writeln!(out, "        #[allow(non_camel_case_types)]")?;
        writeln!(out, "        #[derive(zerocopy::AsBytes)]")?;
        writeln!(out, "        #[repr(C, packed)]")?;
        writeln!(out, "        struct {}_{}_ARGS {{", iface.name, name)?;
        for (argname, arg) in &op.args {
            writeln!(out, "            {}: {},", argname, arg.ty.0)?;
        }
        writeln!(out, "        }}")?;
        writeln!(out)?;

        // Determine required size of reply buffer.
        writeln!(out, "        const REPLY_SIZE: usize = {{")?;
        match &op.reply {
            syntax::Reply::Result { ok, err } => {
                writeln!(
                    out,
                    "            let oksize = core::mem::size_of::<{}>();",
                    ok.display()
                )?;
                match err {
                    syntax::Error::CLike(_ty) => {
                        writeln!(out, "            let errsize = 0;")?;
                    }
                }

                writeln!(
                    out,
                    "            if oksize > errsize {{ oksize  }} else {{ errsize }}"
                )?;
            }
        }
        writeln!(out, "        }};")?;
        writeln!(out)?;

        // Create instance of args struct from args.
        writeln!(out, "        let args = {}_{}_ARGS {{", iface.name, name)?;
        for argname in op.args.keys() {
            writeln!(out, "            {0}: arg_{0},", argname)?;
        }
        writeln!(out, "        }};")?;
        writeln!(out)?;

        // Create reply buffer.
        writeln!(out, "        let mut reply = [0u8; REPLY_SIZE];")?;
        writeln!(out)?;

        writeln!(out, "        let task = self.current_id.get();")?;
        writeln!(out)?;
        writeln!(out, "        let (rc, len) = sys_send(")?;
        writeln!(out, "            task,")?;
        writeln!(out, "            {}Operation::{} as u16,", iface.name, name)?;
        writeln!(out, "            zerocopy::AsBytes::as_bytes(&args),")?;
        writeln!(out, "            &mut reply,")?;
        writeln!(out, "            &[")?;
        for (leasename, lease) in &op.leases {
            let ctor = match (lease.read, lease.write) {
                (true, true) => "read_write",
                (false, true) => "write_only",
                (true, false) => "read_only",
                (false, false) => panic!("should have been caught above"),
            };
            writeln!(
                out,
                "                userlib::Lease::{}(arg_{}),",
                ctor, leasename
            )?;
        }
        writeln!(out, "            ],")?;
        writeln!(out, "        );")?;

        match &op.reply {
            syntax::Reply::Result { ok, err } => {
                let reply_ty = format!("{}_{}_REPLY", iface.name, name);
                writeln!(out, "        if rc == 0 {{")?;
                writeln!(out, "            #[derive(zerocopy::FromBytes, zerocopy::Unaligned)]")?;
                writeln!(out, "            #[repr(C, packed)]")?;
                writeln!(out, "            struct {} {{", reply_ty)?;
                writeln!(out, "                value: {},", ok.repr_ty().0)?;
                writeln!(out, "            }}")?;
                writeln!(out, "            let lv = zerocopy::LayoutVerified::<_, {}>::new_unaligned(&reply[..])", reply_ty)?;
                writeln!(out, "                .unwrap();")?;
                writeln!(out, "            let v: {} = lv.value;", ok.repr_ty().0)?;
                match &ok.recv {
                    syntax::RecvStrategy::FromBytes => {
                        writeln!(out, "            Ok(v)")?;
                    }
                    syntax::RecvStrategy::From(_, None) => {
                        writeln!(out, "            Ok(v.into())")?;
                    }
                    syntax::RecvStrategy::From(_, Some(f)) => {
                        writeln!(out, "            Ok({}(v))", f)?;
                    }
                    syntax::RecvStrategy::FromPrimitive(p) => {
                        writeln!(out, "            Ok(<{} as userlib::FromPrimitive>::from_{}(v))", ok.ty.0, p.0)?;
                    }
                }
                writeln!(out, "        }} else {{")?;
                match err {
                    syntax::Error::CLike(ty) => {
                        writeln!(out, "            assert!(len == 0);")?;
                        writeln!(
                            out,
                            "            if let Some(g) = userlib::extract_new_generation(rc) {{"
                        )?;
                        writeln!(out, "                self.current_id.set(userlib::TaskId::for_index_and_gen(task.index(), g));")?;
                        writeln!(out, "            }}")?;
                        writeln!(
                            out,
                            "            Err(<{} as core::convert::TryFrom<u32>>::try_from(rc)",
                            ty.0
                        )?;
                        writeln!(out, "                .unwrap())")?;
                    }
                }
                writeln!(out, "        }}")?;
            }
        }

        writeln!(out, "    }}")?;

        writeln!(out)?;
    }
    writeln!(out, "}}")?;
    writeln!(out)?;
    Ok(())
}

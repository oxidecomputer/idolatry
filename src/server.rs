// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::{common, syntax};
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

pub enum ServerStyle {
    InOrder,
}

pub fn build_server_support(
    source: &str,
    stub_name: &str,
    style: ServerStyle,
) -> Result<(), Box<dyn std::error::Error>> {
    let out = &PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let mut stub_file = File::create(out.join(stub_name)).unwrap();

    let text = std::fs::read_to_string(source)?;
    let iface: syntax::Interface = ron::de::from_str(&text)?;

    generate_server_constants(&iface, &mut stub_file)?;
    generate_server_conversions(&iface, &mut stub_file)?;
    common::generate_op_enum(&iface, &mut stub_file)?;
    generate_server_op_impl(&iface, &mut stub_file)?;

    match style {
        ServerStyle::InOrder => {
            generate_server_in_order_trait(&iface, &mut stub_file)?;
        }
    }

    generate_server_section(&iface, &text, &mut stub_file)?;
    println!("cargo:rerun-if-changed={}", source);
    Ok(())
}

pub fn generate_server_constants(
    iface: &syntax::Interface,
    mut out: impl Write,
) -> Result<(), Box<dyn std::error::Error>> {
    // Generate message sizing constants for each message.
    let mut upper_names = vec![];
    for (name, op) in &iface.ops {
        let upper_name = name.to_uppercase();

        writeln!(out, "pub const {}_MSG_SIZE: usize = 0", upper_name)?;

        match op.encoding {
            syntax::Encoding::Zerocopy | syntax::Encoding::Ssmarshal => {
                // Zerocopy moves fields as a packed struct; Ssmarshal
                // serializes them, but guarantees that the serialized size will
                // be no larger than the size of the input types. So this method
                // works for both.
                for arg in op.args.values() {
                    writeln!(
                        out,
                        "    + core::mem::size_of::<{}>()",
                        arg.ty.0
                    )?;
                }
            }
        }
        writeln!(out, "    ;")?;

        write!(out, "pub const {}_REPLY_SIZE: usize =", upper_name)?;
        match &op.reply {
            syntax::Reply::Result { ok, .. } => {
                match op.encoding {
                    syntax::Encoding::Zerocopy
                    | syntax::Encoding::Ssmarshal => {
                        // This strategy only uses bytes for the OK side of the type,
                        // and only sends one type, so:
                        writeln!(
                            out,
                            "core::mem::size_of::<{}>();",
                            ok.display()
                        )?;
                    }
                }
            }
            syntax::Reply::Simple(t) => match op.encoding {
                syntax::Encoding::Zerocopy | syntax::Encoding::Ssmarshal => {
                    writeln!(out, "core::mem::size_of::<{}>();", t.display())?;
                }
            },
        }

        upper_names.push(upper_name);
    }

    writeln!(out, "const fn max_incoming_size() -> usize {{")?;
    writeln!(out, "    let mut max = 0;")?;
    for un in upper_names {
        writeln!(out, "    if max < {}_MSG_SIZE {{", un)?;
        writeln!(out, "        max = {}_MSG_SIZE;", un)?;
        writeln!(out, "    }}")?;
    }
    writeln!(out, "    max")?;
    writeln!(out, "}}")?;
    writeln!(out, "pub const INCOMING_SIZE: usize = max_incoming_size();")?;
    Ok(())
}

pub fn generate_server_conversions(
    iface: &syntax::Interface,
    mut out: impl Write,
) -> Result<(), Box<dyn std::error::Error>> {
    for (name, op) in &iface.ops {
        // Define args struct.
        writeln!(out, "#[allow(non_camel_case_types)]")?;
        match op.encoding {
            syntax::Encoding::Zerocopy => {
                writeln!(out, "#[repr(C, packed)]")?;
                writeln!(
                    out,
                    "#[derive(Copy, Clone, zerocopy::FromBytes, zerocopy::Unaligned)]"
                )?;
            }
            syntax::Encoding::Ssmarshal => {
                writeln!(out, "#[derive(Copy, Clone, serde::Deserialize)]")?;
            }
        }
        writeln!(out, "pub struct {}_{}_ARGS {{", iface.name, name)?;
        let mut need_args_impl = false;
        for (argname, arg) in &op.args {
            match &arg.recv {
                syntax::RecvStrategy::FromBytes => {
                    writeln!(out, "    pub {}: {},", argname, arg.ty.0)?;
                }
                syntax::RecvStrategy::FromPrimitive(ty)
                | syntax::RecvStrategy::From(ty, _) => {
                    writeln!(out, "    pub raw_{}: {},", argname, ty.0)?;
                    need_args_impl = true;
                }
            }
        }
        writeln!(out, "}}")?;
        writeln!(out)?;

        if need_args_impl {
            writeln!(out, "impl {}_{}_ARGS {{", iface.name, name)?;
            for (argname, arg) in &op.args {
                match &arg.recv {
                    syntax::RecvStrategy::FromPrimitive(ty) => {
                        writeln!(
                            out,
                            "    pub fn {}(&self) -> Option<{}> {{",
                            argname, arg.ty.0
                        )?;
                        writeln!(
                            out,
                            "        userlib::FromPrimitive::from_{}(self.raw_{})",
                            ty.0, argname
                        )?;
                        writeln!(out, "    }}")?;
                    }
                    _ => (),
                }
            }
            writeln!(out, "}}")?;
        }

        match op.encoding {
            syntax::Encoding::Zerocopy => {
                writeln!(out, "pub fn read_{}_msg(bytes: &[u8])", name)?;
                writeln!(out, "    -> Option<&{}_{}_ARGS>", iface.name, name)?;
                writeln!(out, "{{")?;
                writeln!(
                    out,
                    "    Some(zerocopy::LayoutVerified::<_, {}_{}_ARGS>::new_unaligned(bytes)?",
                    iface.name, name
                )?;
                writeln!(out, "        .into_ref())")?;
                writeln!(out, "}}")?;
            }
            syntax::Encoding::Ssmarshal => {
                writeln!(out, "pub fn read_{}_msg(bytes: &[u8])", name)?;
                writeln!(out, "    -> Option<{}_{}_ARGS>", iface.name, name)?;
                writeln!(out, "{{")?;
                writeln!(
                    out,
                    "    ssmarshal::deserialize(bytes).ok().map(|(x, _)| x)"
                )?;
                writeln!(out, "}}")?;
            }
        }
    }

    Ok(())
}

pub fn generate_server_op_impl(
    iface: &syntax::Interface,
    mut out: impl Write,
) -> Result<(), Box<dyn std::error::Error>> {
    writeln!(
        out,
        "impl idol_runtime::ServerOp for {}Operation {{",
        iface.name
    )?;

    writeln!(out, "    fn max_reply_size(&self) -> usize {{")?;
    writeln!(out, "        match self {{")?;
    for opname in iface.ops.keys() {
        writeln!(
            out,
            "            Self::{} => {}_REPLY_SIZE,",
            opname,
            opname.to_uppercase()
        )?;
    }
    writeln!(out, "        }}")?;
    writeln!(out, "    }}")?;
    writeln!(out)?;

    writeln!(out, "    fn required_lease_count(&self) -> usize {{")?;
    writeln!(out, "        match self {{")?;
    // Note: if we start allowing optional leases this will have to get fancier.
    for (opname, op) in &iface.ops {
        writeln!(out, "            Self::{} => {},", opname, op.leases.len(),)?;
    }
    writeln!(out, "        }}")?;
    writeln!(out, "    }}")?;
    writeln!(out)?;

    writeln!(out, "}}")?;

    Ok(())
}

pub fn generate_server_in_order_trait(
    iface: &syntax::Interface,
    mut out: impl Write,
) -> Result<(), Box<dyn std::error::Error>> {
    let trt = format!("InOrder{}Impl", iface.name);

    writeln!(out, "pub trait {} {{", trt)?;
    writeln!(
        out,
        "    fn recv_source(&self) -> Option<userlib::TaskId> {{"
    )?;
    writeln!(out, "        None")?;
    writeln!(out, "    }}")?;
    writeln!(out)?;
    writeln!(out, "    fn closed_recv_fail(&mut self) {{")?;
    writeln!(out, "        panic!()")?;
    writeln!(out, "    }}")?;
    writeln!(out)?;
    for (name, op) in &iface.ops {
        writeln!(out, "    fn {}(", name)?;
        writeln!(out, "        &mut self,")?;
        writeln!(out, "        msg: &userlib::RecvMessage,")?;
        for (argname, arg) in &op.args {
            writeln!(out, "        {}: {},", argname, arg.ty.0)?;
        }
        for (leasename, lease) in &op.leases {
            if let Some(n) = &lease.max_len {
                write!(out, "        {}: idol_runtime::LenLimit<idol_runtime::Leased<idol_runtime::", leasename)?;
                if lease.read {
                    write!(out, "R")?;
                }
                if lease.write {
                    write!(out, "W")?;
                }
                writeln!(out, ", {}>, {}>,", lease.ty.0, n)?;
            } else {
                write!(
                    out,
                    "        {}: idol_runtime::Leased<idol_runtime::",
                    leasename
                )?;
                if lease.read {
                    write!(out, "R")?;
                }
                if lease.write {
                    write!(out, "W")?;
                }
                writeln!(out, ", {}>,", lease.ty.0)?;
            }
        }
        write!(out, ")")?;

        match &op.reply {
            syntax::Reply::Result { ok, err } => {
                write!(
                    out,
                    " -> Result<{}, idol_runtime::RequestError<",
                    ok.display()
                )?;
                match err {
                    syntax::Error::CLike(ty) => {
                        write!(out, "{}", ty.0)?;
                    }
                }
                write!(out, ">>")?;
            }
            syntax::Reply::Simple(t) => {
                write!(out, " -> {}", t.display())?;
            }
        }
        writeln!(out, ";")?;
        writeln!(out)?;
    }
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(out, "impl<S: {}> idol_runtime::Server<{}Operation> for (core::marker::PhantomData<{1}Operation>, &'_ mut S) {{", trt, iface.name)?;

    writeln!(
        out,
        "    fn recv_source(&self) -> Option<userlib::TaskId> {{"
    )?;
    writeln!(out, "        <S as {}>::recv_source(self.1)", trt)?;
    writeln!(out, "    }}")?;
    writeln!(out)?;
    writeln!(out, "    fn closed_recv_fail(&mut self) {{")?;
    writeln!(out, "        <S as {}>::closed_recv_fail(self.1)", trt)?;
    writeln!(out, "    }}")?;
    writeln!(out)?;
    writeln!(out, "    fn handle(")?;
    writeln!(out, "        &mut self,")?;
    writeln!(out, "        op: {}Operation,", iface.name)?;
    writeln!(out, "        incoming: &[u8],")?;
    writeln!(out, "        rm: &userlib::RecvMessage,")?;
    writeln!(
        out,
        "    ) -> Result<(), idol_runtime::RequestError<u16>> {{"
    )?;
    writeln!(out, "        #[allow(unused_imports)]")?;
    writeln!(out, "        use core::convert::TryInto;")?;
    writeln!(out, "        use idol_runtime::ClientError;")?;
    writeln!(out, "        match op {{")?;
    for (opname, op) in &iface.ops {
        writeln!(out, "            {}Operation::{} => {{", iface.name, opname)?;
        writeln!(
            out,
            "                let {}args = read_{}_msg(incoming).ok_or(ClientError::BadMessageContents.fail())?;",
            if op.args.is_empty() { "_" } else { "" },
            opname
        )?;
        writeln!(out, "                let r = self.1.{}(", opname)?;
        writeln!(out, "                    rm,")?;
        for (argname, arg) in &op.args {
            match &arg.recv {
                syntax::RecvStrategy::FromBytes => {
                    writeln!(out, "                    args.{},", argname)?;
                }
                syntax::RecvStrategy::From(_, None) => {
                    writeln!(
                        out,
                        "                    args.raw_{}.into(),",
                        argname
                    )?;
                }
                syntax::RecvStrategy::From(_, Some(f)) => {
                    writeln!(
                        out,
                        "                    {}(args.raw_{}),",
                        f, argname
                    )?;
                }
                syntax::RecvStrategy::FromPrimitive(_) => {
                    writeln!(
                        out,
                        "                    args.{}().ok_or(ClientError::BadMessageContents.fail())?,",
                        argname
                    )?;
                }
            }
        }
        for (i, lease) in op.leases.values().enumerate() {
            // This is gross, but, let's spot us some slices :-(
            let fun = match (lease.read, lease.write) {
                (true, false) => "read_only",
                (false, true) => "write_only",
                (true, true) => "read_write",
                _ => unreachable!(),
            };

            let (suffix, limit) = if lease.ty.appears_unsized() {
                let max_len = if let Some(n) = lease.max_len {
                    // It's ok to unwrap the value in server code because we've
                    // just gotten it _out of_ a NonZeroU32 here, so we know
                    // it'll be statically valid.
                    format!(
                        ", Some(core::num::NonZeroU32::new({}).unwrap())",
                        n
                    )
                } else {
                    ", None".to_string()
                };
                ("_slice", max_len)
            } else {
                if lease.max_len.is_some() {
                    panic!("Lease {} on operation {}.{} has sized type but also max_len field",
                        i, iface.name, opname);
                }
                ("", "".to_string())
            };

            write!(out, "                    idol_runtime::Leased::{}{}(rm.sender, {}{}).ok_or(ClientError::BadLease.fail())?", fun, suffix, i, limit)?;
            if lease.max_len.is_some() {
                write!(out, ".try_into().unwrap()")?;
            }
            writeln!(out, ",")?;
        }
        writeln!(out, "                );")?;
        match &op.reply {
            syntax::Reply::Simple(_t) => {
                writeln!(out, "                match r {{")?;
                writeln!(out, "                    Ok(val) => {{")?;
                match op.encoding {
                    syntax::Encoding::Zerocopy => {
                        writeln!(out, "                        userlib::sys_reply(rm.sender, 0, zerocopy::AsBytes::as_bytes(&val));")?;
                    }
                    syntax::Encoding::Ssmarshal => {
                        writeln!(out, "                        let mut reply_buf = [0u8; {}_REPLY_SIZE];",
                            opname.to_uppercase())?;
                        writeln!(out, "                        let n_reply = ssmarshal::serialize(&mut reply_buf, &val).map_err(|_| ()).unwrap();")?;
                        writeln!(out, "                        userlib::sys_reply(rm.sender, 0, &reply_buf[..n_reply]);")?;
                    }
                }
                writeln!(out, "                        Ok(())")?;
                writeln!(out, "                    }}")?;
                writeln!(out, "                    Err(val) => {{")?;
                // Simple returns can only return ClientError.
                writeln!(out, "                        Err(val.into())")?;
                writeln!(out, "                    }}")?;
                writeln!(out, "                }}")?;
            }
            syntax::Reply::Result { err, .. } => {
                writeln!(out, "                match r {{")?;
                writeln!(out, "                    Ok(val) => {{")?;
                match op.encoding {
                    syntax::Encoding::Zerocopy => {
                        writeln!(out, "                        userlib::sys_reply(rm.sender, 0, zerocopy::AsBytes::as_bytes(&val));")?;
                    }
                    syntax::Encoding::Ssmarshal => {
                        writeln!(out, "                        let mut reply_buf = [0u8; {}_REPLY_SIZE];",
                            opname.to_uppercase())?;
                        writeln!(out, "                        let n_reply = ssmarshal::serialize(&mut reply_buf, &val).map_err(|_| ()).unwrap();")?;
                        writeln!(out, "                        userlib::sys_reply(rm.sender, 0, &reply_buf[..n_reply]);")?;
                    }
                }
                writeln!(out, "                        Ok(())")?;
                writeln!(out, "                    }}")?;
                writeln!(out, "                    Err(val) => {{")?;
                match err {
                    // Note: for non-CLike errors we'd need to do an actual
                    // reply here and then return Ok(()) to avoid invoking the
                    // simple "return an integer" error path.
                    syntax::Error::CLike(_) => {
                        writeln!(
                            out,
                            "                        Err(val.map_runtime(u16::from))"
                        )?;
                    }
                }
                writeln!(out, "                    }}")?;
                writeln!(out, "                }}")?;
            }
        }
        writeln!(out, "            }}")?;
    }
    writeln!(out, "        }}")?;
    writeln!(out, "    }}")?;

    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}

fn generate_server_section(
    iface: &syntax::Interface,
    text: &str,
    mut out: impl Write,
) -> Result<(), Box<dyn std::error::Error>> {
    let bytes = text.as_bytes();

    write!(
        out,
        r##"
// To allow it to be pulled out by debuggers, we drop the entirety of the
// interface definition in a dedicated (unloaded) section
#[used]
#[link_section = ".idolatry"]
static _{}_IDOL_DEFINITION: [u8; {}] = ["##,
        iface.name.to_uppercase(),
        text.len()
    )?;

    for i in 0..bytes.len() {
        let delim = if i % 10 == 0 { "\n    " } else { " " };
        write!(out, "{}0x{:02x},", delim, bytes[i])?;
    }

    writeln!(out, "\n];\n")?;

    Ok(())
}

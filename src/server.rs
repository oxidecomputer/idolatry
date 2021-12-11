// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::{common, syntax};
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

pub fn build_server_support(
    source: &str,
    stub_name: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let out = &PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let mut stub_file = File::create(out.join(stub_name)).unwrap();

    let text = std::fs::read_to_string(source)?;
    let iface: syntax::Interface = ron::de::from_str(&text)?;

    generate_server_constants(&iface, &mut stub_file)?;
    generate_server_conversions(&iface, &mut stub_file)?;
    generate_server_in_order_trait(&iface, &mut stub_file)?;
    generate_server_pipelined_trait(&iface, &mut stub_file)?;
    common::generate_op_enum(&iface, &mut stub_file)?;
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

        for arg in op.args.values() {
            writeln!(out, "    + core::mem::size_of::<{}>()", arg.ty.0)?;
        }
        writeln!(out, "    ;")?;

        write!(out, "pub const {}_REPLY_SIZE: usize =", upper_name)?;
        match &op.reply {
            syntax::Reply::Result { ok, .. } => {
                // This strategy only uses bytes for the OK side of the type,
                // and only sends one type, so:
                writeln!(out, "core::mem::size_of::<{}>();", ok.0)?;
            }
        }

        upper_names.push(upper_name);
    }

    writeln!(out, "pub const INCOMING_SIZE: usize = 0")?;
    for un in upper_names {
        writeln!(out, "    + {}_MSG_SIZE", un)?;
    }
    writeln!(out, "    ;")?;
    Ok(())
}

pub fn generate_server_conversions(
    iface: &syntax::Interface,
    mut out: impl Write,
) -> Result<(), Box<dyn std::error::Error>> {
    for (name, op) in &iface.ops {
        // Define args struct.
        writeln!(out, "#[allow(non_camel_case_types)]")?;
        writeln!(out, "#[repr(C, packed)]")?;
        writeln!(
            out,
            "#[derive(Copy, Clone, zerocopy::FromBytes, zerocopy::Unaligned)]"
        )?;
        writeln!(out, "pub struct {}_{}_ARGS {{", iface.name, name)?;
        let mut need_args_impl = false;
        for (argname, arg) in &op.args {
            match &arg.recv {
                syntax::ArgRecvStrategy::FromBytes => {
                    writeln!(out, "    pub {}: {},", argname, arg.ty.0)?;
                }
                syntax::ArgRecvStrategy::FromPrimitive(ty) => {
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
                    syntax::ArgRecvStrategy::FromPrimitive(ty) => {
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

    Ok(())
}

pub fn generate_server_in_order_trait(
    iface: &syntax::Interface,
    mut out: impl Write,
) -> Result<(), Box<dyn std::error::Error>> {
    writeln!(out, "pub trait NotificationHandler {{")?;
    writeln!(out, "    fn current_notification_mask(&self) -> u32;")?;
    writeln!(out)?;
    writeln!(out, "    fn handle_notification(&mut self, bits: u32);")?;
    writeln!(out)?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(out, "pub trait InOrder{}Server {{", iface.name)?;
    writeln!(
        out,
        "    fn current_recv_source(&self) -> Option<userlib::TaskId> {{"
    )?;
    writeln!(out, "        None")?;
    writeln!(out, "    }}")?;
    writeln!(out)?;
    writeln!(out, "    fn closed_recv_fail(&mut self) {{}}")?;
    writeln!(out)?;
    for (name, op) in &iface.ops {
        writeln!(out, "    fn {}(", name)?;
        writeln!(out, "        &mut self,")?;
        writeln!(out, "        msg: &userlib::RecvMessage,")?;
        for (argname, arg) in &op.args {
            writeln!(out, "        {}: {},", argname, arg.ty.0)?;
        }
        write!(out, ")")?;

        match &op.reply {
            syntax::Reply::Result { ok, err } => {
                write!(out, " -> Result<{}, ", ok.0)?;
                match err {
                    syntax::Error::CLike(ty) => {
                        write!(out, "{}", ty.0)?;
                    }
                }
                write!(out, ">")?;
            }
        }
        writeln!(out, ";")?;
        writeln!(out)?;
    }
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(out, "fn in_order_dispatch_core(")?;
    writeln!(out, "    rm: &userlib::RecvMessage,")?;
    writeln!(out, "    incoming: &[u8],")?;
    writeln!(out, "    server: &mut impl InOrder{}Server,", iface.name)?;
    writeln!(out, ") -> Result<(), u32> {{")?;
    writeln!(out, "    let op = <{}Operation as userlib::FromPrimitive>::from_u32(rm.operation).ok_or(1u32)?;", iface.name)?;
    writeln!(out, "    match op {{")?;
    for (opname, op) in &iface.ops {
        writeln!(out, "        {}Operation::{} => {{", iface.name, opname)?;
        writeln!(
            out,
            "            if rm.response_capacity < {}_REPLY_SIZE {{",
            opname.to_uppercase()
        )?;
        writeln!(out, "                return Err(1);")?;
        writeln!(out, "            }}")?;
        writeln!(
            out,
            "            let {}args = read_{}_msg(incoming).ok_or(2u32)?;",
            if op.args.is_empty() { "_" } else { "" },
            opname
        )?;
        writeln!(out, "            let r = server.{}(", opname)?;
        writeln!(out, "                rm,")?;
        for (argname, arg) in &op.args {
            match &arg.recv {
                syntax::ArgRecvStrategy::FromBytes => {
                    writeln!(out, "                args.{},", argname)?;
                }
                syntax::ArgRecvStrategy::FromPrimitive(_) => {
                    writeln!(
                        out,
                        "                args.{}().ok_or(2u32)?,",
                        argname
                    )?;
                }
            }
        }
        writeln!(out, "            );")?;
        match &op.reply {
            syntax::Reply::Result { err, .. } => {
                writeln!(out, "            match r {{")?;
                writeln!(out, "                Ok(val) => {{")?;
                writeln!(out, "                    userlib::sys_reply(rm.sender, 0, zerocopy::AsBytes::as_bytes(&val));")?;
                writeln!(out, "                    Ok(())")?;
                writeln!(out, "                }}")?;
                writeln!(out, "                Err(val) => {{")?;
                match err {
                    // Note: for non-CLike errors we'd need to do an actual
                    // reply here and then return Ok(()) to avoid invoking the
                    // simple "return an integer" error path.
                    syntax::Error::CLike(_) => {
                        writeln!(out, "                    Err(val.into())")?;
                    }
                }
                writeln!(out, "                }}")?;
                writeln!(out, "            }}")?;
            }
        }
        writeln!(out, "        }}")?;
    }
    writeln!(out, "    }}")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(out, "#[allow(dead_code)]")?;
    writeln!(out, "pub fn in_order_dispatch(")?;
    writeln!(out, "    buffer: &mut [u8; INCOMING_SIZE],")?;
    writeln!(out, "    server: &mut impl InOrder{}Server,", iface.name)?;
    writeln!(out, ") {{")?;
    writeln!(
        out,
        "    let rm = match userlib::sys_recv(buffer, 0, server.current_recv_source()) {{"
    )?;
    writeln!(out, "        Ok(rm) => rm,")?;
    writeln!(out, "        Err(_) => {{")?;
    writeln!(out, "            server.closed_recv_fail();")?;
    writeln!(out, "            return;")?;
    writeln!(out, "        }}")?;
    writeln!(out, "    }};")?;

    writeln!(out, "    let incoming = &buffer[..rm.message_len];")?;
    writeln!(
        out,
        "    match in_order_dispatch_core(&rm, incoming, server) {{"
    )?;
    writeln!(out, "        Ok(()) => (),")?;
    writeln!(
        out,
        "        Err(rc) => userlib::sys_reply(rm.sender, rc, &[]),"
    )?;
    writeln!(out, "    }}")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(out, "#[allow(dead_code)]")?;
    writeln!(out, "pub fn in_order_dispatch_n(")?;
    writeln!(out, "    buffer: &mut [u8; INCOMING_SIZE],")?;
    writeln!(
        out,
        "    server: &mut (impl InOrder{}Server + NotificationHandler),",
        iface.name
    )?;
    writeln!(out, ") {{")?;
    writeln!(out, "    let mask = server.current_notification_mask();")?;
    writeln!(
        out,
        "    let rm = match userlib::sys_recv(buffer, mask, server.current_recv_source()) {{"
    )?;
    writeln!(out, "        Ok(rm) => rm,")?;
    writeln!(out, "        Err(_) => {{")?;
    writeln!(out, "            server.closed_recv_fail();")?;
    writeln!(out, "            return;")?;
    writeln!(out, "        }}")?;
    writeln!(out, "    }};")?;

    writeln!(out, "    if rm.sender == userlib::TaskId::KERNEL {{")?;
    writeln!(out, "        server.handle_notification(rm.operation);")?;
    writeln!(out, "    }} else {{")?;

    writeln!(out, "        let incoming = &buffer[..rm.message_len];")?;
    writeln!(
        out,
        "        match in_order_dispatch_core(&rm, incoming, server) {{"
    )?;
    writeln!(out, "            Ok(()) => (),")?;
    writeln!(
        out,
        "            Err(rc) => userlib::sys_reply(rm.sender, rc, &[]),"
    )?;
    writeln!(out, "        }}")?;
    writeln!(out, "    }}")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}

pub fn generate_server_pipelined_trait(
    iface: &syntax::Interface,
    mut out: impl Write,
) -> Result<(), Box<dyn std::error::Error>> {
    writeln!(out, "pub trait Pipelined{}Server {{", iface.name)?;
    for (name, op) in &iface.ops {
        writeln!(out, "    fn {}(", name)?;
        writeln!(out, "        &mut self,")?;
        writeln!(out, "        sender: userlib::TaskId,")?;
        for (argname, arg) in &op.args {
            writeln!(out, "        {}: {},", argname, arg.ty.0)?;
        }
        write!(out, ");")?;
    }
    writeln!(out, "}}")?;

    writeln!(out, "fn pipelined_dispatch_core(")?;
    writeln!(out, "    sender: userlib::TaskId,")?;
    writeln!(out, "    operation: u32,")?;
    writeln!(out, "    incoming: &[u8],")?;
    writeln!(out, "    reply_capacity: usize,")?;
    writeln!(out, "    server: &mut impl Pipelined{}Server,", iface.name)?;
    writeln!(out, ") -> Result<(), u32> {{")?;
    writeln!(
        out,
        "    let op = <{}Operation as userlib::FromPrimitive>::from_u32(operation).ok_or(1u32)?;",
        iface.name
    )?;
    writeln!(out, "    match op {{")?;
    for (opname, op) in &iface.ops {
        writeln!(out, "        {}Operation::{} => {{", iface.name, opname)?;
        writeln!(
            out,
            "            if reply_capacity < {}_REPLY_SIZE {{",
            opname.to_uppercase()
        )?;
        writeln!(out, "                return Err(1);")?;
        writeln!(out, "            }}")?;
        writeln!(
            out,
            "            let {}args = read_{}_msg(incoming).ok_or(2u32)?;",
            if op.args.is_empty() { "_" } else { "" },
            opname
        )?;

        writeln!(out, "            server.{}(", opname)?;
        writeln!(out, "                sender,")?;
        for (argname, arg) in &op.args {
            match &arg.recv {
                syntax::ArgRecvStrategy::FromBytes => {
                    writeln!(out, "                args.{},", argname)?;
                }
                syntax::ArgRecvStrategy::FromPrimitive(_) => {
                    writeln!(
                        out,
                        "                args.{}().ok_or(2u32)?,",
                        argname
                    )?;
                }
            }
        }
        writeln!(out, "            );")?;
        writeln!(out, "        }}")?;
    }
    writeln!(out, "    }}")?;
    writeln!(out, "    Ok(())")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    writeln!(out, "#[allow(dead_code)]")?;
    writeln!(out, "pub fn pipelined_dispatch(")?;
    writeln!(out, "    buffer: &mut [u8; INCOMING_SIZE],")?;
    writeln!(out, "    server: &mut impl Pipelined{}Server,", iface.name)?;
    writeln!(out, ") {{")?;
    writeln!(out, "    let rm = userlib::sys_recv_open(buffer, 0);")?;
    writeln!(out, "    let incoming = &buffer[..rm.message_len];")?;
    writeln!(out, "    match pipelined_dispatch_core(rm.sender, rm.operation, incoming, rm.response_capacity, server) {{")?;
    writeln!(out, "        Ok(()) => (),")?;
    writeln!(
        out,
        "        Err(rc) => userlib::sys_reply(rm.sender, rc, &[]),"
    )?;
    writeln!(out, "    }}")?;
    writeln!(out, "}}")?;
    Ok(())
}

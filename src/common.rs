// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::syntax;

pub fn generate_op_enum(
    iface: &syntax::Interface,
    mut out: impl std::io::Write,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    writeln!(out, "#[allow(non_camel_case_types)]")?;
    writeln!(
        out,
        "#[derive(Copy, Clone, Debug, Eq, PartialEq, userlib::FromPrimitive)]"
    )?;
    writeln!(out, "pub enum {}Operation {{", iface.name)?;
    for (idx, name) in iface.ops.keys().enumerate() {
        writeln!(out, "    {} = {},", name, idx + 1)?;
    }
    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(())
}

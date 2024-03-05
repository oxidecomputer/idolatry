// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::io::Read;

fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let mut text = String::new();
    std::io::stdin().read_to_string(&mut text)?;

    let iface: idol::syntax::Interface = ron::de::from_str(&text)?;

    idol::Generator::default()
        .generate_client_stub(&iface, std::io::stdout())?;

    Ok(())
}

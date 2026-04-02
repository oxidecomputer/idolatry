// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::io::Read;
use std::str::FromStr;

fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let mut text = String::new();
    std::io::stdin().read_to_string(&mut text)?;

    let raw = idol::syntax::RawInterface::from_str(&text)?;
    let iface = raw.resolve(None)?;

    idol::Generator::new()
        .with_counters(idol::CounterSettings::default())
        .generate_client_stub(&iface, std::io::stdout())?;

    Ok(())
}

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::io::Read;

fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let mut text = String::new();
    std::io::stdin().read_to_string(&mut text)?;

    let iface: idol::syntax::Interface = ron::de::from_str(&text)?;
    let tokens = idol::Generator::new()
        .with_counters(
            idol::CounterSettings::default().combine_client_errors(true),
        )
        .generate_restricted_server_support(
            &iface,
            idol::server::ServerStyle::InOrder,
            &Default::default(),
        )?;
    let syntax_tree = syn::parse2::<syn::File>(tokens)?;
    let formatted = prettyplease::unparse(&syntax_tree);
    println!("{formatted}");

    Ok(())
}

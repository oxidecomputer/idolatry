use std::io::Read;
use std::str::FromStr;

fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let mut text = String::new();
    std::io::stdin().read_to_string(&mut text)?;

    let raw = idol::syntax::RawInterface::from_str(&text)?;
    let iface = raw.resolve(None)?;
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

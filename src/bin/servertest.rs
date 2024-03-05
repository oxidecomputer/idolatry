use miette::IntoDiagnostic;
use std::io::Read;

fn main() -> Result<(), miette::Report> {
    let mut text = String::new();
    std::io::stdin()
        .read_to_string(&mut text)
        .into_diagnostic()?;

    let iface: idol::syntax::Interface = text.parse()?;

    let tokens = idol::server::generate_restricted_server_support(
        &iface,
        idol::server::ServerStyle::InOrder,
        &Default::default(),
    )?;
    let formatted = idol::common::fmt_tokens(tokens)?;
    println!("{formatted}");

    Ok(())
}

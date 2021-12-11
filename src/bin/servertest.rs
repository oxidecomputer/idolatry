use std::io::Read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut text = String::new();
    std::io::stdin().read_to_string(&mut text)?;

    let iface: idol::syntax::Interface = ron::de::from_str(&text)?;

    idol::server::generate_server_constants(&iface, std::io::stdout())?;
    idol::server::generate_server_conversions(&iface, std::io::stdout())?;

    Ok(())
}

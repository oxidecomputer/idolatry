use std::io::Read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut text = String::new();
    std::io::stdin().read_to_string(&mut text)?;

    let iface: idol::syntax::Interface = ron::de::from_str(&text)?;
    let mut out = std::io::stdout();

    idol::server::generate_server_constants(&iface, &mut out)?;
    idol::server::generate_server_conversions(&iface, &mut out)?;
    idol::common::generate_op_enum(&iface, &mut out)?;
    idol::server::generate_server_op_impl(&iface, &mut out)?;

    idol::server::generate_server_in_order_trait(
        &iface,
        &mut out,
        &Default::default(),
    )?;

    Ok(())
}

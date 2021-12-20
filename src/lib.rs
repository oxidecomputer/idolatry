// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod client;
pub mod common;
pub mod server;
pub mod syntax;

#[cfg(test)]
mod test {
    const EXAMPLE: &str = r#"
        Interface(
            name: "Spi",
            ops: {
                "exchange": (
                    args: {
                        "device_index": (type: "u8"),
                    },
                    leases: {
                        "source": (type: "[u8]", read: true),
                        "sink": (type: "[u8]", write: true),
                    },
                    reply: Result(
                        ok: "()",
                        err: CLike("SpiError"),
                    ),
                ),
                "lock": (
                    args: {
                        "device_index": (type: "u8"),
                    },
                    reply: Result(
                        ok: "()",
                        err: CLike("SpiError"),
                    ),
                ),
            },
        )"#;

    #[test]
    fn parse_example() {
        let _iface = super::syntax::Interface::from_str(EXAMPLE)
            .expect("example failed to parse");
    }
}

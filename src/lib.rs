// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod client;
pub mod common;
pub(crate) mod serde_helpers;
pub mod server;
pub mod syntax;

#[derive(Clone, Debug, PartialEq, Eq, Default)]
#[must_use]
pub struct GeneratorSettings {
    pub(crate) counters: bool,
}

impl GeneratorSettings {
    pub fn new() -> Self {
        Self::default()
    }

    /// Generate event counters for each IPC operation.
    pub fn with_counters(self, counters: bool) -> Self {
        Self { counters, ..self }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::str::FromStr;

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
        let _iface = syntax::Interface::from_str(EXAMPLE)
            .expect("example failed to parse");
    }
}

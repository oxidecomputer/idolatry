// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod client;
pub mod common;
mod counters;
pub(crate) mod serde_helpers;
pub mod server;
pub mod syntax;
pub use crate::counters::CounterSettings;

#[derive(Clone, Debug, PartialEq, Eq)]
#[must_use]
pub struct Generator {
    pub(crate) counters: CounterSettings,
    pub(crate) fmt: bool,
    pub(crate) extra_op_enum_derives: Vec<syn::Path>,
    pub(crate) error_server: GenerateErrorServer,
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
enum GenerateErrorServer {
    /// Do not generate an error server under any circumstances
    No,
    /// Generate an error server if all functions have the same error type
    #[default]
    Maybe,
    /// Generate an error server; fail if functions have different error types
    Yes,
}

impl Generator {
    pub fn new() -> Self {
        Self {
            counters: Default::default(),
            extra_op_enum_derives: Vec::new(),
            fmt: true,
            error_server: Default::default(),
        }
    }

    /// Overrides how how event counters will be generated for IPC
    /// operations.
    ///
    /// By default, counters are not enabled.
    ///
    /// # Examples
    ///
    /// Enabling counters with the default settings:
    ///
    /// ```
    /// # let _ =
    /// idol::Generator::new()
    ///     .with_counters(idol::CounterSettings::default())
    /// # ;
    /// ```
    ///
    ///
    /// Disabling counters:
    ///
    /// ```
    /// let counters = idol::CounterSettings::default()
    ///     .with_client_counters(false)
    ///     .with_server_counters(false);
    /// # let _ =
    /// idol::Generator::new()
    ///     .with_counters(counters)
    /// # ;
    /// ```
    ///
    /// Configuring counter generation:
    ///
    /// ```
    /// let counters = idol::CounterSettings::default()
    ///     .combine_client_errors(true);
    ///
    /// # let _ =
    /// idol::Generator::new()
    ///     .with_counters(counters)
    /// # ;
    /// ```
    pub fn with_counters(self, counters: CounterSettings) -> Self {
        Self { counters, ..self }
    }

    /// If `true`, generated code will be formatted with `prettyplease`.
    pub fn with_fmt(self, fmt: bool) -> Self {
        Self { fmt, ..self }
    }

    /// Disables error server generation
    pub fn without_error_server(self) -> Self {
        Self {
            error_server: GenerateErrorServer::No,
            ..self
        }
    }

    /// Enables mandatory error server generation
    pub fn with_error_server(self) -> Self {
        Self {
            error_server: GenerateErrorServer::Yes,
            ..self
        }
    }

    /// Add additional traits to derive for the generated operation enum.
    ///
    /// # Examples
    ///
    /// ```
    /// # let _ =
    /// idol::Generator::new().with_op_enum_derives(&[
    ///     "serde::Serialize",
    ///    "serde::Deserialize",
    /// ])
    /// # ;
    /// ```
    pub fn with_op_enum_derives<T>(
        self,
        derives: impl IntoIterator<Item = T>,
    ) -> Result<Self, syn::Error>
    where
        T: AsRef<str>,
    {
        Ok(Self {
            extra_op_enum_derives: derives
                .into_iter()
                .map(|s| syn::parse_str(s.as_ref()))
                .collect::<Result<_, _>>()?,
            ..self
        })
    }
}

impl Default for Generator {
    fn default() -> Self {
        Self::new()
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

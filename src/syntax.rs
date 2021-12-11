// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Specifies the syntax of interface definitions using Rust types and serde.
//!
//! This is intended to serve as a placeholder method until we write a proper
//! parser, but then, McCarthy said the same thing about s-expressions.

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

/// Definition of an IPC interface.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Interface {
    /// Name of interface. This will be used in generated types, and should
    /// match Rust type name conventions.
    pub name: String,
    /// Operations supported by the interface. The names of the operations
    /// should be Rust identifiers, and will be used in generated function
    /// names.
    ///
    /// This is an `IndexMap`, and the order of declaration of the operations is
    /// significant -- it determines the operation numbering.
    pub ops: IndexMap<String, Operation>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Operation {
    /// Arguments of the operation that are passed by-value in the kernel-copied
    /// message. If omitted, zero arguments are assumed.
    ///
    /// The order of arguments is significant, it determines the packing order.
    /// Because this means that ergonomics changes to the API can affect runtime
    /// performance, we may want a way to override this eventually (TODO).
    #[serde(default)]
    pub args: IndexMap<String, Arg>,
    /// Arguments of the operation that are converted into leases. If omitted,
    /// zero leases are assumed.
    ///
    /// The order of leases is significant, as it determines their numerical
    /// index from the server's perspective.
    #[serde(default)]
    pub leases: IndexMap<String, Lease>,
    /// Expected type of the response.
    pub reply: Reply,

    /// When `true`, signals that clients should automatically retry this
    /// operation if the server crashes. When `false`, the dead-codes produced
    /// by a crash need to be mapped into the result type.
    #[serde(default)]
    pub idempotent: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Arg {
    /// Type being sent.
    #[serde(rename = "type")]
    pub ty: Ty,
    /// Deserialization strategy to be used on RECV.
    #[serde(default)]
    pub recv: ArgRecvStrategy,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Lease {
    /// Type being leased.
    #[serde(rename = "type")]
    pub ty: Ty,
    /// The server will be able to read from this lease. The type being leased
    /// must implement `zerocopy::AsBytes`.
    #[serde(default)]
    pub read: bool,
    /// The server will be able to write to this lease. The type being leased
    /// must implement both `zerocopy::AsBytes` and `zerocopy::FromBytes`.
    #[serde(default)]
    pub write: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Reply {
    /// The operation may fail with an error type. This method assumes that
    /// success is indicated by rc=0, and all other values are errors.
    Result {
        /// On success (rc=0), the reply buffer will be interpreted as this
        /// type.
        ok: Ty,
        /// On failure (rc != 0), the given strategy will kick in.
        err: Error,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Ty(pub String);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Error {
    /// The error type should be created from the (non-zero) return code only.
    /// The reply message in error cases is expected to be zero-length.
    ///
    /// The error type here may or may not be required to also represent dead
    /// codes, depending on whether the operation is `idempotent`.
    CLike(Ty),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ArgRecvStrategy {
    FromBytes,
    FromPrimitive(Ty),
}

impl Default for ArgRecvStrategy {
    fn default() -> Self {
        Self::FromBytes
    }
}

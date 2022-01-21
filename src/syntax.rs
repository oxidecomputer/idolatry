// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Specifies the syntax of interface definitions using Rust types and serde.
//!
//! This is intended to serve as a placeholder method until we write a proper
//! parser, but then, McCarthy said the same thing about s-expressions.

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::num::NonZeroU32;

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

impl Interface {
    /// Converts the canonical text representation of an interface into an
    /// `Interface`.
    ///
    /// The canonical text representation is the Serde representation of
    /// `Interface` as encoded by RON.
    pub fn from_str(text: &str) -> Result<Self, ron::Error> {
        let iface: Self = ron::de::from_str(text)?;
        Ok(iface)
    }
}

/// Definition of an operation within an `Interface`.
///
/// Each interface has zero or more operations; operations are assigned
/// distinguishing numbers (discriminators) starting from 1 (for historical
/// reasons).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Operation {
    /// Arguments of the operation that are passed by-value in the kernel-copied
    /// message. If omitted, zero arguments are assumed.
    ///
    /// The order of arguments is significant, it determines the packing order.
    /// Because this means that ergonomics changes to the API can affect runtime
    /// performance, we may want a way to override this eventually (TODO).
    #[serde(default)]
    pub args: IndexMap<String, AttributedTy>,
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

    /// How to encode arguments and return types. The default is `Zerocopy`.
    #[serde(default)]
    pub encoding: Encoding,
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum Encoding {
    /// Try to pass types by direct memory image where possible. This is
    /// cheapest but won't work for complex types.
    Zerocopy,
    /// Encode types using the `ssmarshal` codec for `serde`. This can transfer
    /// arbitrarily complex Rust types.
    Ssmarshal,
}

impl Default for Encoding {
    fn default() -> Self {
        Self::Zerocopy
    }
}

/// Description of a lease expected by an operation.
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
    /// The server cannot accept leases longer than this.
    ///
    /// This is only meaningful if `ty` is unsized, which at the moment means a
    /// slice. It is measured in number of elements in the slice, _not_ number
    /// of bytes.
    ///
    /// It's limited to `u32` rather than `usize` because in the current kernel
    /// ABI, individual leases are limited to 4GiB. This also means that, if
    /// this value is not provided, the limit defaults to 4GiB.
    ///
    /// If provided, the value cannot be zero.
    #[serde(default)]
    pub max_len: Option<NonZeroU32>,
}

/// Potential packings of reply types into the Hubris IPC reply format.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Reply {
    /// The operation can't fail, or can only fail through reply-fault, and
    /// always returns this type:
    Simple(AttributedTy),
    /// The operation may fail with an error type. This method assumes that
    /// success is indicated by rc=0, and all other values are errors.
    Result {
        /// On success (rc=0), the reply buffer will be interpreted as this
        /// type.
        ok: AttributedTy,
        /// On failure (rc != 0), the given strategy will kick in.
        err: Error,
    },
}

/// A type that can also have common attributes applied.
///
/// `AttributedTy` appears in both argument and reply type positions in the
/// interface syntax tree. Its `Deserialize` impl is fancy and allows it to be
/// written either as
///
/// - A raw type name, e.g. `"u8"`, or
/// - A struct with attributes added, e.g. `(type: "u8", foo: bar)`.
///
/// If it's written as a raw type name, the attributes (other fields in this
/// struct) are all defaulted.
#[derive(Debug, Clone, Serialize)]
pub struct AttributedTy {
    /// Name of type.
    #[serde(rename = "type")]
    pub ty: Ty,
    /// How to unpack this type when it is received from another task, either as
    /// an incoming argument, or as a reply.
    #[serde(default)]
    pub recv: RecvStrategy,
}

impl AttributedTy {
    pub fn display(&self) -> &impl std::fmt::Display {
        &self.ty.0
    }

    /// Returns the Rust type that should be used to represent this in the
    /// internal args/reply structs.
    pub fn repr_ty(&self) -> &Ty {
        match &self.recv {
            RecvStrategy::From(t, _) | RecvStrategy::FromPrimitive(t) => t,
            RecvStrategy::FromBytes => &self.ty,
        }
    }
}

/// Visitor for the `Deserialize` impl of `AttributedTy`.
#[derive(Default)]
struct AttributedTyVisitor;

impl<'de> serde::de::Visitor<'de> for AttributedTyVisitor {
    type Value = AttributedTy;

    fn expecting(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        formatter.write_str("type name or attributed type struct")
    }

    fn visit_map<A>(self, mut access: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field {
            Type,
            Recv,
        }

        let mut ty = None;
        let mut recv = None;
        while let Some(key) = access.next_key()? {
            match key {
                Field::Type => {
                    if ty.is_some() {
                        return Err(serde::de::Error::duplicate_field("type"));
                    }
                    ty = Some(access.next_value()?);
                }
                Field::Recv => {
                    if recv.is_some() {
                        return Err(serde::de::Error::duplicate_field("recv"));
                    }
                    recv = Some(access.next_value()?);
                }
            }
        }
        let ty = ty.ok_or_else(|| serde::de::Error::missing_field("type"))?;
        let recv = recv.unwrap_or_else(RecvStrategy::default);
        Ok(AttributedTy { ty, recv })
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(AttributedTy {
            ty: Ty(v.to_string()),
            recv: RecvStrategy::default(),
        })
    }
}

impl<'de> Deserialize<'de> for AttributedTy {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        deserializer.deserialize_any(AttributedTyVisitor::default())
    }
}

/// A type name.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Ty(pub String);

impl Ty {
    /// Checks whether the type name looks like it might be unsized.
    ///
    /// We use this to choose lease validation strategies.
    pub fn appears_unsized(&self) -> bool {
        // This is a hack. Need to work out a better way to determine this.
        self.0.starts_with('[') && self.0.ends_with(']')
    }
}

/// Enumerates different ways that an error type might be passed through the
/// REPLY syscall.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Error {
    /// The error type should be created from the (non-zero) return code only.
    /// The reply message in error cases is expected to be zero-length.
    ///
    /// The error type here may or may not be required to also represent dead
    /// codes, depending on whether the operation is `idempotent`.
    CLike(Ty),
}

/// Enumerates different ways that a type might be unpacked when received over
/// an IPC interface from another task.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RecvStrategy {
    /// The received bytes should be directly reinterpreted as the type using
    /// `zerocopy::FromBytes`.
    FromBytes,
    /// The received bytes should be the named type, which will then be
    /// converted into the target type using `num_traits::FromPrimitive` (which
    /// is also re-exported by Hubris `userlib`).
    FromPrimitive(Ty),
    /// The received bytes should be the named type, which will then be
    /// converted into the target type.
    ///
    /// If the second field is `None`, it specifies conversion by `From`.
    ///
    /// If the second field is `Some(fn_name)`, it specifies conversion by
    /// `fn_name`.
    From(Ty, #[serde(default)] Option<String>),
}

impl Default for RecvStrategy {
    fn default() -> Self {
        Self::FromBytes
    }
}

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Specifies the syntax of interface definitions using Rust types and serde.
//!
//! This is intended to serve as a placeholder method until we write a proper
//! parser, but then, McCarthy said the same thing about s-expressions.

use indexmap::IndexMap;
use once_cell::unsync::OnceCell;
use quote::TokenStreamExt;
use serde::{Deserialize, Serialize};
use serde_with::{DeserializeFromStr, SerializeDisplay};
use std::num::NonZeroU32;

/// An identifier.
#[derive(Debug, SerializeDisplay, DeserializeFromStr)]
pub struct Name {
    pub ident: syn::Ident,
    /// Necessary to stop syn from rendering the underlying string repr
    /// inaccessible, used for map lookups.
    as_string: String,
    // A bunch of code generation uses input identifiers to construct other
    // identifiers, such as prefixing or uppercasing, multiple times for the
    // same identifier. Thus, we cache these to reduce the number of
    // strings we allocate a bunch of times during codegen.
    //
    // This is, admittedly, a kind of goofy microoptimization that probably
    // doesn't matter that much.
    /// Cached uppercase version of the identifier, used for generating constants.
    uppercase: String,
    /// Cached version of the identifier with a `arg_` prefix, used for
    /// generating argument names in generated code.
    arg_name: OnceCell<syn::Ident>,
    /// Cached version of the identifier as a `{uppercase}_REPLY_SIZE` constant.
    reply_size: OnceCell<syn::Ident>,
    /// Cached version of the identifier with a `raw_` prefix, used for
    /// generating arguments which perform conversions.
    raw_argname: OnceCell<syn::Ident>,
    /// Cached version of the identifier as `{name}Operation`.
    op_enum: OnceCell<syn::Ident>,
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.ident.fmt(f)
    }
}

impl std::str::FromStr for Name {
    type Err = syn::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ident = syn::parse_str(s)?;
        let uppercase = s.to_uppercase();
        Ok(Self {
            as_string: s.to_string(),
            ident,
            arg_name: OnceCell::new(),
            reply_size: OnceCell::new(),
            raw_argname: OnceCell::new(),
            op_enum: OnceCell::new(),
            uppercase,
        })
    }
}

impl quote::ToTokens for Name {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.ident.to_tokens(tokens)
    }
}

impl quote::IdentFragment for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        quote::IdentFragment::fmt(&self.ident, f)
    }

    fn span(&self) -> Option<proc_macro2::Span> {
        quote::IdentFragment::span(&self.ident)
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
    }
}

impl Eq for Name {}

impl Ord for Name {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.ident.cmp(&other.ident)
    }
}

impl PartialOrd for Name {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::hash::Hash for Name {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ident.hash(state);
    }
}

impl Clone for Name {
    fn clone(&self) -> Self {
        Self {
            as_string: self.as_string.clone(),
            ident: self.ident.clone(),
            uppercase: self.uppercase.clone(),
            arg_name: OnceCell::new(),
            reply_size: OnceCell::new(),
            raw_argname: OnceCell::new(),
            op_enum: OnceCell::new(),
        }
    }
}

impl Name {
    pub(crate) fn as_str(&self) -> &str {
        &self.as_string
    }

    pub(crate) fn uppercase(&self) -> &str {
        &self.uppercase
    }

    pub(crate) fn arg_prefixed(&self) -> &syn::Ident {
        self.arg_name
            .get_or_init(|| quote::format_ident!("arg_{}", self))
    }

    pub(crate) fn as_reply_size(&self) -> &syn::Ident {
        self.reply_size.get_or_init(|| {
            quote::format_ident!("{}_REPLY_SIZE", self.uppercase)
        })
    }

    pub(crate) fn raw_prefixed(&self) -> &syn::Ident {
        self.raw_argname
            .get_or_init(|| quote::format_ident!("raw_{self}"))
    }

    pub(crate) fn as_op_enum(&self) -> &syn::Ident {
        self.op_enum
            .get_or_init(|| quote::format_ident!("{self}Operation"))
    }
}

impl std::borrow::Borrow<str> for Name {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

/// Definition of an IPC interface.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Interface {
    /// Name of interface. This will be used in generated types, and should
    /// match Rust type name conventions.
    pub name: Name,
    /// Operations supported by the interface. The names of the operations
    /// should be Rust identifiers, and will be used in generated function
    /// names.
    ///
    /// This is an `IndexMap`, and the order of declaration of the operations is
    /// significant -- it determines the operation numbering.
    #[serde(
        deserialize_with = "crate::serde_helpers::deserialize_reject_dup_keys"
    )]
    pub ops: IndexMap<Name, Operation>,
}

impl std::str::FromStr for Interface {
    type Err = ron::Error;
    /// Converts the canonical text representation of an interface into an
    /// `Interface`.
    ///
    /// The canonical text representation is the Serde representation of
    /// `Interface` as encoded by RON.
    fn from_str(text: &str) -> Result<Self, ron::Error> {
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
    #[serde(
        default,
        deserialize_with = "crate::serde_helpers::deserialize_reject_dup_keys"
    )]
    pub args: IndexMap<Name, AttributedTy>,
    /// Arguments of the operation that are converted into leases. If omitted,
    /// zero leases are assumed.
    ///
    /// The order of leases is significant, as it determines their numerical
    /// index from the server's perspective.
    #[serde(
        default,
        deserialize_with = "crate::serde_helpers::deserialize_reject_dup_keys"
    )]
    pub leases: IndexMap<Name, Lease>,
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
    /// complex Rust types, but has strict rules about the formation of those
    /// types that are _not_ checked at compile time. Prefer `Hubpack`.
    Ssmarshal,
    /// Encode types using the `hubpack` codec for `serde`. This can transfer
    /// complex Rust types. It has strict rules about the formation of those
    /// types, but all those rules are checked at compile time.
    Hubpack,
}

impl Default for Encoding {
    fn default() -> Self {
        Self::Zerocopy
    }
}

impl Encoding {
    pub fn crate_name(&self) -> syn::Ident {
        match self {
            Self::Zerocopy => {
                syn::Ident::new("zerocopy", proc_macro2::Span::call_site())
            }
            Self::Ssmarshal => {
                syn::Ident::new("ssmarshal", proc_macro2::Span::call_site())
            }
            Self::Hubpack => {
                syn::Ident::new("hubpack", proc_macro2::Span::call_site())
            }
        }
    }
}

/// Description of a lease expected by an operation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Lease {
    /// Type being leased.
    #[serde(rename = "type")]
    pub ty: Ty,
    /// The server will be able to read from this lease. The type being leased
    /// must implement `idol_runtime::zerocopy::AsBytes`.
    #[serde(default)]
    pub read: bool,
    /// The server will be able to write to this lease. The type being leased
    /// must implement both `idol_runtime::zerocopy::AsBytes` and `idol_runtime::zerocopy::FromBytes`.
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

impl quote::ToTokens for Reply {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Simple(AttributedTy { ty, .. }) => ty.to_tokens(tokens),
            Self::Result {
                ok: AttributedTy { ty: ok, .. },
                err,
            } => quote::quote! {
                Result<#ok, #err>
            }
            .to_tokens(tokens),
        }
    }
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
    /// Returns the Rust type that should be used to represent this in the
    /// internal args/reply structs.
    pub fn repr_ty(&self) -> syn::Type {
        match &self.recv {
            RecvStrategy::From(t, _) | RecvStrategy::FromPrimitive(t) => {
                t.0.clone()
            }
            RecvStrategy::FromBytes if self.ty.is_bool() => {
                syn::parse_quote! { u8 }
            }
            RecvStrategy::FromBytes => self.ty.0.clone(),
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
        let ty: Ty =
            ty.ok_or_else(|| serde::de::Error::missing_field("type"))?;
        let recv = recv.unwrap_or_default();
        Ok(AttributedTy { ty, recv })
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        let ty = v.parse::<Ty>().map_err(E::custom)?;
        Ok(AttributedTy {
            ty,
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

impl quote::ToTokens for AttributedTy {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.ty.to_tokens(tokens)
    }
}

/// A type name.
#[derive(
    Clone,
    Debug,
    Eq,
    PartialEq,
    serde_with::SerializeDisplay,
    serde_with::DeserializeFromStr,
)]
pub struct Ty(pub syn::Type);

impl Ty {
    /// Checks whether the type name looks like it might be unsized.
    ///
    /// We use this to choose lease validation strategies.
    pub fn appears_unsized(&self) -> bool {
        matches!(self.0, syn::Type::Slice(_) | syn::Type::TraitObject(_))
    }

    /// Returns `true` if this is a bool.
    pub fn is_bool(&self) -> bool {
        thread_local! {
            static BOOL_TY: std::cell::RefCell<Option<syn::Type>> = const { std::cell::RefCell::new(None) };
        }
        BOOL_TY.with(|ty| {
            let mut ty = ty.borrow_mut();
            let ty = ty.get_or_insert_with(|| syn::parse_quote! { bool });
            self.0 == *ty
        })
    }
}

impl std::fmt::Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        quote::quote! { #self }.fmt(f)
    }
}

impl std::str::FromStr for Ty {
    type Err = syn::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        syn::parse_str(s).map(Self)
    }
}

impl quote::ToTokens for Ty {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.0.to_tokens(tokens)
    }
}

/// Enumerates different ways that an error type might be passed through the
/// REPLY syscall.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Error {
    /// The error type should be created from the (non-zero) return code only.
    /// The reply message in error cases is expected to be zero-length.
    ///
    /// The error type here may or may not be required to also represent dead
    /// codes, depending on whether the operation is `idempotent`.
    CLike(Ty),

    /// A non-zero return code indicates an error, but the specific type of
    /// error is found by deserializing the message payload using the same
    /// encoding as specified for the Ok side.
    ///
    /// The error type here may or may not be required to also represent dead
    /// codes, depending on whether the operation is `idempotent`.
    Complex(Ty),

    /// The client will never return an error, but the function is not
    /// idempotent and will return `Err(ServerDeath {})` if the server died
    /// mid-message.
    ServerDeath,
}

impl quote::ToTokens for Error {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::CLike(ty) | Self::Complex(ty) => ty.to_tokens(tokens),
            Self::ServerDeath => {
                tokens.append(proc_macro2::Ident::new(
                    "idol_runtime::ServerDeath",
                    proc_macro2::Span::call_site(),
                ));
            }
        }
    }
}

/// Enumerates different ways that a type might be unpacked when received over
/// an IPC interface from another task.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RecvStrategy {
    /// The received bytes should be directly reinterpreted as the type using
    /// `idol_runtime::zerocopy::FromBytes`.
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
    From(Ty, #[serde(default)] Option<Name>),
}

impl Default for RecvStrategy {
    fn default() -> Self {
        Self::FromBytes
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn reject_duplicate_ops() {
        const HAS_DUPES: &str = r#"
            Interface(
                name: "Spi",
                ops: {
                    "exchange": (
                        args: {
                            "device_index": (type: "u8"),
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
                    "exchange": (
                        args: {
                            "device_index": (type: "u8"),
                        },
                        reply: Result(
                            ok: "()",
                            err: CLike("SpiError"),
                        ),
                    ),
                },
            )
        "#;

        let err = Interface::from_str(HAS_DUPES).unwrap_err();
        assert!(err
            .to_string()
            .starts_with("invalid entry: found duplicate key"));
    }

    #[test]
    fn reject_duplicate_args() {
        const HAS_DUPES: &str = r#"
            Interface(
                name: "Spi",
                ops: {
                    "exchange": (
                        args: {
                            "foo": (type: "u8"),
                            "bar": (type: "u8"),
                            "foo": (type: "u8"),
                        },
                        reply: Result(
                            ok: "()",
                            err: CLike("SpiError"),
                        ),
                    ),
                },
            )
        "#;

        let err = Interface::from_str(HAS_DUPES).unwrap_err();
        assert!(err
            .to_string()
            .starts_with("invalid entry: found duplicate key"));
    }

    #[test]
    fn reject_duplicate_leases() {
        const HAS_DUPES: &str = r#"
            Interface(
                name: "Spi",
                ops: {
                    "exchange": (
                        args: {
                            "foo": (type: "u8"),
                        },
                        leases: {
                            "source": (type: "[u8]", read: true),
                            "sink": (type: "[u8]", write: true),
                            "source": (type: "[u8]", read: true),
                        },
                        reply: Result(
                            ok: "()",
                            err: CLike("SpiError"),
                        ),
                    ),
                },
            )
        "#;

        let err = Interface::from_str(HAS_DUPES).unwrap_err();
        assert!(err
            .to_string()
            .starts_with("invalid entry: found duplicate key"));
    }
}

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::syntax;
use quote::quote;

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

pub fn generate_op_enum(
    iface: &syntax::Interface,
    settings: &GeneratorSettings,
) -> proc_macro2::TokenStream {
    let variants = iface.ops.keys().enumerate().map(|(idx, name)| {
        // This little dance is unfortunately necessary because `quote` will, by
        // default, generate a literal with the `usize` suffix when
        // interpolating a `usize`. This is *not* what we want here, because we
        // don't generate a `#[repr(usize)]` attribute.
        let val = syn::LitInt::new(
            &(idx + 1).to_string(),
            proc_macro2::Span::call_site(),
        );
        debug_assert_eq!(val.suffix(), "");
        quote! {
            #name = #val,
        }
    });
    let name = iface.name.as_op_enum();
    let maybe_count = if settings.counters {
        quote! {
            #[derive(counters::Count)]
        }
    } else {
        quote! {}
    };
    quote! {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Debug, Eq, PartialEq, userlib::FromPrimitive)]
        #maybe_count
        pub enum #name {
            #(#variants)*
        }
    }
}

pub(crate) fn fmt_tokens(
    tokens: proc_macro2::TokenStream,
) -> Result<String, Box<dyn std::error::Error + Send + Sync>> {
    let syntax_tree = syn::parse2::<syn::File>(tokens)?;
    Ok(prettyplease::unparse(&syntax_tree))
}

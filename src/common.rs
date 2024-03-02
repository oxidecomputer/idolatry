// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::syntax;
use quote::quote;

pub fn generate_op_enum(iface: &syntax::Interface) -> proc_macro2::TokenStream {
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
    quote! {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Debug, Eq, PartialEq, userlib::FromPrimitive)]
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

pub(crate) struct Counters {
    pub(crate) event_enum: syn::Ident,
    pub(crate) enum_def: proc_macro2::TokenStream,
    pub(crate) counters_static: syn::Ident,
}

impl Counters {
    pub(crate) fn new(
        iface: &syntax::Interface,
        counters_static: syn::Ident,
        variants: impl Iterator<Item = proc_macro2::TokenStream>,
    ) -> Self {
        let event_enum = quote::format_ident!("{}Event", iface.name);

        let enum_def = quote! {
            #[derive(counters::Count)]
            #[allow(non_camel_case_types)]
            pub enum #event_enum {
                #(#variants),*
            }
        };

        Self {
            event_enum,
            enum_def,
            counters_static,
        }
    }

    pub(crate) fn generate_defs(&self) -> proc_macro2::TokenStream {
        let Self {
            enum_def,
            event_enum,
            counters_static,
        } = self;
        quote! {
            #enum_def

            #[used]
            static #counters_static: <#event_enum as counters::Count>::Counters =
                <#event_enum as counters::Count>::NEW_COUNTERS;
        }
    }

    pub fn count_simple_op(
        &self,
        op: &syntax::Name,
    ) -> proc_macro2::TokenStream {
        let Self {
            event_enum,
            counters_static,
            ..
        } = self;
        quote! {
            counters::count!(#counters_static, #event_enum::#op);
        }
    }

    pub fn count_result(
        &self,
        op: &syntax::Name,
        result: impl quote::ToTokens,
    ) -> proc_macro2::TokenStream {
        let Self {
            event_enum,
            counters_static,
            ..
        } = self;
        quote! {
            counters::count!(#counters_static, #event_enum::#op(#result));
        }
    }
}

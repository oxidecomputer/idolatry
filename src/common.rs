// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::{syntax, GeneratorSettings};
use quote::quote;

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
    let counters = if settings.counters {
        let enum_name = quote::format_ident!("{}Event", iface.name);
        let counters_name = quote::format_ident!(
            "__{}_OPERATION_COUNTERS",
            iface.name.uppercase()
        );
        let lt = if iface
            .ops
            .values()
            .any(|op| matches!(&op.reply, syntax::Reply::Result { .. }))
        {
            quote! { <'a> }
        } else {
            quote! {}
        };
        let variants = iface.ops.iter().map(|(opname, op)| match &op.reply {
            syntax::Reply::Simple(_) => quote! { #opname },
            syntax::Reply::Result { err, .. } => {
                let err_ty = match err {
                    syntax::Error::CLike(ty) | syntax::Error::Complex(ty) => {
                        quote! { idol_runtime::RequestError<#ty> }
                    }
                    syntax::Error::ServerDeath => {
                        quote! { idol_runtime::RequestError<core::convert::Infallible> }
                    }
                };
                quote! {#opname(#[count(children)] &'a Result<(), #err_ty>) }
            }
        });
        quote! {
            #[derive(counters::Count)]
            #[allow(non_camel_case_types)]
            pub enum #enum_name #lt {
                #(#variants),*
            }

            #[used]
            static #counters_name: <#enum_name as counters::Count>::Counters =
                <#enum_name as counters::Count>::NEW_COUNTERS;
        }
    } else {
        quote! {}
    };
    quote! {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Debug, Eq, PartialEq, userlib::FromPrimitive)]
        pub enum #name {
            #(#variants)*
        }

        #counters
    }
}

pub(crate) fn fmt_tokens(
    tokens: proc_macro2::TokenStream,
) -> Result<String, Box<dyn std::error::Error + Send + Sync>> {
    let syntax_tree = syn::parse2::<syn::File>(tokens)?;
    Ok(prettyplease::unparse(&syntax_tree))
}

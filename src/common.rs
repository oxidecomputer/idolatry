// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::syntax;
use quote::{format_ident, quote};

pub fn generate_op_enum(iface: &syntax::Interface) -> proc_macro2::TokenStream {
    let variants = iface.ops.keys().enumerate().map(|(idx, name)| {
        let val = idx + 1;
        quote! {
            #name = #val,
        }
    });
    let name = format_ident!("{}Operation", iface.name);
    quote! {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Debug, Eq, PartialEq, userlib::FromPrimitive)]
        pub enum #name {
            #(#variants)*
        }
    }
}

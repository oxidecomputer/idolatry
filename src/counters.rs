use crate::syntax;
use proc_macro2::TokenStream;
use quote::quote;

/// Configures how event counters are generated.
#[derive(Clone, Debug, PartialEq, Eq)]
#[must_use]
pub struct CounterSettings {
    pub(crate) combine_client_errors: bool,
    client_counters: bool,
    server_counters: bool,
}

impl Default for CounterSettings {
    fn default() -> Self {
        Self {
            combine_client_errors: false,
            client_counters: !cfg!(feature = "no-counters"),
            server_counters: !cfg!(feature = "no-counters"),
        }
    }
}

impl CounterSettings {
    /// Explicitly enable or disable generation of server-side IPC counters.
    ///
    /// This overrides the default determined based on whether the
    /// "server-counters" feature flag is set.
    pub fn with_server_counters(self, server_counters: bool) -> Self {
        Self {
            server_counters,
            ..self
        }
    }

    /// Explicitly enable or disable generation of server-side IPC counters.
    ///
    /// This overrides the default determined based on whether the
    /// "server-counters" feature flag is set.
    pub fn with_client_counters(self, client_counters: bool) -> Self {
        Self {
            client_counters,
            ..self
        }
    }

    /// If `true`, `idol_runtime::ClientError` variants will be counted
    /// globally for the entire server, rather than separately for each IPC
    /// operation.
    ///
    /// This reduces the memory usage of the counter table by `(28 * (len(IPC
    /// operations) - 1))` bytes: `ClientError` has 7 variants, and each
    /// counter is 4 bytes, so 28 bytes of counters are generated to count
    /// `ClientError`s.
    pub fn combine_client_errors(self, combine_client_errors: bool) -> Self {
        Self {
            combine_client_errors,
            ..self
        }
    }

    pub(crate) fn server(&self, iface: &syntax::Interface) -> Option<Counters> {
        if !self.server_counters {
            return None;
        }

        let mut needs_client_error_variant = false;
        let mut variants = iface.ops.iter().map(|(opname, op)| match &op.reply {
                syntax::Reply::Simple(_) => quote! { #opname },
                syntax::Reply::Result { err, .. } => {
                    let err_ty = match err {
                        syntax::Error::CLike(ty) | syntax::Error::Complex(ty) if self.combine_client_errors => {
                            needs_client_error_variant = true;
                            quote! { #ty }
                        }
                        syntax::Error::CLike(ty) | syntax::Error::Complex(ty) => {
                            quote! { idol_runtime::RequestError<#ty> }
                        }
                        syntax::Error::ServerDeath if self.combine_client_errors => {
                            needs_client_error_variant = true;
                            return quote! {
                                #opname
                            }
                        }
                        syntax::Error::ServerDeath => {
                            quote! { idol_runtime::RequestError<core::convert::Infallible> }
                        }
                    };
                    quote! {
                        #opname(#[count(children)] Result<(), #err_ty>)
                    }
                }
            }).collect::<Vec<_>>();
        if needs_client_error_variant {
            variants.push(quote! {
                ClientError(#[count(children)] idol_runtime::ClientError)
            })
        };
        Some(Counters::new(self.clone(), iface, variants, true))
    }

    pub(crate) fn client(&self, iface: &syntax::Interface) -> Option<Counters> {
        if !self.client_counters {
            return None;
        }
        let variants = iface.ops.iter().map(|(opname, op)| match &op.reply {
            syntax::Reply::Simple(_) => quote! { #opname },
            syntax::Reply::Result { err, .. } => {
                let err_ty = match err {
                    syntax::Error::CLike(ty) | syntax::Error::Complex(ty) => {
                        quote! { #ty }
                    }
                    syntax::Error::ServerDeath => {
                        quote! { idol_runtime::ServerDeath }
                    }
                };
                quote! {
                    #opname(#[count(children)] Result<(), #err_ty>)
                }
            }
        });
        Some(Counters::new(self.clone(), iface, variants, false))
    }
}

pub(crate) struct Counters {
    pub(crate) event_enum: syn::Ident,
    pub(crate) enum_def: TokenStream,
    pub(crate) counters_static: syn::Ident,
    pub(crate) settings: CounterSettings,
    server: bool,
}

impl Counters {
    pub(crate) fn new(
        settings: CounterSettings,
        iface: &syntax::Interface,
        variants: impl IntoIterator<Item = TokenStream>,
        server: bool,
    ) -> Self {
        let event_enum = quote::format_ident!("{}Event", iface.name);
        let variants = variants.into_iter();
        let enum_def = quote! {
            #[derive(counters::Count)]
            #[allow(non_camel_case_types)]
            pub enum #event_enum {
                #(#variants),*
            }
        };

        let counters_static = {
            let name = if server { "SERVER" } else { "CLIENT" };
            quote::format_ident!("__{}_{name}_COUNTERS", iface.name.uppercase())
        };

        Self {
            server,
            event_enum,
            enum_def,
            counters_static,
            settings,
        }
    }

    pub(crate) fn generate_defs(&self) -> TokenStream {
        let Self {
            enum_def,
            event_enum,
            counters_static,
            ..
        } = self;
        quote! {
            #enum_def

            counters::counters!(#counters_static, #event_enum);
        }
    }

    pub fn count_simple_op(&self, op: &syntax::Name) -> TokenStream {
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
    ) -> TokenStream {
        let Self {
            ref event_enum,
            ref counters_static,
            ref settings,
            server,
            ..
        } = *self;
        if server && settings.combine_client_errors {
            quote! {
                counters::count!(#counters_static,
                    match r {
                        Ok(_) => #event_enum::#op(Ok(())),
                        Err(idol_runtime::RequestError::ClientError(ref e)) => #event_enum::ClientError(*e),
                        Err(ref val) => #event_enum::#op(Err(*val)),
                });
            }
        } else if server {
            quote! {
                counters::count!(#counters_static,
                    #event_enum::#op(match r {
                        Ok(_) => Ok(()),
                        Err(ref e) => Err(*e),
                    })
                );
            }
        } else {
            quote! {
                counters::count!(#counters_static, #event_enum::#op(#result));
            }
        }
    }
}

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::{common, syntax};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::collections::BTreeMap;
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

pub enum ServerStyle {
    InOrder,
}

pub fn build_server_support(
    source: &str,
    stub_name: &str,
    style: ServerStyle,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    build_restricted_server_support(source, stub_name, style, &BTreeMap::new())
}

pub fn build_restricted_server_support(
    source: &str,
    stub_name: &str,
    style: ServerStyle,
    allowed_callers: &BTreeMap<String, Vec<usize>>,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let out = &PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let mut stub_file = File::create(out.join(stub_name)).unwrap();

    let text = std::fs::read_to_string(source)?;
    let iface: syntax::Interface = ron::de::from_str(&text)?;
    let mut tokens =
        generate_restricted_server_support(&iface, style, allowed_callers)?;

    tokens.extend(generate_server_section(&iface, &text));
    let formatted = common::fmt_tokens(tokens)?;
    write!(stub_file, "{formatted}")?;
    println!("cargo:rerun-if-changed={}", source);
    Ok(())
}

// `Name` is only mutable as it contains `OnceCell`s, but they don't effect its
// `Hash`, `PartialEq`, `Eq`, `Ord`, or `PartialOrd` implementations. So, it can
// safely be used as a map key.
#[allow(clippy::mutable_key_type)]
pub fn generate_restricted_server_support(
    iface: &syntax::Interface,
    style: ServerStyle,
    allowed_callers: &BTreeMap<String, Vec<usize>>,
) -> Result<TokenStream, Box<dyn std::error::Error + Send + Sync>> {
    let mut tokens = quote! {
        #[allow(unused_imports)]
        use userlib::UnwrapLite;
    };

    tokens.extend(generate_server_constants(iface));
    tokens.extend(generate_server_conversions(iface));
    tokens.extend(common::generate_op_enum(iface));
    tokens.extend(generate_server_op_impl(iface));

    tokens.extend(match style {
        ServerStyle::InOrder => {
            generate_server_in_order_trait(iface, allowed_callers)?
        }
    });

    Ok(tokens)
}

pub fn generate_server_constants(iface: &syntax::Interface) -> TokenStream {
    // Generate message sizing constants for each message.
    let mut msgsize_names = Vec::with_capacity(iface.ops.len());
    let consts = iface.ops.iter().map(|(name, op)| {
        let msg_size = {
            let const_name = format_ident!("{}_MSG_SIZE", name.uppercase());
            let val = match op.encoding {
                // Zerocopy moves fields as a packed struct, so the sum of input
                // type sizes is sufficient to fit the message.
                syntax::Encoding::Zerocopy => {
                    let vals = op.args.values().map(|arg| {
                        quote! {
                            + core::mem::size_of::<#arg>()
                        }
                    });
                    quote! { 0 #( #vals )* }
                }

                // ssmarshal guarantees that the serialized size will be no longer
                // than the size of the input struct. Note that this may be larger
                // than the sum of struct members!
                syntax::Encoding::Ssmarshal => {
                    let args = format_ident!("{}_{name}_ARGS", iface.name);
                    quote! { core::mem::size_of::<#args>() }
                }

                // hubpack's SerializedSize traits defines a `MAX_SIZE` associated
                // constant
                syntax::Encoding::Hubpack => {
                    let args = format_ident!("{}_{name}_ARGS", iface.name);
                    quote! { <#args as hubpack::SerializedSize>::MAX_SIZE }
                }
            };

            let tokens = quote! {
                pub const #const_name: usize = #val;
            };
            msgsize_names.push(const_name);
            tokens
        };
        let reply_size = {
            let const_name = name.as_reply_size();
            let val = match &op.reply {
                syntax::Reply::Result { ok, .. } => {
                    // This strategy only uses bytes for the OK side of the type,
                    // and only sends one type, so:
                    match op.encoding {
                        syntax::Encoding::Zerocopy
                        | syntax::Encoding::Ssmarshal => quote! {
                           core::mem::size_of::<#ok>()
                        },
                        syntax::Encoding::Hubpack => quote! {
                            <#ok as hubpack::SerializedSize>::MAX_SIZE
                        },
                    }
                }
                syntax::Reply::Simple(t) => match op.encoding {
                    syntax::Encoding::Zerocopy
                    | syntax::Encoding::Ssmarshal => quote! {
                        core::mem::size_of::<#t>()
                    },
                    syntax::Encoding::Hubpack => quote! {
                        <#t as hubpack::SerializedSize>::MAX_SIZE
                    },
                },
            };
            quote! {
                pub const #const_name: usize = #val;
            }
        };
        quote! {
            #msg_size
            #reply_size
        }
    });

    quote! {
        #( #consts )*

        // `max_incoming_size` may compare `max < 0`, which we want to ignore
        #[allow(clippy::absurd_extreme_comparisons)]
        const fn max_incoming_size() -> usize {
            let mut max = 0;
            #(
                if max < #msgsize_names {
                    max = #msgsize_names;
                }
            )*
            max
        }
        pub const INCOMING_SIZE: usize = max_incoming_size();
    }
}

pub fn generate_server_conversions(iface: &syntax::Interface) -> TokenStream {
    let conversions = iface.ops.iter().map(|(name, op)| {
        // Define args struct.
            let attrs = match op.encoding {
                syntax::Encoding::Zerocopy => quote! {
                    #[repr(C, packed)]
                    #[derive(Copy, Clone, zerocopy::FromBytes, zerocopy::Unaligned)]
                },
                syntax::Encoding::Ssmarshal => quote! {
                    #[derive(Copy, Clone, serde::Deserialize)]
                },
                syntax::Encoding::Hubpack => quote! {
                    #[derive(Copy, Clone, serde::Deserialize, hubpack::SerializedSize)]
                },
            };
            if !matches!(op.encoding, syntax::Encoding::Zerocopy) {
                // The recv strategy thing only really makes sense for the Zerocopy
                // encoding. In particular, generated clients for the other encoding
                // won't use it.
                for (argname, arg) in &op.args {
                    if !matches!(arg.recv, syntax::RecvStrategy::FromBytes) {
                        panic!("operation {name} argument {argname} uses a recv strategy, \
                            but this won't work with Ssmarshal/Hubpack encoding (remove it)");
                    }
                }
            }
            let mut need_args_impl = false;
            let args = op.args.iter().map(|(argname, arg)| {
                match &arg.recv {
                    syntax::RecvStrategy::FromBytes if arg.ty.is_bool() => {
                        // Special-case handling to send bools using a Zerocopy
                        // encoding strategy, for efficiency.
                        let ident =argname.raw_prefixed();
                        need_args_impl = true;
                        quote! {
                            pub #ident: u8
                        }
                    }
                    syntax::RecvStrategy::FromBytes => {
                        let ty = &arg.ty;
                        quote! {pub #argname: #ty }
                    }
                    syntax::RecvStrategy::FromPrimitive(ty)
                    | syntax::RecvStrategy::From(ty, _) => {
                        need_args_impl = true;
                        let ident = argname.raw_prefixed();
                        quote! {
                            pub #ident: #ty
                        }
                    }
                }
            });
            let struct_name = format_ident!("{}_{name}_ARGS", iface.name);
            let struct_def = quote! {
                #attrs
                #[allow(non_camel_case_types)]
                pub struct #struct_name {
                    #( #args ),*
                }
            };
            let args_impl = if need_args_impl {
                let args_fns = op.args.iter().map(|(argname, arg)| {
                    match &arg.recv {
                        syntax::RecvStrategy::FromPrimitive(ty) => {
                            let arg_ty = &arg.ty;
                            let raw_argname = argname.raw_prefixed();
                            let from_ty = format_ident!("from_{ty}");
                            quote! {
                                pub fn #argname(&self) -> Option<#arg_ty> {
                                    userlib::FromPrimitive::#from_ty(self.#raw_argname)
                                }
                            }
                        }
                        syntax::RecvStrategy::FromBytes if arg.ty.is_bool() => {
                            // The only FromBytes type which also needs a decoder
                            // function is a `bool` encoded as a single `u8`
                            let raw_argname = argname.raw_prefixed();
                            quote! {
                                pub fn #argname(&self) -> bool {
                                    self.#raw_argname != 0
                                }
                            }
                        }
                        _ => quote! {}
                    }
                });
                quote! {
                    impl #struct_name {
                        #( #args_fns )*
                    }
                }
            } else {
                quote! {}
            };

            let read_fn = {
                let read_fn = format_ident!("read_{}_msg", name);
                match op.encoding {
                    syntax::Encoding::Zerocopy => quote! {
                        pub fn #read_fn(bytes: &[u8]) -> Option<&#struct_name> {
                            zerocopy::LayoutVerified::<_, #struct_name>::new_unaligned(bytes)
                                .ok()
                                .into_ref()
                        }
                    },
                    syntax::Encoding::Ssmarshal => quote! {
                        pub fn #read_fn(bytes: &[u8]) -> Option<#struct_name> {
                            ssmarshal::deserialize(bytes).ok().map(|(x, _)| x)
                        }
                    },
                    syntax::Encoding::Hubpack => quote! {
                        pub fn #read_fn(bytes: &[u8]) -> Option<#struct_name> {
                            hubpack::deserialize(bytes).ok().map(|(x, _)| x)
                        }
                    },
                }
            };

            // The DWARF generated for types is load-bearing in that Humility
            // potentially needs them to be able to form arguments to Idol calls
            // and to make sense of the reply.  But if an Idol server is declared
            // without a consuming Idol client, any synthetic reply type won't be
            // generated.  To force any synthetic reply type definition to be
            // generated, we create a meaningless static (that itself will be
            // optimized away), which has the side-effect of getting the type that
            // we need in the binary (but without changing the generated text or
            // data).
            let mut reply_ty_def = quote! {};

            if let syntax::Reply::Result { ok, err: _ } = &op.reply {
                if let syntax::Encoding::Zerocopy = op.encoding {
                    let reply_ty = format_ident!("{}_{name}_REPLY", iface.name);
                    let static_name = format_ident!(
                        "{}_{}_REPLY",
                        iface.name.uppercase(),
                        name.uppercase(),
                    );
                    reply_ty_def = quote! {
                        #[repr(C, packed)]
                        struct #reply_ty {
                            value: #ok,
                        }
                        #[allow(dead_code)]
                        static #static_name: Option<&#reply_ty> = None;
                    }
                }
            };

            quote! {
                #struct_def
                #args_impl
                #read_fn
                #reply_ty_def
            }
        });
    quote! {
        #( #conversions )*
    }
}

pub fn generate_server_op_impl(iface: &syntax::Interface) -> TokenStream {
    let op_enum = iface.name.as_op_enum();
    let max_reply_size_cases = iface.ops.keys().map(|opname| {
        let reply_size = opname.as_reply_size();
        quote! {
            Self::#opname => #reply_size,
        }
    });
    let required_leases_cases = iface.ops.iter().map(|(opname, op)| {
        let leases = op.leases.len();
        quote! {
            Self::#opname => #leases
        }
    });
    quote! {
        #[automatically_derived]
        impl idol_runtime::ServerOp for #op_enum {
            fn max_reply_size(self) -> usize {
                match self {
                    #( #max_reply_size_cases ),*
                }
            }

            fn required_lease_count(self) -> usize {
                match self {
                    #( #required_leases_cases ),*
                }
            }
        }
    }
}

// `Name` is only mutable as it contains `OnceCell`s, but they don't effect its
// `Hash`, `PartialEq`, `Eq`, `Ord`, or `PartialOrd` implementations. So, it can
// safely be used as a map key.
#[allow(clippy::mutable_key_type)]
pub fn generate_server_in_order_trait(
    iface: &syntax::Interface,
    allowed_callers: &BTreeMap<String, Vec<usize>>,
) -> Result<TokenStream, Box<dyn std::error::Error + Send + Sync>> {
    // Ensure any operations listed in `allowed_callers` actually exist for this
    // server.
    for opname in allowed_callers.keys() {
        if !iface.ops.contains_key(opname.as_str()) {
            return Err(Box::from(format!(
                "allowed_callers operation `{}` does not exist for this server",
                opname
            )));
        }
    }

    let iface_name = &iface.name;
    let trt = format_ident!("InOrder{iface_name}Impl");
    let trait_def = generate_trait_def(iface, &trt);

    let enum_name = iface.name.as_op_enum();
    let op_cases = iface.ops.iter().map(|(opname, op)| {
        let check_allowed = if let Some(allowed_callers) = allowed_callers.get(opname.as_str()) {
            // With our current optimization settings and rustc/llvm version,
            // the compiler generates better code for raw `if` checks than it
            // does for the more general `[T;N].contains(&T)`. We'll do a bit of
            // manual optimization here; if `allowed_callers` is less than 4
            // (which we expect it to be basically always), we'll generate a
            // suitable `if`. For longer allowed_callers lists, we'll fall back
            // to `[T;N].contains(&T)`, which produces a loop.
            let cond = if allowed_callers.len() < 4 {
                quote! {
                    {
                        let sender = rm.sender.index();
                        #( sender != #allowed_callers )&&*
                    }
                }
            } else {
                quote! {
                    ![#( #allowed_callers ),*].contains(&rm.sender.index())
                }
            };
            quote! {
                if #cond {
                    return Err(idol_runtime::RequestError::Fail(idol_runtime::ClientError::AccessViolation));
                }
            }
        } else {
            quote!{}
        };
        let read = {
            let arg_var = if op.args.is_empty() {
                quote! { _ }
            } else {
                quote! { args }
            };
            let readfn = format_ident!("read_{opname}_msg");
            quote! {
                let #arg_var = #readfn(incoming).ok_or_else(|| idol_runtime::ClientError::BadMessageContents.fail())?;
            }
        };
        let args = op.args.iter().map(|(argname, arg)| {
            match &arg.recv {
                syntax::RecvStrategy::FromBytes => {
                    let thingy = if arg.ty.is_bool() {
                        quote! { #argname() }
                    } else {
                        quote! { #argname }
                    };
                    quote! {
                        args.#thingy
                    }
                },
                syntax::RecvStrategy::From(_, None) => {
                    let name = argname.raw_prefixed();
                    quote! {
                        args.#name.into()
                    }
                }
                syntax::RecvStrategy::From(_, Some(f)) => {
                    let name = argname.raw_prefixed();
                    quote! {
                        #f(args.#name)
                    }
                }
                syntax::RecvStrategy::FromPrimitive(_) => {
                    quote! {
                        args.#argname().ok_or_else(|| idol_runtime::ClientError::BadMessageContents.fail())?
                    }
                }
            }
        });
        let leases = op.leases.iter().enumerate().map(|(i, (leasename, lease))| {
            // This is gross, but, let's spot us some slices :-(
            let fun = match (lease.read, lease.write) {
                (true, false) => "read_only",
                (false, true) => "write_only",
                (true, true) => "read_write",
                _ => unreachable!(),
            };

            let (fun, limit) = if lease.ty.appears_unsized() {
                let max_len = if let Some(n) = lease.max_len {
                    // It's ok to unwrap the value in server code because we've
                    // just gotten it _out of_ a NonZeroU32 here, so we know
                    // it'll be statically valid.
                    let n = n.get();
                    quote! {
                        , Some(core::num::NonZeroU32::new(#n).unwrap_lite())
                    }
                } else {
                    quote!{ , None }
                };
                (format_ident!("{fun}_slice"), max_len)
            } else if lease.max_len.is_some() {
                panic!(
                    "Lease {i} ({leasename}) on operation {iface_name}.{opname} \
                    has sized type but also max_len field"
                );
            } else {
                (format_ident!("{fun}"), quote!{} )
            };
            let maybe_unwrap = if lease.max_len.is_some() {
                quote! { .try_into().unwrap_lite() }
            } else {
                quote!{}
            };
            quote! {
                idol_runtime::Leased::#fun(rm.sender, #i #limit).ok_or_else(|| ClientError::BadLease.fail())?#maybe_unwrap
            }
        });
        let reply = {
            let encode = match op.encoding {
                syntax::Encoding::Zerocopy => quote! {
                    userlib::sys_reply(rm.sender, 0, zerocopy::AsBytes::as_bytes(&val));
                },
                syntax::Encoding::Hubpack | syntax::Encoding::Ssmarshal => {
                    let reply_size = opname.as_reply_size();
                    let serializer = op.encoding.crate_name();
                    quote! {
                        let mut reply_buf = [0u8; #reply_size];
                        let n_reply = #serializer::serialize(&mut reply_buf, &val).map_err(|_| ()).unwrap_lite();
                        userlib::sys_reply(rm.sender, 0, &reply_buf[..n_reply]);
                    }
                }
            };
            match &op.reply {
                syntax::Reply::Simple(_) => quote! {
                    match r {
                        Ok(val) => {
                            #encode
                            Ok(())
                        }
                        // Simple returns can only return ClientError. The compiler
                        // can't see this. Jump through some hoops:
                        Err(val) => Err(val.map_runtime(|e| match e {})),
                    }
                },
                syntax::Reply::Result{ err, .. } => {
                    let err_case = match err {
                        // It might be surprising, but to return a complex error
                        // we need to behave very much like the reply code
                        // above: rather than returning `Err`, we need to
                        // perform an actual `sys_reply` and then return `Ok` to
                        // avoid triggering the reply handling code in the
                        // generic dispatch loop.
                        //
                        // So, the fact that this error returns `Ok` is not a
                        // bug.
                        syntax::Error::Complex (ty) =>  match op.encoding {
                            syntax::Encoding::Hubpack => quote! {
                                match val {
                                    idol_runtime::RequestError::Fail(f) => {
                                        // Note: because of the way `into_fault` works,
                                        // if it returns None, we don't send a reply at
                                        // all. This is because None indicates that the
                                        // caller was restarted or otherwise crashed
                                        if let Some(fault) = f.into_fault() {
                                                userlib::sys_reply_fault(rm.sender, fault);
                                        }
                                    }
                                    idol_runtime::RequestError::Runtime(e) => {
                                        let mut reply_buf = [0u8; <#ty as hubpack::SerializedSize>::MAX_SIZE];
                                        let n_reply = hubpack::serialize(&mut reply_buf, &e).map_err(|_| ()).unwrap_lite();
                                        userlib::sys_reply(rm.sender, 1, &reply_buf[..n_reply]);
                                    }
                                }
                                Ok(())
                            },
                            encoding => panic!("Complex error types not supported for {encoding:?} encoding"),
                        },
                        syntax::Error::CLike(_) => quote! {
                            Err(val.map_runtime(u16::from))
                        },
                        syntax::Error::ServerDeath => quote! {
                            Err(val.map_runtime(|e| match e {}))
                        }
                    };
                    quote! {
                        match r {
                            Ok(val) => {
                                #encode
                                Ok(())
                            }
                            Err(val) => {
                                #err_case
                            }
                        }
                    }
                }
            }
        };
        quote! {
            #enum_name::#opname => {
                #check_allowed
                #read
                let r = self.1.#opname(
                    rm,
                    #( #args ),*,
                    #( #leases ),*
                );
                #reply
            }
        }
    });
    Ok(quote! {
        #trait_def

        #[automatically_derived]
        impl <S: #trt> idol_runtime::Server<#enum_name> for (core::marker::PhantomData<#enum_name>, &'_ mut S) {
            fn recv_source(&self) -> Option<userlib::TaskId> {
                <S as #trt>::recv_source(self.1)
            }

            fn closed_recv_fail(&mut self) {
                <S as #trt>::closed_recv_fail(self.1)
            }
            fn handle(
                &mut self,
                op: #enum_name,
                incoming: &[u8],
                rm: &userlib::RecvMessage,
            ) -> Result<(), idol_runtime::RequestError<u16>> {
                #[allow(unused_imports)]
                use core::convert::TryInto;
                use idol_runtime::ClientError;
                match op {
                    #( #op_cases )*
                }
            }
        }
    })
}

fn generate_trait_def(
    iface: &syntax::Interface,
    trt: &syn::Ident,
) -> TokenStream {
    let ops = iface.ops.iter().map(|(name, op)| {
        let args = op.args.iter().map(|(argname, arg)| {
            let ty = &arg.ty;
            quote! {
                #argname: #ty
            }
        });
        let leases = op.leases.iter().map(|(leasename, lease)| {
            let r = if lease.read { "R" } else { ""};
            let w = if lease.write { "W" } else { ""};
            let lease_kind = format_ident!("{r}{w}");
            let ty = &lease.ty;
            if let Some(n) = lease.max_len {
                let n = n.get();
                quote! {
                    #leasename: idol_runtime::LenLimit<idol_runtime::Leased<idol_runtime::#lease_kind, #ty>, #n>
                }
            } else {
                quote! {
                    #leasename: idol_runtime::Leased<idol_runtime::#lease_kind, #ty>
                }
            }
        });
        let mut error_type_bounds = quote! {};
        let ret_ty = match &op.reply {
            syntax::Reply::Result { ok, err } => {
                let err_ty = match err {
                    syntax::Error::CLike(ty) => {
                        // For non-idempotent operations, generate a bound on
                        // the error type ensuring it can represent server
                        // death.
                        if !op.idempotent {
                            error_type_bounds = quote!{
                                where #ty: idol_runtime::IHaveConsideredServerDeathWithThisErrorType
                            };
                        }
                        quote! { #ty }
                    }
                    syntax::Error::Complex(ty) => {
                        error_type_bounds = quote! {
                            where #ty: From<idol_runtime::ServerDeath>
                        };
                        quote! { #ty }
                    }
                    syntax::Error::ServerDeath => {
                        quote! { core::convert::Infallible }
                    }
                };
                quote! { Result<#ok, idol_runtime::RequestError<#err_ty>> }
            },
            syntax::Reply::Simple(t) => {
                quote! { Result<#t, idol_runtime::RequestError<core::convert::Infallible>> }
            },
        };
        quote! {
            fn #name(
                &mut self,
                msg: &userlib::RecvMessage,
                #( #args ),*,
                #( #leases ),*
            ) -> #ret_ty #error_type_bounds;
        }
    });
    quote! {
        pub trait #trt: idol_runtime::NotificationHandler {
            fn recv_source(&self) -> Option<userlib::TaskId> {
                None
            }

            fn closed_recv_fail(&mut self) {
                panic!()
            }

            #( #ops )*
        }
    }
}

fn generate_server_section(
    iface: &syntax::Interface,
    text: &str,
) -> TokenStream {
    let name = format_ident!("_{}_IDOL_DEFINITION", iface.name.uppercase());
    let bytes = text.as_bytes();
    let len = bytes.len();
    let byte_str = syn::LitByteStr::new(bytes, proc_macro2::Span::call_site());
    quote! {
        // To allow it to be pulled out by debuggers, we drop the entirety of the
        // interface definition in a dedicated (unloaded) section
        #[used]
        #[link_section = ".idolatry"]
        static #name: [u8; #len] = #byte_str;
    }
}

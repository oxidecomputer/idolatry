// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::{common, syntax, Generator};
use proc_macro2::TokenStream;
use quote::quote;
use std::env;
use std::fs::File;
use std::path::PathBuf;

pub fn build_client_stub(
    source: &str,
    stub_name: &str,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    Generator::default().build_client_stub(source, stub_name)
}

pub fn generate_client_stub_from_file(
    source: impl AsRef<std::path::Path>,
    out: impl std::io::Write,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    Generator::default().generate_client_stub_from_file(source, out)
}

pub fn generate_client_stub(
    iface: &syntax::Interface,
    out: impl std::io::Write,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    Generator::default().generate_client_stub(iface, out)
}

impl Generator {
    pub fn build_client_stub(
        &self,
        source: &str,
        stub_name: &str,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let out = &PathBuf::from(env::var_os("OUT_DIR").unwrap());
        let stub_file = File::create(out.join(stub_name)).unwrap();
        self.generate_client_stub_from_file(source, stub_file)?;
        println!("cargo:rerun-if-changed={}", source);
        Ok(())
    }

    pub fn generate_client_stub_from_file(
        &self,
        source: impl AsRef<std::path::Path>,
        out: impl std::io::Write,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let text = std::fs::read_to_string(source)?;
        let iface: syntax::Interface = ron::de::from_str(&text)?;
        self.generate_client_stub(&iface, out)
    }

    pub fn generate_client_stub(
        &self,
        iface: &syntax::Interface,
        mut out: impl std::io::Write,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let mut tokens = self.generate_op_enum(iface);
        tokens.extend(self.client_stub_tokens(iface)?);
        let formatted = common::fmt_tokens(tokens)?;
        write!(out, "{formatted}")?;
        Ok(())
    }

    fn client_stub_tokens(
        &self,
        iface: &syntax::Interface,
    ) -> Result<TokenStream, Box<dyn std::error::Error + Send + Sync>> {
        let mut ops = Vec::with_capacity(iface.ops.len());
        let iface_name = &iface.name;
        let counters = self.counters.client(iface);
        for (idx, (name, op)) in iface.ops.iter().enumerate() {
            // Let's do some checks
            if op.idempotent {
                // An idempotent operation with a read-write lease seems like a
                // problem, since it could forward half-initialized state from one
                // instance of the server to the next, potentially propagating the
                // crash; we could potentially handle it but for now, we'll reject
                // it.
                if op.leases.values().any(|lease| lease.read && lease.write) {
                    return Err(
                        "idempotent operation with read/write lease".into()
                    );
                }
                match &op.reply {
                    syntax::Reply::Result { err, .. }
                        if matches!(err, syntax::Error::ServerDeath) =>
                    {
                        return Err(
                            format!("idempotent operations should not indicate server death: {name}")
                                .into(),
                        );
                    }
                    _ => (),
                }
            } else {
                // A non-idempotent operation had better have an error type.
                match &op.reply {
                    syntax::Reply::Result { .. } => (),
                    syntax::Reply::Simple(_) => {
                        return Err(format!(
                            "operation can't indicate server death: {name}",
                        )
                        .into());
                    }
                }
            }

            let idempotent = if op.idempotent {
                quote! {
                    /// idempotent - will auto-retry
                }
            } else {
                quote! {
                    /// not idempotent, error type must represent death
                }
            };
            let operation = format!(" operation: {name} ({idx})");
            let args = op.args.iter().map(|(name, arg)| {
                let ty = &arg.ty;
                quote! {
                    #name: #ty
                }
            });
            let leases = op.leases.iter().map(|(name, lease)| {
                let reftype = if lease.write {
                    quote! { &mut }
                } else if lease.read {
                    quote! { & }
                } else {
                    panic!("lease {name} grants no access");
                };
                let ty = &lease.ty;
                quote! {
                    #name: #reftype #ty
                }
            });
            let reply_ty = match &op.reply {
                syntax::Reply::Result { ok, err } => {
                    let err = match err {
                        syntax::Error::CLike(ty)
                        | syntax::Error::Complex(ty) => {
                            quote! { #ty }
                        }
                        syntax::Error::ServerDeath => {
                            quote! { idol_runtime::ServerDeath }
                        }
                    };
                    quote! { Result<#ok, #err> }
                }
                syntax::Reply::Simple(t) => {
                    quote! { #t }
                }
            };
            // Map the args with user-chosen names, which are good for rustdoc, into
            // args with names that are guaranteed not to collide with our generated
            // identifiers.
            let argmap = {
                let argnames = {
                    let args = op.args.keys().map(syntax::Name::arg_prefixed);
                    let leases =
                        op.leases.keys().map(syntax::Name::arg_prefixed);
                    args.chain(leases)
                };
                let argvals = op.args.keys().chain(op.leases.keys());
                quote! {
                    let (
                        #(#argnames),*
                    ) = (
                        #(#argvals),*
                    );
                }
            };

            // Perform lease validation.
            let lease_validators =
                op.leases.iter().filter_map(|(leasename, lease)| {
                    // cast to usize here is load bearing, because `quote` will
                    // suffix this with `{n}u32` when interpolating, but `.len()`
                    // returns a `usize`. blah!
                    let n = lease.max_len?.get() as usize;
                    let argname = leasename.arg_prefixed();
                    // Note: we're not generating a panic message in the client to
                    // save ROM space. If the user chases the line number into the
                    // client stub source file the error should be clear.
                    Some(quote! {
                        if #argname.len() > #n {
                            panic!();
                        }
                    })
                });

            // Define args struct.
            let arg_struct_name =
                quote::format_ident!("{}_{name}_ARGS", iface.name);
            let arg_struct = {
                let attrs = match op.encoding {
                    syntax::Encoding::Zerocopy => {
                        quote! {
                            #[derive(
                                zerocopy_derive::IntoBytes,
                                zerocopy_derive::Immutable,
                            )]
                            #[repr(C, packed)]
                        }
                    }
                    syntax::Encoding::Ssmarshal => {
                        quote! {
                            #[derive(serde::Serialize)]
                        }
                    }
                    syntax::Encoding::Hubpack => {
                        quote! {
                            #[derive(serde::Serialize, hubpack::SerializedSize)]
                        }
                    }
                };
                let fields = op.args.iter().map(|(argname, arg)| {
                    let ty = if arg.ty.is_bool() {
                        quote! { u8 }
                    } else {
                        quote! { #arg }
                    };
                    quote! {
                        #argname: #ty
                    }
                });
                quote! {
                    #[allow(non_camel_case_types)]
                    #attrs
                    struct #arg_struct_name {
                        #( #fields ),*
                    }
                }
            };

            // Determine required size of reply buffer.
            fn size_expr(
                encoding: &syntax::Encoding,
                ty: &syntax::AttributedTy,
            ) -> TokenStream {
                match encoding {
                    syntax::Encoding::Zerocopy
                    | syntax::Encoding::Ssmarshal => {
                        // Both these encodings guarantee that sizeof is big
                        // enough.
                        quote! { core::mem::size_of::<#ty>() }
                    }
                    syntax::Encoding::Hubpack => {
                        quote! { <#ty as hubpack::SerializedSize>::MAX_SIZE }
                    }
                }
            }
            let reply_size = {
                let size = match &op.reply {
                    syntax::Reply::Simple(t) => size_expr(&op.encoding, t),
                    syntax::Reply::Result { ok, err } => {
                        let oksize = size_expr(&op.encoding, ok);
                        let errsize = match err {
                            syntax::Error::CLike(_)
                            | syntax::Error::ServerDeath => {
                                quote! { 0 }
                            }
                            syntax::Error::Complex(ty) => {
                                quote! { <#ty as hubpack::SerializedSize>::MAX_SIZE; }
                            }
                        };
                        quote! {
                            let oksize = #oksize;
                            let errsize = #errsize;
                            if oksize > errsize { oksize } else { errsize }
                        }
                    }
                };
                quote! {
                    const REPLY_SIZE: usize = {
                        #size
                    };
                }
            };

            // Create instance of args struct from args.
            let mk_arg_struct = {
                let initializers = op.args.iter().map(|(argname, arg)| {
                    let arg_argname = argname.arg_prefixed();
                    if arg.ty.is_bool() {
                        // Special case: we send booleans as non-zero u8, so that
                        // we can use them in Zerocopy situations
                        quote! { #argname: #arg_argname as u8 }
                    } else {
                        quote! { #argname: #arg_argname }
                    }
                });
                let encoding_prep = match op.encoding {
                    syntax::Encoding::Zerocopy => {
                        // No further prep required.
                        quote! {}
                    }
                    syntax::Encoding::Ssmarshal => {
                        // Serialize the arguments.
                        quote! {
                            let mut argsbuf = [0; core::mem::size_of::<#arg_struct_name>()];
                            let arglen = ssmarshal::serialize(&mut argsbuf, &args).unwrap_lite();
                        }
                    }
                    syntax::Encoding::Hubpack => {
                        // Serialize the arguments.
                        quote! {
                            let mut argsbuf = [0; <#arg_struct_name as hubpack::SerializedSize>::MAX_SIZE];
                            let arglen = hubpack::serialize(&mut argsbuf, &args).unwrap_lite();
                        }
                    }
                };
                quote! {
                    let args = #arg_struct_name {
                        #( #initializers ),*
                    };
                    #encoding_prep
                }
            };

            let send = {
                let op_enum_name = iface_name.as_op_enum();
                let buf = match op.encoding {
                    syntax::Encoding::Zerocopy => {
                        quote! { zerocopy::IntoBytes::as_bytes(&args) }
                    }
                    syntax::Encoding::Ssmarshal | syntax::Encoding::Hubpack => {
                        quote! { &argsbuf[..arglen] }
                    }
                };
                let leases = op.leases.iter().map(|(leasename, lease)| {
                    let (ctor, asbytes) = match (lease.read, lease.write) {
                        (true, true) => ("read_write", "as_mut_bytes"),
                        (false, true) => ("write_only", "as_mut_bytes"),
                        (true, false) => ("read_only", "as_bytes"),
                        (false, false) => panic!("should have been caught above"),
                    };
                    let ctor =
                        syn::Ident::new(ctor, proc_macro2::Span::call_site());
                    let asbytes =
                        syn::Ident::new(asbytes, proc_macro2::Span::call_site());
                    let argname = leasename.arg_prefixed();
                    quote! {
                        userlib::Lease::#ctor(zerocopy::IntoBytes::#asbytes(#argname))
                    }
                });
                quote! {
                    let task = self.current_id.get();
                    let (rc, len) = sys_send(
                        task,
                        #op_enum_name::#name as u16,
                        #buf,
                        &mut reply,
                        &[
                            #( #leases ),*
                        ],
                    );
                }
            };

            let reply = {
                let gen_zerocopy_decode = |repr_ty: &syntax::Ty, msg: &str| {
                    let reply_ty =
                        quote::format_ident!("{iface_name}_{name}_{msg}");
                    quote! {
                        let _len = len;
                        #[derive(
                            zerocopy_derive::FromBytes,
                            zerocopy_derive::Unaligned,
                        )]
                        #[repr(C, packed)]
                        struct #reply_ty {
                            value: #repr_ty,
                        }
                        let v: #repr_ty = zerocopy::FromBytes::read_from_bytes(&reply[..]).unwrap_lite();
                    }
                };
                let gen_decode = |t: &syntax::AttributedTy| match op.encoding {
                    syntax::Encoding::Zerocopy => {
                        let repr_ty = t.repr_ty();
                        gen_zerocopy_decode(repr_ty, "REPLY")
                    }
                    syntax::Encoding::Ssmarshal => quote! {
                        let (v, _): (#t, _) = ssmarshal::deserialize(&reply[..len]).unwrap_lite();
                    },
                    syntax::Encoding::Hubpack => quote! {
                        let (v, _): (#t, _) = hubpack::deserialize(&reply[..len]).unwrap_lite();
                    },
                };

                match &op.reply {
                    syntax::Reply::Simple(t) => {
                        let mut decode = gen_decode(t);
                        if let syn::Type::Tuple(tt) = &t.ty.0 {
                            if tt.elems.is_empty() {
                                // Override for the `Simple("()")` case.
                                decode = quote::quote! { let v = (); };
                            }
                        }
                        let count = match counters {
                            Some(ref ctrs) => ctrs.count_simple_op(name),
                            None => quote! {},
                        };
                        let ret = match &t.recv {
                            syntax::RecvStrategy::FromBytes
                                if t.ty.is_bool() =>
                            {
                                quote! {return v != 0;}
                            }
                            syntax::RecvStrategy::FromBytes => {
                                quote! {return v;}
                            }
                            syntax::RecvStrategy::From(_, None) => {
                                quote! { return v.into(); }
                            }
                            syntax::RecvStrategy::From(_, Some(f)) => {
                                quote! { return #f(v); }
                            }
                            syntax::RecvStrategy::FromPrimitive(p) => {
                                let from_prim =
                                    quote::format_ident!("from_{p}");
                                quote! { return <#t as userlib::FromPrimitive>::#from_prim(v).unwrap_lite(); }
                            }
                        };
                        // We're going to assume rc is zero at this point since
                        // we've already extracted the server death case. This
                        // avoids an otherwise-hard-to-avoid client panic site
                        // that doesn't happen in practice.
                        quote! {
                            #decode
                            #count
                            #ret
                        }
                    }
                    syntax::Reply::Result { ok, err } => {
                        let decode = gen_decode(ok);
                        let ret_ok = match &ok.recv {
                            syntax::RecvStrategy::FromBytes
                                if ok.ty.is_bool() =>
                            {
                                quote! { return Ok(v != 0); }
                            }
                            syntax::RecvStrategy::FromBytes => {
                                quote! { return Ok(v); }
                            }
                            syntax::RecvStrategy::From(_, None) => {
                                quote! { return Ok(v.into()) }
                            }
                            syntax::RecvStrategy::From(_, Some(f)) => {
                                quote! { return Ok(#f(v)) }
                            }
                            syntax::RecvStrategy::FromPrimitive(p) => {
                                let from_prim =
                                    quote::format_ident!("from_{p}");
                                quote! { return Ok(<#ok as userlib::FromPrimitive>::#from_prim(v).unwrap_lite()); }
                            }
                        };
                        let ret_err = match err {
                            syntax::Error::CLike(ty) => {
                                let check_server_death = if op.idempotent {
                                    // Idempotent ops already checked for server death
                                    // above.
                                    quote! {}
                                } else {
                                    quote! {
                                        if let Some(g) = userlib::extract_new_generation(rc) {
                                            self.current_id.set(userlib::TaskId::for_index_and_gen(task.index(), g));
                                        }
                                    }
                                };
                                let count = match counters {
                                    Some(ref ctrs) => ctrs
                                        .count_result(name, quote! {Err(err)}),
                                    None => quote! {},
                                };
                                quote! {
                                    #check_server_death
                                    let err = <#ty as core::convert::TryFrom<u32>>::try_from(rc)
                                        .unwrap_lite();
                                    #count
                                    return Err(err);
                                }
                            }
                            syntax::Error::Complex(ty) => {
                                let decode_v = match op.encoding {
                                    syntax::Encoding::Hubpack => {
                                        quote! {
                                            let (v, _): (#ty, _) = hubpack::deserialize(&reply[..len]).unwrap_lite();
                                        }
                                    }
                                    syntax::Encoding::Zerocopy => {
                                        gen_zerocopy_decode(ty, "ERROR")
                                    }
                                    e => {
                                        panic!(
                                            "Complex error types not supported for \
                                            {e:?} encoding, sorry"
                                        );
                                    }
                                };
                                let count = match counters {
                                    Some(ref ctrs) => {
                                        ctrs.count_result(name, quote! {Err(v)})
                                    }
                                    None => quote! {},
                                };
                                let check_server_death = if op.idempotent {
                                    // Idempotent ops already checked for server death
                                    // above.
                                    quote! {}
                                } else {
                                    quote! {
                                        if let Some(g) = userlib::extract_new_generation(rc) {
                                            self.current_id.set(userlib::TaskId::for_index_and_gen(task.index(), g));
                                            let v = #ty::from(idol_runtime::ServerDeath);
                                            #count
                                            return Err(v);
                                        }
                                    }
                                };
                                quote! {
                                    #check_server_death
                                    #decode_v
                                    #count
                                    return Err(v);
                                }
                            }
                            syntax::Error::ServerDeath => {
                                assert!(!op.idempotent, "idempotent operations should not indicate server death");
                                let count = match counters {
                                    Some(ref counters) => counters.count_result(
                                        name,
                                        quote! {Err(idol_runtime::ServerDeath)},
                                    ),
                                    None => quote! {},
                                };
                                quote! {
                                    if let Some(g) = userlib::extract_new_generation(rc) {
                                        self.current_id.set(userlib::TaskId::for_index_and_gen(task.index(), g));
                                        #count
                                        return Err(idol_runtime::ServerDeath);
                                    } else {
                                        panic!()
                                    }
                                }
                            }
                        };
                        let count_ok = match counters {
                            Some(ref counters) => {
                                counters.count_result(name, quote! {Ok(())})
                            }
                            None => quote! {},
                        };
                        quote! {
                            if rc == 0 {
                                #decode
                                #count_ok
                                #ret_ok
                            } else {
                                #ret_err
                            }
                        }
                    }
                }
            };

            let body = if op.idempotent {
                quote! {
                    loop {
                        #send

                        // If the operation is idempotent but failed due to server death, retry.
                        if let Some(g) = userlib::extract_new_generation(rc) {
                            self.current_id.set(userlib::TaskId::for_index_and_gen(task.index(), g));
                            continue;
                        }

                        #reply
                    }
                }
            } else {
                quote! {
                    #send
                    #reply
                }
            };
            ops.push(quote! {
                #[doc = #operation]
                #idempotent
                pub fn #name(&self, #(#args,)* #(#leases,)*) -> #reply_ty {
                    #argmap
                    #(#lease_validators)*
                    #arg_struct
                    #reply_size
                    let mut reply = [0u8; REPLY_SIZE];
                    #mk_arg_struct
                    #body
                }
            })
        }
        let counters = counters
            .as_ref()
            .map(|ctrs| ctrs.generate_defs())
            .unwrap_or_default();

        Ok(quote! {
            #[allow(unused_imports)]
            use userlib::UnwrapLite;

            #[derive(Clone, Debug)]
            pub struct #iface_name {
                current_id: core::cell::Cell<userlib::TaskId>,
            }

            #[automatically_derived]
            impl From<userlib::TaskId> for #iface_name {
                fn from(x: userlib::TaskId) -> Self {
                    Self { current_id: core::cell::Cell::new(x) }
                }
            }

            #counters

            #[allow(clippy::let_unit_value,
                    clippy::collapsible_else_if,
                    clippy::needless_return,
                    clippy::unused_unit,
                    // code generation may include unnecessary braces and/or parens
                    // if it doesn't know whether a block contains one or more
                    // expressions.
                    unused_braces,
                    unused_parens)]
            #[automatically_derived]
            impl #iface_name {
                #(#ops)*
            }
        })
    }
}

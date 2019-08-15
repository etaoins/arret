#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

extern crate proc_macro;

#[macro_use]
extern crate quote;

use syn::{parse_macro_input, ItemFn, Token};

fn arg_is_task(arg: &syn::PatType) -> bool {
    if let syn::Type::Reference(_) = *arg.ty {
    } else {
        return false;
    };

    if let syn::Pat::Ident(ref pat_ident) = *arg.pat {
        pat_ident.ident == "task"
    } else {
        false
    }
}

/// Annotates a Rust function to be exported via `arret_runtime::define_rust_module!`
///
/// This takes a single metadata string containing the full Arret type of the function. This is used
/// to express concepts in Arret that don't exist in Rust. These include rest arguments and function
/// purity.
///
/// The annotated Rust function must take a `arret_runtime::task::Task` as its first parameter. An
/// attempt will be made to encode the types of the remaining parameters but only certain primitive
/// types and `arret_runtime::boxed` values are allowed.
#[proc_macro_attribute]
pub fn rust_fun(
    attrs: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attrs: proc_macro2::TokenStream = attrs.into();

    let mut attrs_iter = attrs.into_iter();
    let arret_type = attrs_iter.next().expect("Arret type expected");

    if attrs_iter.next().is_some() {
        panic!("unexpected tokens after Arret type");
    }

    // Parse the input tokens into a syntax tree
    let mut input_fn = parse_macro_input!(input as ItemFn);
    let mut input_sig = &mut input_fn.sig;
    let vis = input_fn.vis.clone();

    // Rename the function so the descriptor can take its original name
    let descriptor_ident = input_sig.ident.clone();
    let entry_point_name = format!("arret_{}_entry_point", input_sig.ident);
    input_sig.ident = proc_macro2::Ident::new(&entry_point_name, input_sig.ident.span());

    // RFI assumes a C ABI
    input_sig.abi = Some(syn::Abi {
        extern_token: Token![extern](input_sig.ident.span()),
        name: Some(syn::LitStr::new("C", input_sig.ident.span())),
    });

    let takes_task = input_sig
        .inputs
        .first()
        .map(|arg| match arg {
            syn::FnArg::Typed(typed) => arg_is_task(typed),
            _ => false,
        })
        .unwrap_or(false);

    let mut param_iter = input_sig.inputs.iter();
    if takes_task {
        param_iter.next();
    }

    let param_types = param_iter.map(|arg| match arg {
        syn::FnArg::Typed(typed) => typed.ty.clone(),
        _ => panic!("unexpected arg type"),
    });

    let ret_type = match input_sig.output {
        syn::ReturnType::Default => quote!(()),
        syn::ReturnType::Type(_, ref ret_type) => quote!(#ret_type),
    };

    // Build the output, possibly using quasi-quotation
    let expanded = quote! {
        #[allow(non_upper_case_globals)]
        #vis const #descriptor_ident: RustFun = RustFun {
            arret_type: #arret_type,
            takes_task: #takes_task,
            params: &[#(
                <#param_types as ::arret_runtime::abitype::EncodeABIType>::PARAM_ABI_TYPE
            ),*],
            ret: <#ret_type as ::arret_runtime::abitype::EncodeRetABIType>::RET_ABI_TYPE,
            symbol: #entry_point_name,
        };

        #[no_mangle]
        #input_fn
    };

    expanded.into()
}

#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

extern crate proc_macro;

#[macro_use]
extern crate quote;

use syn::{parse_macro_input, ItemFn, Token};

fn arg_is_task(arg: &syn::ArgCaptured) -> bool {
    if let syn::Type::Reference(_) = arg.ty {
    } else {
        return false;
    };

    if let syn::Pat::Ident(ref pat_ident) = arg.pat {
        return pat_ident.ident == "task";
    } else {
        return false;
    };
}

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
    let vis = input_fn.vis.clone();

    // Rename the function so the descriptor can take its original name
    let descriptor_ident = input_fn.ident.clone();
    let entry_point_name = format!("arret_{}_entry_point", input_fn.ident);
    input_fn.ident = proc_macro2::Ident::new(&entry_point_name, input_fn.ident.span());

    // RFI assumes a C ABI
    input_fn.abi = Some(syn::Abi {
        extern_token: Token![extern](input_fn.ident.span()),
        name: Some(syn::LitStr::new("C", input_fn.ident.span())),
    });

    let takes_task = input_fn
        .decl
        .inputs
        .first()
        .map(|arg| match arg.value() {
            syn::FnArg::Captured(captured) => arg_is_task(captured),
            _ => false,
        })
        .unwrap_or(false);

    let mut param_iter = input_fn.decl.inputs.iter();
    if takes_task {
        param_iter.next();
    }

    let param_types = param_iter.map(|arg| match arg {
        syn::FnArg::Captured(captured) => captured.ty.clone(),
        _ => panic!("unexpected arg type"),
    });

    let ret_type = match input_fn.decl.output {
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
                <#param_types as ::runtime::abitype::EncodeABIType>::PARAM_ABI_TYPE
            ),*],
            ret: <#ret_type as ::runtime::abitype::EncodeRetABIType>::RET_ABI_TYPE,
            symbol: #entry_point_name,
        };

        #[no_mangle]
        #input_fn
    };

    expanded.into()
}

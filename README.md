# Arret

## Overview

Arret is pure functional, strongly typed language with Lisp-like syntax.
It aims to combine the expressiveness of Lisp with guarantees provided by functional programming.
Concurrency is provided through an Erlang-like model of actors communicating using message passing.

Arret is a successor to [Llambda](https://github.com/etaoins/llambda) without the constraint of Scheme compatibility.

## Implementation

The compiler and runtime are implemented in Rust with code generation provided by LLVM.

## Status

Currently parsing, lowering, macros and type checking are implemented.
A basic garbage collected runtime is provided with a limited set of data types.
There is no support for compiling or evaluating programs at all.


# Arret

[![Build Status](https://travis-ci.org/etaoins/arret.svg?branch=master)](https://travis-ci.org/etaoins/arret)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Overview

Arret is pure functional, strongly typed language with Lisp-like syntax.
It aims to combine the expressiveness of Lisp with guarantees provided by functional programming.
Concurrency is provided through an Erlang-like model of actors communicating using message passing.

Arret is a successor to [Llambda](https://github.com/etaoins/llambda) without the constraint of Scheme compatibility.

## Installation

This project is participating in the [Rust 2018 Preview](https://internals.rust-lang.org/t/rust-2018-an-early-preview/7776).
As such it will require the nightly Rust compiler until Rust 2018 is released.

### Using rustup and Cargo

```shell
> curl https://sh.rustup.rs -sSf | sh
> rustup toolchain install nightly
> cd ~/path/to/repo/root
> cargo +nightly run
```

### Using Docker

```shell
> cd ~/path/to/repo/root
> docker build . -t arret
> docker run -ti arret
```

## Usage

The best way to explore Arret is by using the REPL

```
arret> (length '(1 2 3 4 5))
=> 5
arret> (defn identity #{T} ([x : T]) -> T x)
defined
arret> :type identity
=> #{T} (T -> T)
arret> (identity "Hello, world!")
=> "Hello, world!"
arret> :type (identity [one two three])
=> (Vector 'one 'two 'three)
```

## Implementation

The compiler and runtime are implemented in Rust with code generation provided by LLVM.

## Status

Currently parsing, macros, type checking and limited constant evaluation are implemented.
A basic garbage collected runtime is provided with a partial set of data types.

There is no support for calling functions implemented in Rust or code generation.
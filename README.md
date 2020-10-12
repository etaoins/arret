# Arret

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Build status](https://badge.buildkite.com/bcda02e06b6795e669edae4264bdecbb11ff98b4f5afb1fa4b.svg?branch=master)](https://buildkite.com/arret/arret)

## Overview

Arret is pure functional, strongly typed language with Lisp-like syntax.
It aims to combine the expressiveness of Lisp with guarantees provided by functional programming.
The [language design documentation](./docs/language-design.md) has a high-level summary of the language's design choices.

The Arret compiler and parts of its standard library are written in Rust.
The mechanism for calling Rust code from Arret is referred to as the Rust Function Interface or RFI.
Documentation for the [`arret_runtime` crate](https://rustdoc.arret-lang.org/arret_runtime/index.html) describes the core concepts of the RFI.

## Installation

### Docker REPL Image

There is a public Docker image at [etaoins/arret:repl](https://cloud.docker.com/u/etaoins/repository/docker/etaoins/arret) that runs the Arret REPL.
Whenever `cargo run repl` appears in the documentation this command can be used instead:

```shell
> docker run -ti etaoins/arret:repl
```

It can also evaluate single file programs:

```shell
> cat hello-world.arret
(import [stdlib base])
(defn main! ()
  (println! "Hello, world!"))

> docker run -i etaoins/arret:repl eval - < hello-world.arret
Hello, world!
```

### Build Requirements

1. A Unix-like host running on ARM64, x86-64 or x86-32.
   These are the platforms supporting lazy compilation with LLVM's ORC JIT.
1. [LLVM](http://releases.llvm.org) 10 or 11
1. [Rust](https://www.rust-lang.org/en-US/)

### Building with rustup and Cargo

```shell
> curl https://sh.rustup.rs -sSf | sh
> cd ~/path/to/repo/root
> cargo run repl
```

## Usage

### REPL

The REPL provides an interactive environment for exploring Arret.
It's supported as a first class environment in Arret; the REPL is just as powerful as the compiler.

```text
> cargo run repl
arret> (length '(1 2 3 4 5))
=> 5
arret> (defn identity #{T} ([x T]) -> T x)
defined
arret> /type identity
=> (All #{T} T -> T)
arret> (identity "Hello, world!")
=> "Hello, world!"
arret> /type (identity [one two three])
=> (Vector 'one 'two 'three)
arret> /quit
```

### Compiler

Compiled programs have a `(main!)` function as their entry point:

```clojure
(import [stdlib base])

(defn main! ()
  (println! "Hello, world!"))
```

These can be compiled to a static binary by running Arret with the path name:

```sh
> cargo run compile hello-world.arret
> ./hello-world
"Hello, world!"
```

### Editors

A basic [Visual Studio Code](https://code.visualstudio.com) extension is bundled in [editors/code](./editors/code).
This uses the [Language Server](https://microsoft.github.io/language-server-protocol/) from the [lsp-server crate](./lsp-server).

```sh
# Install `arret-lsp-server`
cargo install --path lsp-server

# Install the Visual Studio code extension
cd editors/code
yarn
yarn vscode:install
```

## Examples

The Arret language is still rapidly evolving.
This makes it impractical to provide accurate documentation of the language and standard library.
However, the test programs in [run-pass](compiler/tests/run-pass) give examples of working Arret code.

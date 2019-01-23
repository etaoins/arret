# Arret

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Build status](https://badge.buildkite.com/bcda02e06b6795e669edae4264bdecbb11ff98b4f5afb1fa4b.svg?branch=master)](https://buildkite.com/arret/arret)

## Overview

Arret is pure functional, strongly typed language with Lisp-like syntax.
It aims to combine the expressiveness of Lisp with guarantees provided by functional programming.
Concurrency is provided through an Erlang-like model of actors communicating using message passing.

Arret is a successor to [Llambda](https://github.com/etaoins/llambda) without the constraint of Scheme compatibility.

## Installation

### Docker REPL Image

There is a public Docker image at [etaoins/arret-lang:repl](https://cloud.docker.com/u/etaoins/repository/docker/etaoins/arret-lang) that runs the Arret REPL. Whenever `cargo run repl` appears in the documentation this command can be used instead:

```shell
> docker run -ti etaoins/arret-lang:repl
```

It can also evaluate single file programs:

```shell
> cat hello-world.arret
(import [stdlib base])
(defn main! ()
  (println! "Hello, world!"))

> docker run -i etaoins/arret-lang:repl eval - < hello-world.arret
Hello, world!
```

### Build Requirementss

1. A Unix-like host running on ARM64, x86-64 or x86-32.
   These are the platforms supporting lazy compilation with LLVM's ORC JIT.
1. [LLVM 7.0](http://releases.llvm.org)
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
It's supported as a first class concept in Arret; the REPL is just as powerful as the compiler.

```text
> cargo run repl
arret> (length '(1 2 3 4 5))
=> 5
arret> (defn identity #{T} ([x T]) -> T x)
defined
arret> /type identity
=> #{T} (T -> T)
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
  (println! "Hello, world!")
  ())
```

These can be compiled to a static binary by running Arret with the path name:

```sh
> cargo run compile hello-world.arret
> ./hello-world
"Hello, world!"
```

## Examples

The Arret language is still rapidly evolving.
This makes it impractical to provide accurate documentation of the language and standard library.
However, the test programs in [eval-pass](compiler/tests/eval-pass) and [run-pass](compiler/tests/run-pass) give complex examples of working Arret code.
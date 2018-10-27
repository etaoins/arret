# Arret

[![Build Status](https://travis-ci.org/etaoins/arret.svg?branch=master)](https://travis-ci.org/etaoins/arret)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Overview

Arret is pure functional, strongly typed language with Lisp-like syntax.
It aims to combine the expressiveness of Lisp with guarantees provided by functional programming.
Concurrency is provided through an Erlang-like model of actors communicating using message passing.

Arret is a successor to [Llambda](https://github.com/etaoins/llambda) without the constraint of Scheme compatibility.

## Installation

### Requirementss

1. A Unix-like host running on ARM64, x86-64 or x86-32.
   These are the platforms supporting lazy compilation with LLVM's ORC JIT.
1. [LLVM 7.0](http://releases.llvm.org)
1. [Rust nightly](https://www.rust-lang.org/en-US/)
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

### REPL

The REPL provides an interactive environment for exploring Arret.
It's supported as a first class concept in Arret; the REPL is just as powerful as the compiler.

```text
> cargo +nightly run repl
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
> cargo +nightly run compile hello-world.arret
> ./hello-world
"Hello, world!"
```

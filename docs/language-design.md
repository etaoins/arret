# Language Design

## Introduction

Arret is a strongly typed, pure functional [Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)).
Its mandatory typing distinguishes it from most other Lisp dialects.
However, type inference and a relatively simple type system allow type declarations to be omitted in most places.

The base syntax is a subset of Clojure's [Extensible Data Notation](https://github.com/edn-format/edn).
Many tools that support EDN such as [cljfmt](https://github.com/weavejester/cljfmt) will work without modification on Arret source code.

Most of Arret's primitives and standard library functions are also modelled after [Clojure](https://clojure.org).
Clojure's functional design and mindshare make it a good source of inspiration.
However, Arret does not intend to be source compatible with Clojure.

All data structures are immutable and [strictly evaluated](https://en.wikipedia.org/wiki/Eager_evaluation).
This is similar to [Elm](https://elm-lang.org) or [PureScript](http://www.purescript.org).

In the text below the crystal ball (🔮) indicates planned features of the language.

## Basic Data Types

The basic data types are:

- `Int` is a signed 64bit integer.
- `Float` is a 64bit floating point value.
  This is known as a “double” in some other languages.
- `Num` is the union of `Int` and `Float`.
  This allows mathematical functions to be generic over number types.
  Specific numeric types should be used whenever possible to improve type inference and runtime performance.
- `Bool` is the union of the `true` and `false` types.
  Unlike most Lisps there is no concept of [truthy values](https://en.wikipedia.org/wiki/Truth_value#Computing);
  constructs such as `(if)` will only accept `true` or `false` values for their condition.
- `Str` is an immutable [UTF-8](https://en.wikipedia.org/wiki/UTF-8) string.
- `Sym` is an [interned](https://en.wikipedia.org/wiki/String_interning) symbol.
  These are used to represent both EDN symbols and identifiers.
  Additionally, every symbol has its own literal type (named `'foo`) that can be used to construct ad-hoc unions.
- `Char` is an [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value).

## Collections

The language provides immutable lists, vectors, maps and sets corresponding to the data types provided by EDN.
The `(List)`, `(Vector)`/`(Vectorof)`, `(Set)` and `(Map)` type constructors can be used to specify typed collections with a particular member type.

List types can specify zero or more fixed members followed by a uniform rest type.
For example, `(List Int Int & Float)` indicates a list of at least two `Int`s followed by zero or more `Float`s.
This is closely related to how function arguments are specified.

Lists are the primary data type.
Most collection functions are only provided for lists;
other collections need to be temporarily converted to lists to use them.
The compiler aggressively attempts to optimise these temporary lists away - this makes lists fill the role of [iterators](https://en.wikipedia.org/wiki/Iterator) in other languages.

## User Defined Types

Users can define their own types in two different ways:

1. `(deftype)` can be used to create an alias of an existing type.
2. The `(U)` type constructor can be used to define a union type.
   Type predicates can be used to determine which member type a given value has.

🔮 In the future developers will also be able to define record types.
These are also known as structs or product types in other languages.

## Functions

Arret functions take zero or more parameters and return a single value.
[Variadic](https://en.wikipedia.org/wiki/Variadic_function) functions are supported by using `& rest` to capture a list of the variable arguments.

By convention the empty list (`()` aka nil) is used to indicate no useful value is returned by the function.
This is used by functions that are only called for their side effects such as `(println!)`.

All functions are either impure or pure:

- Pure functions are declared using the `->` function arrow.
  They are not allowed to have side effects.
  The compiler may evaluate them at compile time, remove or duplicate calls to them, etc.
  Pure functions cannot call impure functions.
- Impure functions are declared using the `->!` function arrow.
  They are allowed to have side effects and will never be evaluated at compile time.
  By convention they should have a name ending with `!`.

It's also possible for functions to be [polymorphic](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)) on their purity.
For example, the higher-order functions `(map)` and `(filter)` are only impure if passed an impure function.
By convention these functions are named as if they were pure, i.e. without the `!` suffix.

## Destructuring

Arret supports [destructuring](https://en.wikipedia.org/wiki/Assignment_(computer_science)#Parallel_assignment) lists for variable assignments and function arguments.
This can be used to emulate multiple return values by returning a fixed sized list of values.
List destructures can be nested or use `& rest` syntax to capture the tail of a list.

Vector destructuring is unsupported as vector notation is already used for type annotations.
🔮 Record or map destructuring may be possible in the future.

## Macros

Arret provides a [hygienic](https://en.wikipedia.org/wiki/Hygienic_macro) macro system modelled after [Scheme R7RS](http://r7rs.org).
This is a powerful macro-by-example system that allows defining new language constructs and flow control patterns.
In fact, many language features such as `(defn)`, `(not)` and `(when)` are actually macros implemented on top of a small set of core primitives.

🔮 One of the goals of Arret is to allow an additional type of macro implemented by user provided functions.
These would be pure functions taking a syntax tree and input and returning the replacement syntax tree.

## Occurrence Typing

Arret requires that every value is known to have the correct type at compile time.
For example, a `Num` cannot be passed to a function expecting an `Int`.
This makes Arret [strongly typed](https://en.wikipedia.org/wiki/Strong_and_weak_typing) which distinguishes it from most Lisp dialects.

Occurrence typing is used to allow many idiomatic Lisp constructs to work with strong typing.
It collects implicit type information from type predicates (e.g. `(str?)` and `(int?)`) and equality comparisons on variables.
The variables' types are automatically refined based on if the predicate succeeded or failed.

This is inspired by similar features in [Typed Racket](https://docs.racket-lang.org/ts-guide/) and [TypeScript](https://www.typescriptlang.org).

## 🔮 Task & Actors

*This section is unimplemented. It's included to explain other design decisions.*

Tasks are the atomic unit of concurrency and fault isolation.
While Arret code cannot directly use threads it can spawn tasks which are run concurrently.
Tasks are scheduled by the runtime across the system's cores as [green threads](https://en.wikipedia.org/wiki/Green_threads).

If a task panics it will only terminate that task and allow the program to continue execution.
This is important as Arret intentionally doesn't implement exceptions, instead preferring error return values and panics.

An [actor system](https://en.wikipedia.org/wiki/Actor_model) is built on top of tasks.
Actors are tasks with the additional ability to send and receive messages.
They can keep internal state between messages which programs can use to implement a controlled form of mutable state.
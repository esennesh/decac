Deca is a language designed to provide the advanced features of sophisticated, high-level programming languages while still programming as close as possible to the bare metal.  It brings in the functional, object-oriented, and generic programming paradigms without requiring a garbage collector or a threading system, so programmers really only pay in performance for the features they use.

Deca provides:
  * [Variants](SumTypes.md)
  * Tuples
  * Tail-call optimization (via LLVM)
  * [Static type inference](StrongStaticTyping.md) that takes subtyping into account
  * Universally-quantified types (aka generics or templates)
  * [Existentially-quantified types](ExistentialTypes.md) (aka abstract data-types, first-class modules)
  * [Unboxed data representation](UnboxedDataTypes.md)
  * A strong region-and-effect system that prohibits unsafe escaping pointers and double-free errors
  * Mutability control, with mutability subtyping to distinguish between alias-safe and alias-unsafe immutability
  * Strict evaluation
  * Compilation to [LLVM](http://llvm.org) assembly language, and thence to bare-metal binary code.

Deca will provide:
  * UTF-8 strings
  * First-class functions, including [lexical closures and lambdas](ExistentialTypes.md)
  * Pattern matching
  * A CLOS-style [object system](MinimalRecords.md) with multimethods and their overrides operating on variants
  * Exceptions and exception handling (really just a special case of variants with some LLVM intrinsics to pass them up the stack)
  * Scoped implicit parameters (for type-based dispatch as in type-classes)
  * Hierarchical modules
  * Binary compatibility for linking to libraries written in C

Deca will **not** include:
  * Binary compatibility with C++
  * Pointer arithmetic  (except through [bit-casts](StrongStaticTyping.md) to integers and back)
  * Lispy macros or syntax
  * Lispy or Smalltalkish image-based execution
  * Lazy evaluation (users can create their own thunks)

Deca will never _require_:
  * Garbage collection
  * A virtual machine of any kind
  * A run-time library or class library
  * An operating system to run atop
  * An extensive knowledge of category theory.

There are some simple [code examples](https://code.google.com/p/decac/source/browse/examples) in the repository.  They currently demonstrate some rather simple use of the type system and pattern matching by writing out an implementation of cons cells and some basic higher-order functions dealing with list processing.  A malloc implementation is coming soon.
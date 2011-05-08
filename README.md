## An aggressively optimizing compiler for the VL language

### Overview

The name VL is derived from VLAD, which is a higher-order functional
programming language that supports automatic differentiation (AD); see
[1] for more details.  VL stands for VLAD without AD.  The compiler
uses polyvariant union-free flow analysis to compile VL to efficient,
Fortran-like C code.  This program is meant to be expository and not
necessarily efficient.  I've written it in order to understand
polyvariant union-free flow analysis described in [1].  The name VL
was coined by [Alexey Radul](http://web.mit.edu/~axch/www/), who wrote
a VL to Scheme compiler in Scheme and suggested that I repeat this
exercise in Haskell for comparison.  I'm releasing the code, so that I
can reference it, and hoping that somebody may find it useful,
although I'm not planning to develop it any further.  Still, if you
spot bugs or have improvement suggestions, I'd be glad to hear from
you.

### Dependencies

* template-haskell-2.5.0.0 (_not_ 2.4.0.0, which has a slightly
  different API)

* [test-framework](http://batterseapower.github.com/test-framework/)
  for running the evaluator test suite

### Compiling

To compile the VL to C compiler, run

    make vl2c

### Testing

To test both the concrete and abstract evaluator, run

    make test-evaluator

To add tests to the evaluator test suite, edit `Test/Test.hs`.

To test the compiler, run

    make test-compiler

To add a test to the compiler test suite, add a file with extension
`.vl` to the directory `Test/vl` containing the VL code of the program
and put the expected value in a comment in the last line of the file.

### Bibliography

[1] Jeffrey Siskind and Barak Pearlmutter, _Using Polyvariant
Union-Free Flow Analysis to Compile a Higher-Order Functional
Programming Language with a First-Class Derivative Operator to
Efficient Fortran-like Code_, Purdue University ECE Technical Report,
2008, [http://docs.lib.purdue.edu/ecetr/367](http://docs.lib.purdue.edu/ecetr/367).

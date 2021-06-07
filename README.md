## An aggressively optimizing compiler for the VL language

### Overview

The name VL derives from VLAD, which is a higher-order functional
programming language that supports automatic differentiation (AD); see
[1] for more details.  VL stands for VLAD without AD.

The compiler uses polyvariant union-free flow analysis to compile VL
to an efficient, FORTRAN-like C code.  This program is meant to be
expository and not necessarily efficient.  I've written it in order to
understand polyvariant union-free flow analysis described in [1].

The name VL was coined by [Alexey Radul](http://alexey.radul.name),
who wrote a VL to Scheme compiler in Scheme and suggested that I
repeat this exercise in Haskell for comparison.  I'm releasing the
code, so that I can reference it, and hoping that somebody may find it
useful, although I'm not planning to develop it any further.  Still,
if you spot bugs or have improvement suggestions, I'd be glad to hear
from you.

### Compiling

To compile the VL to C compiler, run

	stack build

### Testing

To build and run the test suites, run:

	stack test

The `package.yaml` file defines two test suites: `test-evaluator`,
which tests both the concrete and abstract evaluator, and
`test-compiler`, which tests the compiler.  To add more tests to the
evaluator test suite, edit the source file `test/test-evaluator.hs`.
To add a test to the compiler test suite, drop a file with the
extension `.vl` containing the VL source code of the program into the
directory `test/vl` and put the expected value in a comment in the
last line of that file.

### Bibliography

[1] Jeffrey Siskind and Barak Pearlmutter, _Using Polyvariant
Union-Free Flow Analysis to Compile a Higher-Order Functional
Programming Language with a First-Class Derivative Operator to
Efficient FORTRAN-like Code_, Purdue University ECE Technical Report,
2008, [http://docs.lib.purdue.edu/ecetr/367](http://docs.lib.purdue.edu/ecetr/367).

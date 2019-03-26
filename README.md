# jcompiler

[![Crates.io](https://img.shields.io/crates/v/jcompiler.svg?color=green)](https://crates.io/crates/jcompiler)
[![Build Status](https://travis-ci.com/mattjquinn/jcompiler.svg?branch=master)](https://travis-ci.com/mattjquinn/jcompiler)
[![Coverage Status](https://coveralls.io/repos/github/mattjquinn/jcompiler/badge.svg?branch=master)](https://coveralls.io/github/mattjquinn/jcompiler)
[![lines of code](https://tokei.rs/b1/github/mattjquinn/jcompiler)](https://github.com/Aaronepower/tokei)

### A compiler for the J programming language.

## Documentation

The latest API reference for the master branch is [available here](https://mattjquinn.github.io/jcompiler/master/jcompiler/index.html).

## Usage

You will need LLVM 7 and Rust installed.

    $ cargo build --release

You can then compile and run programs as follows:

```
$ target/release/jcompiler j_examples/test.ijs
$ ./test
Hello World!
```

By default, jcompiler compiles programs to executables that run on the
current machine. You can explicitly specify architecture using LLVM
target triples:

```
$ target/release/jcompiler j_examples/test.ijs --target=x86_64-pc-linux-gnu
```

### Running tests

```
$ cargo test
```

### TODO
* Fix segfault when an identifier is used without prior declaration.
* Write more parser tests.
* Reorganize llvm.rs, make as much as possible private (i.e. global_idents map on Module struct)
* Handle divide by zero
* Add tests for dyadic Power, Residue, Reciprocal verbs (ints + doubles like that for dyadic %).
* Experiment with definining runtime library in Rust
  on branch `with-rust-runtime` (and possibly use Cargo's
  workspace feature to contain both crates such that they
  can share definitions.)
* Print large numbers using scientific notation.
* Replace exit() calls in c funcs with handler that cleans up.
* Add a macro to compiler_tests.rs that eliminates duplication of that code.
* Move simpler definitions back to LLVM to expand their test coverage
  (dump IR to ensure correct translation).
* Allow verbs to be standalone expressions by themselves (printing them should give
  their textual representation just as ijconsole does).
* Add tests for illegal array operations (i.e., 1 2 3 + 4 5); error
  message should be same as ijconsole (i.e., "length error")
* READ: APL: A Glimpse of Heaven: https://news.ycombinator.com/item?id=19325361
* READ: K7 Tutorial: https://news.ycombinator.com/item?id=19418570
* Only declare J library functions that are actually used by the program being compiled.
* Next: for dynamic expressions, implement J's random fn using C's srand(seed)
* Improve parser/compiler error messages (especially using `ansi_term` crate).
* Thoroughly document library code using inline rustdoc comments, publicly host at accessible URL.
* Add precompiled binaries for each tagged release in GitHub.
* Add initial entry to CHANGELOG.md for 0.1.0.
* Compartmentalize LLVM state so as to allow concurrent tests and concurrent compilations in general.

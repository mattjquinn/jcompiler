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
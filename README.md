# jcompiler

[![CircleCI](https://circleci.com/gh/mattjquinn/jcompiler.svg?style=shield)](https://circleci.com/gh/mattjquinn/jcompiler)
[![Crates.io](https://img.shields.io/crates/v/jcompiler.svg?color=green)](https://crates.io/crates/jcompiler)

A compiler for [the J array programming language](https://www.jsoftware.com).

## Getting Started

To build jCompiler, you will need:
* a stable version of Rust
* LLVM 10 and Clang 10 (see [scripts/install-llvm-backend-dependencies.sh](scripts/install-llvm-backend-dependencies.sh))

To develop jCompiler, you will want to run the tests, which introduce additional dependencies on:
* `ijconsole`, the J language interpreter (see [scripts/install-common-dependencies.sh](scripts/install-common-dependencies.sh))
* Linaro, to cross-assemble and link binaries on an x86_64 development machine, and `qemu` to run them (see [scripts/install-arm-backend-dependencies.sh](scripts/install-arm-backend-dependencies.sh))

The [CircleCI job definitions in this repo](.circleci/config.yml) invoke the scripts mentioned above
when preparing test environments, so they are the authoritative resources as to what exactly is required.

## Using the LLVM Backend

```
$ cargo build --release
$ target/release/jcompiler --backend=llvm <jfile>.ijs
$ ./<jfile>
```

By default, LLVM use the target architecture of the host machine.
You can explicitly choose a target architecture using LLVM target triples:

```
$ target/release/jcompiler <jfile>.ijs --target=x86_64-pc-linux-gnu
```

## Using the ARM Backend

```
$ cargo build --release
$ target/release/jcompiler --backend=arm <jfile>.ijs
$ ./<jfile>
```

Note: the output emitted by this backend most closely matches
the godbolt.org configuration "ARM gcc 7.2.1 (none)" with options "-O0 -mfloat-abi=hard".

## Examples

You can find many examples of J programs in the `jlang_programs` directory.
As one example, compiling this J program:

```j
'Some array operations...'
*: 1 2 3 4
matrix =: 2 3 $ 5 + 2 3 4 5 6 7
10 * matrix
1 + 10 20 30
1 2 3 + 10
residues =: 2 | 0 1 2 3 4 5 6 7
residues
```

with jcompiler and running the compiled binary will yield the following on stdout:

```j
Some array operations...
1 4 9 16
 70  80  90
100 110 120
11 21 31
11 12 13
0 1 0 1 0 1 0 1
```

## Documentation

The API reference for the master branch is
[available here](https://mattjquinn.github.io/jcompiler/master/jcompiler/index.html),
but is out-of-date and incomplete at the moment.

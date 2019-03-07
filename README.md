# jcompiler
### A compiler for the J programming language.

## Usage

You will need LLVM and Rust installed.

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
* Replace `integration_tests.sh` with integration tests alongside `src` per Rust book.
* Rewrite `.travis.yml` to build, test, and run `integration_tests.sh`.
* Improve parser/compiler error messages (especially using `ansi_term` crate).
* Thoroughly document library code using inline rustdoc comments.
* Add `cargo clippy` to Travis build (see GitHub README.md for clippy).
* Builds should fail if `rustfmt` finds unformatted source file (see README.md for `rustfmt`).
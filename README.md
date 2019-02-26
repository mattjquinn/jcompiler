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

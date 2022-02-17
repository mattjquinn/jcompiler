# TODOs

## CI/CD
* reinstate publishing of code coverage; use codecov.io instead of coveralls; publish pill status on README.md
* run `cargo doc`, publish results somewhere.
* run `cargo bench` to compare optimized vs unoptimized, publish results somewhere.
* split the LLVM target further into x86_64 and arm

## Other
- TODO: implement monadic reciprocal, where we can call the newly centralized load(D0) function shared by print()
- TODO: make BasicBlock respect ARM calling conventions; if we use anything above r3 push it to stack
- TODO: Store doubles as 8 contiguous bytes on stack, use ldmia to load them contiguously

- Solve all ctest_monadic_ tests that aren't passing
- Use movw and movt to replace shifting when loading double precision constants:
  https://stackoverflow.com/questions/10261300/invalid-constant-after-fixup

- Use LDMIA to load doubles into r2-r3 when printing, rather than two separate LDR instructions
- Make BasicBlock assert that all registers have been freed before
  providing its finalized list of instructions to the ASM writer
- Don't assign registers directly, have a function do so per-scope
* TODO: modify GlobalContext to support arrays of ints, currently only supports single ints
* add README note for LLVM backend to simplify by using more LLVM IR, i.e. mul, add, etc
* all load/stores should be delegated to Offset impl, which should choose appropriate ARM instructions based on actual offset involved
* figure out why I need that one test is blowing the immediate width; use godbolt.org to compare
* TODO: Label basic blocks with comments
* TODO: Make registers an enum
* TODO: Use a macro to reduce verbosity of instructions.

## Reminders
* workflow: Create issue, create PR, review, then merge.
* experiment: interchange PEG with parser combinator
* AFL fuzzer, Coq proofs for optimizations/transformations,
* memory usage assertions/proofs, profiling/debugging, kernel/net interactions,
  performance tuning/optimizations (the lower the level, the better)

## Related / Of Interest
* KeRF: https://github.com/kevinlawler/kerf
* APL amuse-bouches: https://vector.org.uk/sixteen-apl-amuse-bouches/
* The document "Mapping High Level Constructs to LLVM IR" in this repository.
* Next logical iteration of array languages: https://news.ycombinator.com/item?id=22579781


# TODOs

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

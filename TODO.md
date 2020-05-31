# TODOs

* NEXT: Enable final (commented out) test in ARM, support static arrays
   * requires moving the r3 load out of GlobalVarAssgmt to the places where it's expected, i.e. DyadicOp handling
   * modify GlobalContext to support arrays of ints, currently only supports single ints
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

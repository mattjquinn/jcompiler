struct JVal {
  char type;  // the value's type, as J defines it.
  char loc;   // the value's location in memory (stack, heap, global, etc.)
  int rank;   // number of dimensions: 0 for scalars, 1 for lists, 2 for tables, etc.
  //int* shape; // list of dimensions: [] for scalars, [a] for lists, [a b] for tables, etc.
  void* ptr;   // a pointer to the value
};

enum JValType {
  JIntegerType = 1,
  JArrayType = 2,
  JDoublePrecisionFloatType = 3,
  JStringType = 4,
};

enum JValLocation {
    JLocStack = 1,
    JLocHeapLocal = 2,
    JLocHeapGlobal = 3,
};

enum JDyadicVerb {
  JPlusOp = 1,
  JTimesOp = 2,
  JLessThanOp = 3,
  JLargerThanOp = 4,
  JEqualOp = 5,
  JMinusOp = 6,
  JDivideOp = 7,
  JPowerOp = 8,
  JResidueOp = 9,
  JCopyOp = 10,
  JLargerOfOp = 11,
  JLargerOrEqual = 12,
};

enum JMonadicVerb {
  JIncrementOp = 1,
  JSquareOp = 2,
  JNegateOp = 3,
  JReciprocalOp = 4,
  JTallyOp = 5,
  JCeilingOp = 6,
};


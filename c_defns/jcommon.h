struct JVal {
  char type;  // the value's type, as J defines it.
  char loc;   // the value's location in memory (stack, heap, global, etc.)
  int rank;   // number of dimensions: 0 for scalars, 1 for lists, 2 for tables, etc.
  int* shape; // list of dimensions: [] for scalars, [a] for lists, [a b] for tables, etc.
  void* ptr;   // a pointer to the value
  struct JVal* shape_fut;  // TODO: Replace shape above with this one.
};

enum JValType {
  JIntegerType = 1,
  // TODO: Get rid of this array type, replace with NDimensional
  JArrayType = 2,
  JDoublePrecisionFloatType = 3,
  JStringType = 4,
  JArrayNDimensionalType = 5,
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
  JShapeOp = 13,
};

enum JMonadicVerb {
  JIncrementOp = 1,
  JSquareOp = 2,
  JNegateOp = 3,
  JReciprocalOp = 4,
  JTallyOp = 5,
  JCeilingOp = 6,
};


struct JVal* jdyad_internal_copy_verb(struct JVal* lhs, struct JVal* rhs);
struct JVal* jdyad_internal_shape_verb(struct JVal* lhs, struct JVal* rhs);
struct JVal* jdyad_internal_numeric_with_array(enum JDyadicVerb op,
                                               struct JVal* numeric,
                                               struct JVal* arr,
                                               bool is_numeric_lhs);
struct JVal* jval_heapalloc_array_dim1(int length);
struct JVal* jval_heapalloc_array_dim_n(struct JVal* shape_arr);
struct JVal* jval_heapalloc_int();
struct JVal* jval_heapalloc_double();
void jval_drop(struct JVal* jval, bool do_drop_globals);
struct JVal* jval_clone(struct JVal* jval, enum JValLocation loc);
struct JVal* jreduce(enum JDyadicVerb verb, struct JVal* expr);

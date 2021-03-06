struct JVal {
  char type;    // the value's type, as J defines it.
  char typaram; // an optional type parameter, i.e. numeric array, string array, etc.
  char loc;     // the value's location in memory (stack, heap, global, etc.)
  int rank;     // number of dimensions: 0 for scalars, 1 for lists, 2 for tables, etc.
  int* shape;   // list of dimensions: [] for scalars, [a] for lists, [a b] for tables, etc.
  void* ptr;    // a pointer to the value
};

enum JValType {
  JIntegerType = 1,
  JDoublePrecisionFloatType = 2,
  JCharacterType = 3,
  JArrayNDimensionalType = 4,
};

enum JValTypeParam {
  JNumeric = 1,
  JString = 2,
};

enum JValLocation {
    JLocStack = 1,
    JLocHeapLocal = 2,
    JLocHeapGlobal = 3,
};

enum JDyadicVerb {
  JPlus = 1,
  JTimes = 2,
  JLessThan = 3,
  JLargerThan = 4,
  JEqual = 5,
  JMinus = 6,
  JDivide = 7,
  JPower = 8,
  JResidue = 9,
  JCopy = 10,
  JLargerOf = 11,
  JLargerOrEqual = 12,
  JShape = 13,
  JAppend = 14,
};

enum JMonadicVerb {
  JIncrement = 1,
  JSquare = 2,
  JNegate = 3,
  JReciprocal = 4,
  JTally = 5,
  JCeiling = 6,
  JShapeOf = 7,
};


struct JVal* jdyad_internal_copy_verb(struct JVal* lhs, struct JVal* rhs);
struct JVal* jdyad_internal_shape_verb(struct JVal* lhs, struct JVal* rhs);
struct JVal* jdyad_internal_numeric_with_ndim_array(enum JDyadicVerb op,
                                                    struct JVal* numeric,
                                                    struct JVal* arr,
                                                    bool is_numeric_lhs);
struct JVal* jdyad_internal_numeric_with_array(enum JDyadicVerb op,
                                               struct JVal* numeric,
                                               struct JVal* arr,
                                               bool is_numeric_lhs);
struct JVal* jval_heapalloc_array_dim1(int length);
struct JVal* jval_heapalloc_array_dim_n(int rank, int* shape);
struct JVal* jval_heapalloc_int();
struct JVal* jval_heapalloc_double();
void jval_drop(struct JVal* jval, bool do_drop_globals);
struct JVal* jval_clone(struct JVal* jval, enum JValLocation loc);
struct JVal* jreduce(enum JDyadicVerb verb, struct JVal* expr);
void jprint(struct JVal* val, bool newline);
int* heapalloc_ndim_col_widths_arr(int length);
void heapfree_ndim_col_widths_arr(int* arr, int length);
bool jinternal_same_rank_and_shape(struct JVal* lhs, struct JVal* rhs);
int jarray_length(struct JVal* jval);
struct JVal* jval_heapalloc_array_dim_n_nonprimshape(int rank, struct JVal* shape_arr);
int* jvalarray_as_primarray(struct JVal* arr);
struct JVal* jdyad_internal_append_verb(struct JVal* lhs, struct JVal* rhs);

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <stdbool.h>

extern int heap_jval_alloc_counter;

// NOTE: To get LLVM IR of this source, run
// $ clang-7 -S jlib.c -emit-llvm -o -  (note the trailing hyphen)

// Default print precision is 6; can be changed within J.
static int PRINT_PRECISION = 6;

struct JVal {
  char type;  // the value's type, as J defines it.
  int len;    // the number of elements pointed to by ptr
              // - for scalars: 1
              // - for arrays: the length of the array
  void* ptr;   // a pointer to the value
};

enum JValType {
  JIntegerType = 1,
  JArrayType = 2,
  JDoublePrecisionFloatType = 3,
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
};

enum JMonadicVerb {
  JIncrementOp = 1,
  JSquareOp = 2,
  JNegateOp = 3,
  JReciprocalOp = 4,
  JTallyOp = 5,
};

struct JVal* jdyad_internal_copy_verb(struct JVal* lhs, struct JVal* rhs);
struct JVal* jdyad_internal_numeric_with_array(enum JDyadicVerb op,
                                               struct JVal* numeric,
                                               struct JVal* arr,
                                               bool is_numeric_lhs);
struct JVal* jval_heapalloc(enum JValType type, int len);


void jprint(struct JVal* val, bool newline) {
  struct JVal** jvals;
  int* iptr;
  double* dptr;

  switch (val->type) {
    case JIntegerType:
      iptr = (int*) val->ptr;
      if (*iptr < 0)         { printf("_%d", abs(*iptr)); }
      else                   { printf("%d", *iptr);       }
      break;
    case JDoublePrecisionFloatType:
      dptr = (double*) val->ptr;
      if (signbit(*dptr) && *dptr != -0.0) { printf("_%.*g", PRINT_PRECISION, fabs(*dptr)); }
      else                                 { printf("%.*g", PRINT_PRECISION, fabs(*dptr)); }
      break;
    case JArrayType:
      jvals = val->ptr;
      for (int i = 0; i < val->len; i++) {
        jprint(jvals[i], false);
        if (i < (val->len - 1)) { printf(" "); }
      }
      break;
    default:
      printf("ERROR: jprint: unsupported JVal (type:%d, len:%d)\n", val->type, val->len);
      exit(EXIT_FAILURE);
  }

  if (newline) { printf("\n"); }
}

void jexpand(struct JVal** arr, struct JVal* val, int dest_idx) {
  arr[dest_idx] = val;
}

struct JVal* jdyad(enum JDyadicVerb op, struct JVal* lhs, struct JVal* rhs) {
    struct JVal* ret;
    struct JVal** jvals_a;
    struct JVal** jvals_b;
    struct JVal** jvals_out;
    int* iptr;
    double* dptr;
    int lhsi, rhsi;
    double lhsd, rhsd;

    if (op == JCopyOp) {
        return jdyad_internal_copy_verb(lhs, rhs);
    }

    if (lhs->type == JIntegerType && rhs->type == JIntegerType) {
        lhsi = *((int*) lhs->ptr);
        rhsi = *((int*) rhs->ptr);

        switch(op) {
            case JPlusOp:
                ret = jval_heapalloc(JIntegerType, 1);
                *(int*)ret->ptr = lhsi + rhsi;
                return ret;
            case JMinusOp:
                ret = jval_heapalloc(JIntegerType, 1);
                *(int*)ret->ptr = lhsi - rhsi;
                return ret;
            case JTimesOp:
                ret = jval_heapalloc(JIntegerType, 1);
                *(int*)ret->ptr = lhsi * rhsi;
                return ret;
            case JDivideOp:
                // Division always produces a double.
                ret = jval_heapalloc(JDoublePrecisionFloatType, 1);
                *(double*)ret->ptr = (double) lhsi / (double) rhsi;
                return ret;
            case JPowerOp:
                ret = jval_heapalloc(JIntegerType, 1);
                *(int*)ret->ptr = pow(lhsi, rhsi);
                return ret;
            case JLessThanOp:
                ret = jval_heapalloc(JIntegerType, 1);
                *(int*)ret->ptr = (lhsi < rhsi) ? 1 : 0;
                return ret;
            case JLargerThanOp:
                ret = jval_heapalloc(JIntegerType, 1);
                *(int*)ret->ptr = (lhsi > rhsi) ? 1 : 0;
                return ret;
            case JEqualOp:
                ret = jval_heapalloc(JIntegerType, 1);
                *(int*)ret->ptr = (lhsi == rhsi) ? 1 : 0;
                return ret;
            case JResidueOp:
                ret = jval_heapalloc(JIntegerType, 1);
                // IMPORTANT: In J the operands to Residue are opposite of "%";
                // i.e., 5 | 2 means 2 % 5
                *(int*)ret->ptr = (rhsi % lhsi);
                return ret;
            default:
                printf("ERROR: jdyad: unsupported op: %d for types lhs:%d, rhs:%d\n",
                    op, lhs->type, rhs->type);
                exit(EXIT_FAILURE);
        }
    } else if (   (lhs->type == JDoublePrecisionFloatType && rhs->type == JIntegerType)
               || (lhs->type == JIntegerType              && rhs->type == JDoublePrecisionFloatType)
               || (lhs->type == JDoublePrecisionFloatType && rhs->type == JDoublePrecisionFloatType)) {

        switch (lhs->type) {
            case JDoublePrecisionFloatType:
                lhsd = *((double*) lhs->ptr);
                break;
            case JIntegerType:
                lhsd = (double) *((int*) lhs->ptr);
                break;
        }

        switch (rhs->type) {
            case JDoublePrecisionFloatType:
                rhsd = *((double*) rhs->ptr);
                break;
            case JIntegerType:
                rhsd = (double) *((int*) rhs->ptr);
                break;
        }

        switch(op) {
            case JPlusOp:
                ret = jval_heapalloc(JDoublePrecisionFloatType, 1);
                *(double*)ret->ptr = lhsd + rhsd;
                return ret;
            case JMinusOp:
                ret = jval_heapalloc(JDoublePrecisionFloatType, 1);
                *(double*)ret->ptr = lhsd - rhsd;
                return ret;
            case JTimesOp:
                ret = jval_heapalloc(JDoublePrecisionFloatType, 1);
                *(double*)ret->ptr = lhsd * rhsd;
                return ret;
            case JDivideOp:
                ret = jval_heapalloc(JDoublePrecisionFloatType, 1);
                *(double*)ret->ptr = lhsd / rhsd;
                return ret;
            case JLessThanOp:
                ret = jval_heapalloc(JIntegerType, 1);
                *(int*)ret->ptr = (lhsd < rhsd) ? 1 : 0;
                return ret;
            case JLargerThanOp:
                ret = jval_heapalloc(JIntegerType, 1);
                *(int*)ret->ptr = (lhsd > rhsd) ? 1 : 0;
                return ret;
            case JEqualOp:
                ret = jval_heapalloc(JIntegerType, 1);
                *(int*)ret->ptr = (lhsd == rhsd) ? 1 : 0;
                return ret;
            default:
                printf("ERROR: jdyad: unsupported op: %d for types lhs:%d, rhs:%d\n",
                    op, lhs->type, rhs->type);
                exit(EXIT_FAILURE);
        }
    } else if ( (lhs->type == JIntegerType || lhs->type == JDoublePrecisionFloatType)
               && rhs->type == JArrayType) {

        return jdyad_internal_numeric_with_array(op, lhs, rhs, true);

    } else if ( (rhs->type == JIntegerType || rhs->type == JDoublePrecisionFloatType)
               && lhs->type == JArrayType) {

        return jdyad_internal_numeric_with_array(op, rhs, lhs, false);

    } else if (lhs->type == JArrayType && rhs->type == JArrayType) {

        // Array lengths must be the same.
        if (lhs->len != rhs->len) {
            printf("ERROR: jdyad: length mismatch: lhs (type:%d,len:%d), rhs (type:%d,len:%d)\n",
                lhs->type, lhs->len, rhs->type, rhs->len);
            exit(EXIT_FAILURE);
        }

        jvals_a = lhs->ptr;
        jvals_b = rhs->ptr;
        ret = jval_heapalloc(JArrayType, lhs->len);
        jvals_out = (struct JVal**)ret->ptr;

        for (int i = 0; i < lhs->len; i++) {
            jvals_out[i] = jdyad(op, jvals_a[i], jvals_b[i]);
        }

        return ret;
    }

    printf("ERROR: jdyad: unsupported lhs (type:%d,len:%d) and rhs (type:%d,len:%d)\n",
        lhs->type, lhs->len, rhs->type, rhs->len);
    exit(EXIT_FAILURE);
}

struct JVal* jdyad_internal_copy_verb(struct JVal* lhs, struct JVal* rhs) {
    struct JVal* ret;
    struct JVal* intermediate;
    int* iptr;
    int lhsi, rhsi;
    struct JVal** jvals_out;

    if (lhs->type == JIntegerType && rhs->type == JIntegerType) {
        lhsi = *((int*) lhs->ptr);
        rhsi = *((int*) rhs->ptr);

        ret = jval_heapalloc(JArrayType, lhsi);
        jvals_out = (struct JVal**)ret->ptr;

        // Copies the rhs value "n" times, where n is lhs value.
        for (int i = 0; i < lhsi; i++) {
            intermediate = jval_heapalloc(JIntegerType, 1);
            *(int*)intermediate->ptr = rhsi;
            jvals_out[i] = intermediate;
        }

        return ret;

    } else if (lhs->type == JIntegerType && rhs->type == JArrayType) {
        printf("ERROR: jdyad: copy: support int with array\n");
        exit(EXIT_FAILURE);

    } else if (lhs->type == JArrayType && rhs->type == JIntegerType) {
        printf("ERROR: jdyad: copy: support array with int\n");
        exit(EXIT_FAILURE);

    } else if (lhs->type == JArrayType && rhs->type == JArrayType) {
        printf("ERROR: jdyad: copy: support array with array\n");
        exit(EXIT_FAILURE);

    } else {
        printf("ERROR: jdyad: copy: unsupported lhs (type:%d,len:%d) and rhs (type:%d,len:%d)\n",
            lhs->type, lhs->len, rhs->type, rhs->len);
        exit(EXIT_FAILURE);
    }
}

struct JVal* jdyad_internal_numeric_with_array(enum JDyadicVerb op,
                                               struct JVal* numeric,
                                               struct JVal* arr,
                                               bool is_numeric_lhs) {
    struct JVal* ret;
    struct JVal** jvals;
    struct JVal** jvals_out;

    jvals = arr->ptr;
    ret = jval_heapalloc(JArrayType, arr->len);
    jvals_out = (struct JVal**)ret->ptr;

    for (int i = 0; i < arr->len; i++) {
        // Must preserve order of operands as some operations are not commutative.
        if (is_numeric_lhs)     { jvals_out[i] = jdyad(op, numeric, jvals[i]); }
        else                    { jvals_out[i] = jdyad(op, jvals[i], numeric); }
    }

    return ret;
}

struct JVal* jmonad(enum JMonadicVerb op, struct JVal* expr) {
    struct JVal** jvals_in;
    struct JVal** jvals_out;
    struct JVal* ret;
    int *iptr;
    double *dptr;
    int expri;
    double exprd;

    switch (expr->type) {
        case JIntegerType:
            expri = *((int*) expr->ptr);

            switch (op) {
                case JIncrementOp:
                    ret = jval_heapalloc(JIntegerType, 1);
                    *(int*)ret->ptr = expri + 1;
                    return ret;
                case JNegateOp:
                    ret = jval_heapalloc(JIntegerType, 1);
                    *(int*)ret->ptr = expri * -1;
                    return ret;
                case JSquareOp:
                    ret = jval_heapalloc(JIntegerType, 1);
                    *(int*)ret->ptr = pow(expri, 2);
                    return ret;
                case JReciprocalOp:
                    ret = jval_heapalloc(JDoublePrecisionFloatType, 1);
                    *(double*)ret->ptr = 1.0 / (float) expri;
                    return ret;
                case JTallyOp:
                    ret = jval_heapalloc(JIntegerType, 1);
                    *(int*)ret->ptr = expr->len;
                    return ret;
                default:
                    printf("ERROR: jmonad: unsupported verb on type integer: %d\n", op);
                    exit(EXIT_FAILURE);
            }

        case JDoublePrecisionFloatType:
            exprd = *((double*) expr->ptr);
            ret = jval_heapalloc(JDoublePrecisionFloatType, 1);

            switch (op) {
                case JIncrementOp:
                    *(double*)ret->ptr = exprd + 1.0;
                    return ret;
                case JNegateOp:
                    *(double*)ret->ptr = exprd * -1.0;
                    return ret;
                case JSquareOp:
                    *(double*)ret->ptr = pow(exprd, 2.0);
                    return ret;
                default:
                    printf("ERROR: jmonad: unsupported verb on type double: %d\n", op);
                    exit(EXIT_FAILURE);
            }

        case JArrayType:

            if (op == JTallyOp) {
                // The tally monad is not distributed over arrays;
                // return the length of the array itself.
                ret = jval_heapalloc(JIntegerType, 1);
                *(int*)ret->ptr = expr->len;
                return ret;
            }

            jvals_in = expr->ptr;
            ret = jval_heapalloc(JArrayType, expr->len);
            jvals_out = (struct JVal**)ret->ptr;

            for (int i = 0; i < expr->len; i++) {
                jvals_out[i] = jmonad(op, jvals_in[i]);
            }

            return ret;
        default:
            printf("ERROR: jmonad: unsupported expr (type:%d,len:%d)\n",
                expr->type, expr->len);
            exit(EXIT_FAILURE);
    }
}

struct JVal* jreduce(enum JDyadicVerb verb, struct JVal* expr) {

    // Reducing over a zero length or single expression has no effect.
    if (expr->len < 2) { return expr; }

    struct JVal** jvals_in;
    struct JVal* lhs;
    struct JVal* rhs;
    struct JVal* ret;
    struct JVal* intermediate;
    int i;

    switch (expr->type) {
        case JArrayType:
            // Reduce always results in a single value.
            // Take each difference from right to left as
            // the left hand side of each difference is accumulated in
            // the output array (remember: some operations like
            // subtraction are not commutative).
            jvals_in = expr->ptr;
            i = expr->len - 2;
            ret = jvals_in[i + 1];
            do {
                // TODO: Free the intermediate values that are
                // returned by jdyad but never used beyond this function.
                ret = jdyad(verb, jvals_in[i], ret);
                i -= 1;
            } while (i >= 0);
            return ret;

        default:
            printf("ERROR: jreduce: unsupported expr (type:%d,len:%d)\n",
                expr->type, expr->len);
            exit(EXIT_FAILURE);
    }
}

// All heap memory allocations must be done using this function.
struct JVal* jval_heapalloc(enum JValType type, int len) {
    struct JVal* jval;
    struct JVal** jvals;
    int* iptr;
    double* dptr;

    // Allocate space for the JVal itself.
    jval = malloc(sizeof(struct JVal));
    if (!jval) {
        printf("ERROR: jval_heapalloc: call to malloc new JVal failed.");
        exit(EXIT_FAILURE);
    }
    heap_jval_alloc_counter += 1;

    jval->len = len;
    jval->type = type;

    // Allocate space for the type instance pointed to.
    switch (type) {
        case JIntegerType:
            iptr = malloc(len * sizeof(int));
            if (!iptr) {
                printf("ERROR: jval_heapalloc: call to malloc %d new int(s) failed.", len);
                exit(EXIT_FAILURE);
            }
            jval->ptr = iptr;
            return jval;
        case JDoublePrecisionFloatType:
            dptr = malloc(len * sizeof(double));
            if (!dptr) {
                printf("ERROR: jval_heapalloc: call to malloc %d new double(s) failed.", len);
                exit(EXIT_FAILURE);
            }
            jval->ptr = dptr;
            return jval;
        case JArrayType:
            // REMEMBER: This is just allocating space for _pointers_ to the JVals
            // that will eventually comprise the array, not the actual JVals themselves.
            jvals = malloc(len * sizeof(struct JVal*));
            if (!jvals) {
                printf("ERROR: jval_heapalloc: call to malloc %d new JVals*(s) failed.", len);
                exit(EXIT_FAILURE);
            }
            jval->ptr = jvals;
            return jval;
        default:
            printf("ERROR: jval_heapalloc: unsupported type: %d", type);
            exit(EXIT_FAILURE);
    }
}

void jmemory_report() {
    printf("=== MEMORY REPORT ========================\n");
    printf("%d\tactive JVals allocated on heap\n", heap_jval_alloc_counter);
    printf("==========================================\n");
}
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <stdbool.h>

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
};

enum JMonadicVerb {
  JIncrementOp = 1,
  JSquareOp = 2,
  JNegateOp = 3,
  JReciprocalOp = 4,
  JTallyOp = 5,
};

struct JVal* jdyad_internal_numeric_with_array(enum JDyadicVerb op,
                                               struct JVal* numeric,
                                               struct JVal* arr,
                                               bool is_numeric_lhs);


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

    if (lhs->type == JIntegerType && rhs->type == JIntegerType) {
        lhsi = *((int*) lhs->ptr);
        rhsi = *((int*) rhs->ptr);

        ret = (struct JVal*) malloc(sizeof(struct JVal));
        ret->len = 1;

        switch(op) {
            case JPlusOp:
                iptr = (int*) malloc(sizeof(int));
                *iptr = lhsi + rhsi;
                ret->type = JIntegerType;
                ret->ptr = iptr;
                return ret;
            case JMinusOp:
                iptr = (int*) malloc(sizeof(int));
                *iptr = lhsi - rhsi;
                ret->type = JIntegerType;
                ret->ptr = iptr;
                return ret;
            case JTimesOp:
                iptr = (int*) malloc(sizeof(int));
                *iptr = lhsi * rhsi;
                ret->type = JIntegerType;
                ret->ptr = iptr;
                return ret;
            case JDivideOp:
                // Division always produces a double.
                dptr = (double*) malloc(sizeof(double));
                *dptr = (double) lhsi / (double) rhsi;
                ret->type = JDoublePrecisionFloatType;
                ret->ptr = dptr;
                return ret;
            case JPowerOp:
                iptr = (int*) malloc(sizeof(int));
                *iptr = pow(lhsi, rhsi);
                ret->type = JIntegerType;
                ret->ptr = iptr;
                return ret;
            case JLessThanOp:
                iptr = (int*) malloc(sizeof(int));
                *iptr = (lhsi < rhsi) ? 1 : 0;
                ret->type = JIntegerType;
                ret->ptr = iptr;
                return ret;
            case JLargerThanOp:
                iptr = (int*) malloc(sizeof(int));
                *iptr = (lhsi > rhsi) ? 1 : 0;
                ret->type = JIntegerType;
                ret->ptr = iptr;
                return ret;
            case JEqualOp:
                iptr = (int*) malloc(sizeof(int));
                *iptr = (lhsi == rhsi) ? 1 : 0;
                ret->type = JIntegerType;
                ret->ptr = iptr;
                return ret;
            case JResidueOp:
                iptr = (int*) malloc(sizeof(int));
                // IMPORTANT: In J the operands to Residue are opposite of "%";
                // i.e., 5 | 2 means 2 % 5
                *iptr = (rhsi % lhsi);
                ret->type = JIntegerType;
                ret->ptr = iptr;
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

        ret = (struct JVal*) malloc(sizeof(struct JVal));
        ret->len = 1;

        switch(op) {
            case JPlusOp:
                dptr = (double*) malloc(sizeof(double));
                *dptr = lhsd + rhsd;
                ret->type = JDoublePrecisionFloatType;
                ret->ptr = dptr;
                return ret;
            case JMinusOp:
                dptr = (double*) malloc(sizeof(double));
                *dptr = lhsd - rhsd;
                ret->type = JDoublePrecisionFloatType;
                ret->ptr = dptr;
                return ret;
            case JTimesOp:
                dptr = (double*) malloc(sizeof(double));
                *dptr = lhsd * rhsd;
                ret->type = JDoublePrecisionFloatType;
                ret->ptr = dptr;
                return ret;
            case JDivideOp:
                dptr = (double*) malloc(sizeof(double));
                *dptr = lhsd / rhsd;
                ret->type = JDoublePrecisionFloatType;
                ret->ptr = dptr;
                return ret;
            case JLessThanOp:
                iptr = (int*) malloc(sizeof(int));
                *iptr = (lhsd < rhsd) ? 1 : 0;
                ret->type = JIntegerType;
                ret->ptr = iptr;
                return ret;
            case JLargerThanOp:
                iptr = (int*) malloc(sizeof(int));
                *iptr = (lhsd > rhsd) ? 1 : 0;
                ret->type = JIntegerType;
                ret->ptr = iptr;
                return ret;
            case JEqualOp:
                iptr = (int*) malloc(sizeof(int));
                *iptr = (lhsd == rhsd) ? 1 : 0;
                ret->type = JIntegerType;
                ret->ptr = iptr;
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
        jvals_out = (struct JVal**) malloc(lhs->len * sizeof(struct JVal*));

        for (int i = 0; i < lhs->len; i++) {
            jvals_out[i] = jdyad(op, jvals_a[i], jvals_b[i]);
        }

        ret = (struct JVal*) malloc(sizeof(struct JVal));
        ret->type = JArrayType;
        ret->len = lhs->len;
        ret->ptr = jvals_out;
        return ret;
    }

    printf("ERROR: jdyad: unsupported lhs (type:%d,len:%d) and rhs (type:%d,len:%d)\n",
        lhs->type, lhs->len, rhs->type, rhs->len);
    exit(EXIT_FAILURE);
}

struct JVal* jdyad_internal_numeric_with_array(enum JDyadicVerb op,
                                               struct JVal* numeric,
                                               struct JVal* arr,
                                               bool is_numeric_lhs) {
    struct JVal* ret;
    struct JVal** jvals;
    struct JVal** jvals_out;

    jvals = arr->ptr;
    jvals_out = (struct JVal**) malloc(arr->len * sizeof(struct JVal*));

    for (int i = 0; i < arr->len; i++) {
        // Must preserve order of operands as some operations are not commutative.
        if (is_numeric_lhs)     { jvals_out[i] = jdyad(op, numeric, jvals[i]); }
        else                    { jvals_out[i] = jdyad(op, jvals[i], numeric); }
    }

    ret = (struct JVal*) malloc(sizeof(struct JVal));
    ret->type = JArrayType;
    ret->len = arr->len;
    ret->ptr = jvals_out;
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

            ret = (struct JVal*) malloc(sizeof(struct JVal));
            ret->len = 1;

            switch (op) {
                case JIncrementOp:
                    iptr = (int*) malloc(sizeof(int));
                    *iptr = expri + 1;
                    ret->type = JIntegerType;
                    ret->ptr = iptr;
                    return ret;
                case JNegateOp:
                    iptr = (int*) malloc(sizeof(int));
                    *iptr = expri * -1;
                    ret->type = JIntegerType;
                    ret->ptr = iptr;
                    return ret;
                case JSquareOp:
                    iptr = (int*) malloc(sizeof(int));
                    *iptr = pow(expri, 2);
                    ret->type = JIntegerType;
                    ret->ptr = iptr;
                    return ret;
                case JReciprocalOp:
                    dptr = (double*) malloc(sizeof(double));
                    *dptr = 1.0 / (float) expri;
                    ret->type = JDoublePrecisionFloatType;
                    ret->ptr = dptr;
                    return ret;
                case JTallyOp:
                    iptr = (int*) malloc(sizeof(int));
                    *iptr = expr->len;
                    ret->type = JIntegerType;
                    ret->ptr = iptr;
                    return ret;
                default:
                    printf("ERROR: jmonad: unsupported verb on type integer: %d\n", op);
                    exit(EXIT_FAILURE);
            }

        case JDoublePrecisionFloatType:
            exprd = *((double*) expr->ptr);
            dptr = (double*) malloc(sizeof(double));

            switch (op) {
                case JIncrementOp:
                    *dptr = exprd + 1.0;
                    break;
                case JNegateOp:
                    *dptr = exprd * -1.0;
                    break;
                case JSquareOp:
                    *dptr = pow(exprd, 2.0);
                    break;
                default:
                    printf("ERROR: jmonad: unsupported verb on type double: %d\n", op);
                    exit(EXIT_FAILURE);
            }

            ret = (struct JVal*) malloc(sizeof(struct JVal));
            ret->type = JDoublePrecisionFloatType;
            ret->len = 1;
            ret->ptr = dptr;
            return ret;

        case JArrayType:

            if (op == JTallyOp) {
                // The tally monad is not distributed over arrays;
                // return the length of the array itself.
                iptr = (int*) malloc(sizeof(int));
                *iptr = expr->len;

                ret = (struct JVal*) malloc(sizeof(struct JVal));
                ret->type = JIntegerType;
                ret->len = 1;
                ret->ptr = iptr;
                return ret;
            }

            jvals_in = expr->ptr;
            jvals_out = (struct JVal**) malloc(expr->len * sizeof(struct JVal*));

            for (int i = 0; i < expr->len; i++) {
                jvals_out[i] = jmonad(op, jvals_in[i]);
            }

            ret = (struct JVal*) malloc(sizeof(struct JVal));
            ret->type = JArrayType;
            ret->len = expr->len;
            ret->ptr = jvals_out;
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
            ret = (struct JVal*) malloc(sizeof(struct JVal));

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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <stdbool.h>

#include "jcommon.h"

// NOTE: To get LLVM IR of this source, run
// $ clang-7 -S jlib.c -emit-llvm -o -  (note the trailing hyphen)

// Default print precision is 6; can be changed within J.
static int JPRINT_PRECISION = 6;

struct JVal* jdyad_internal_copy_verb(struct JVal* lhs, struct JVal* rhs);
struct JVal* jdyad_internal_numeric_with_array(enum JDyadicVerb op,
                                               struct JVal* numeric,
                                               struct JVal* arr,
                                               bool is_numeric_lhs);
struct JVal* jval_heapalloc_array_dim1(int length);
struct JVal* jval_heapalloc_int();
struct JVal* jval_heapalloc_double();
void jval_drop(struct JVal* jval, bool do_drop_globals);
struct JVal* jval_clone(struct JVal* jval, enum JValLocation loc);


void jprint(struct JVal* val, bool newline) {
  struct JVal** jvals;
  int* iptr;
  double* dptr;
  char* sptr;

  switch (val->type) {
    case JIntegerType:
      iptr = (int*) val->ptr;
      if (*iptr < 0)         { printf("_%d", abs(*iptr)); }
      else                   { printf("%d", *iptr);       }
      break;
    case JDoublePrecisionFloatType:
      dptr = (double*) val->ptr;
      if (signbit(*dptr) && *dptr != -0.0) { printf("_%.*g", JPRINT_PRECISION, fabs(*dptr)); }
      else                                 { printf("%.*g", JPRINT_PRECISION, fabs(*dptr)); }
      break;
    case JStringType:
      sptr = (char*) val->ptr;
      printf("%s", sptr);
      break;
    case JArrayType:
      jvals = val->ptr;
      for (int i = 0; i < val->shape[0]; i++) {
        jprint(jvals[i], false);
        if (i < (val->shape[0] - 1)) { printf(" "); }
      }
      break;
    default:
      printf("ERROR: jprint: unsupported JVal (loc:%d, type:%d, rank:%d)\n",
        val->loc, val->type, val->rank);
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
                ret = jval_heapalloc_int();
                *(int*)ret->ptr = lhsi + rhsi;
                return ret;
            case JMinusOp:
                ret = jval_heapalloc_int();
                *(int*)ret->ptr = lhsi - rhsi;
                return ret;
            case JTimesOp:
                ret = jval_heapalloc_int();
                *(int*)ret->ptr = lhsi * rhsi;
                return ret;
            case JDivideOp:
                // Division always produces a double.
                ret = jval_heapalloc_double();
                *(double*)ret->ptr = (double) lhsi / (double) rhsi;
                return ret;
            case JPowerOp:
                ret = jval_heapalloc_int();
                *(int*)ret->ptr = pow(lhsi, rhsi);
                return ret;
            case JLessThanOp:
                ret = jval_heapalloc_int();
                *(int*)ret->ptr = (lhsi < rhsi) ? 1 : 0;
                return ret;
            case JLargerThanOp:
                ret = jval_heapalloc_int();
                *(int*)ret->ptr = (lhsi > rhsi) ? 1 : 0;
                return ret;
            case JLargerOrEqual:
                ret = jval_heapalloc_int();
                *(int*)ret->ptr = (lhsi >= rhsi) ? 1 : 0;
                return ret;
            case JLargerOfOp:
                ret = jval_heapalloc_int();
                *(int*)ret->ptr = (lhsi > rhsi) ? lhsi : rhsi;
                return ret;
            case JEqualOp:
                ret = jval_heapalloc_int();
                *(int*)ret->ptr = (lhsi == rhsi) ? 1 : 0;
                return ret;
            case JResidueOp:
                ret = jval_heapalloc_int();
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
                ret = jval_heapalloc_double();
                *(double*)ret->ptr = lhsd + rhsd;
                return ret;
            case JMinusOp:
                ret = jval_heapalloc_double();
                *(double*)ret->ptr = lhsd - rhsd;
                return ret;
            case JTimesOp:
                ret = jval_heapalloc_double();
                *(double*)ret->ptr = lhsd * rhsd;
                return ret;
            case JDivideOp:
                ret = jval_heapalloc_double();
                *(double*)ret->ptr = lhsd / rhsd;
                return ret;
            case JLessThanOp:
                ret = jval_heapalloc_int();
                *(int*)ret->ptr = (lhsd < rhsd) ? 1 : 0;
                return ret;
            case JLargerThanOp:
                ret = jval_heapalloc_int();
                *(int*)ret->ptr = (lhsd > rhsd) ? 1 : 0;
                return ret;
            case JEqualOp:
                ret = jval_heapalloc_int();
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
        if (lhs->shape[0] != rhs->shape[0]) {
            printf("ERROR: jdyad: length mismatch: lhs (type:%d,len:%d), rhs (type:%d,len:%d)\n",
                lhs->type, lhs->shape[0], rhs->type, rhs->shape[0]);
            exit(EXIT_FAILURE);
        }

        jvals_a = lhs->ptr;
        jvals_b = rhs->ptr;
        ret = jval_heapalloc_array_dim1(lhs->shape[0]);
        jvals_out = (struct JVal**)ret->ptr;

        for (int i = 0; i < lhs->shape[0]; i++) {
            jvals_out[i] = jdyad(op, jvals_a[i], jvals_b[i]);
        }

        return ret;
    }

    printf("ERROR: jdyad: unsupported lhs (type:%d,len:%d) and rhs (type:%d,len:%d)\n",
        lhs->type, lhs->shape[0], rhs->type, rhs->shape[0]);
    exit(EXIT_FAILURE);
}

struct JVal* jflatten(struct JVal* jval) {

    if (jval->type != JArrayType) {
        printf("ERROR: jflatten: illegal attempt to flatten non-array type: %d", jval->type);
        exit(EXIT_FAILURE);
    }

    int flatlen = 0;
    struct JVal** arr;

    // First sum up the length of the subarrays comprising the array.
    arr = (struct JVal**)jval->ptr;
    for (int i = 0; i < jval->shape[0]; i++) {
        if (arr[i]->type != JArrayType) {
            printf("ERROR: jflatten: encountered non-array type inside array being flattened; type is: %d",
                arr[i]->type);
            exit(EXIT_FAILURE);
        }
        flatlen += arr[i]->shape[0];
    }

    // Allocate an array to hold flattened items and store each subarray's items inside.
    struct JVal* ret;
    ret = jval_heapalloc_array_dim1(flatlen);
    struct JVal** jvals_out;
    jvals_out = (struct JVal**)ret->ptr;
    int k = 0;
    for (int i = 0; i < jval->shape[0]; i++) {
        for (int j = 0; j < arr[i]->shape[0]; j++) {
            jvals_out[k] = jval_clone(((struct JVal**)(arr[i]->ptr))[j], JLocHeapLocal);
            k += 1;
        }
    }
    return ret;
}

struct JVal* jdyad_internal_copy_verb(struct JVal* lhs, struct JVal* rhs) {
    struct JVal* ret;
    struct JVal* intermediate;
    struct JVal** jvals_a;
    struct JVal** jvals_b;
    int* iptr;
    int lhsi, rhsi;
    struct JVal** jvals_out;

    if (lhs->type == JIntegerType && rhs->type == JIntegerType) {
        lhsi = *((int*) lhs->ptr);
        rhsi = *((int*) rhs->ptr);

        ret = jval_heapalloc_array_dim1(lhsi);
        jvals_out = (struct JVal**)ret->ptr;

        // Copies the rhs value "n" times, where n is lhs value.
        for (int i = 0; i < lhsi; i++) {
            intermediate = jval_heapalloc_int();
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

        // Array lengths must be the same.
        if (lhs->shape[0] != rhs->shape[0]) {
            printf("ERROR: jdyad: copy: length mismatch: lhs (type:%d,len:%d), rhs (type:%d,len:%d)\n",
                lhs->type, lhs->shape[0], rhs->type, rhs->shape[0]);
            exit(EXIT_FAILURE);
        }

        jvals_a = lhs->ptr;
        jvals_b = rhs->ptr;
        intermediate = jval_heapalloc_array_dim1(lhs->shape[0]);
        jvals_out = (struct JVal**)intermediate->ptr;

        for (int i = 0; i < lhs->shape[0]; i++) {
            jvals_out[i] = jdyad_internal_copy_verb(jvals_a[i], jvals_b[i]);
        }

//        // Flatten the intermediate array, then drop before returning flattened array.
        ret = jflatten(intermediate);
        jval_drop(intermediate, false);
        return ret;

    } else {
        printf("ERROR: jdyad: copy: unsupported lhs (type:%d,len:%d) and rhs (type:%d,len:%d)\n",
            lhs->type, lhs->shape[0], rhs->type, rhs->shape[0]);
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
    ret = jval_heapalloc_array_dim1(arr->shape[0]);
    jvals_out = (struct JVal**)ret->ptr;

    for (int i = 0; i < arr->shape[0]; i++) {
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
                    ret = jval_heapalloc_int();
                    *(int*)ret->ptr = expri + 1;
                    return ret;
                case JNegateOp:
                    ret = jval_heapalloc_int();
                    *(int*)ret->ptr = expri * -1;
                    return ret;
                case JSquareOp:
                    ret = jval_heapalloc_int();
                    *(int*)ret->ptr = pow(expri, 2);
                    return ret;
                case JReciprocalOp:
                    ret = jval_heapalloc_double();
                    *(double*)ret->ptr = 1.0 / (float) expri;
                    return ret;
                case JTallyOp:
                    ret = jval_heapalloc_int();
                    // An integer always has a length/tally of 1.
                    *(int*)ret->ptr = 1;
                    return ret;
                case JCeilingOp:
                    // ceil(int) is the int itself:
                    ret = jval_heapalloc_int();
                    *(int*)ret->ptr = expri;
                    return ret;
                default:
                    printf("ERROR: jmonad: unsupported verb on type integer: %d\n", op);
                    exit(EXIT_FAILURE);
            }

        case JDoublePrecisionFloatType:
            exprd = *((double*) expr->ptr);

            switch (op) {
                case JIncrementOp:
                    ret = jval_heapalloc_double();
                    *(double*)ret->ptr = exprd + 1.0;
                    return ret;
                case JNegateOp:
                    ret = jval_heapalloc_double();
                    *(double*)ret->ptr = exprd * -1.0;
                    return ret;
                case JSquareOp:
                    ret = jval_heapalloc_double();
                    *(double*)ret->ptr = pow(exprd, 2.0);
                    return ret;
                case JCeilingOp:
                    ret = jval_heapalloc_int();
                    *(int*)ret->ptr = ceil(exprd);
                    return ret;
                default:
                    printf("ERROR: jmonad: unsupported verb on type double: %d\n", op);
                    exit(EXIT_FAILURE);
            }
        case JStringType:

            switch (op) {
                case JTallyOp:
                    ret = jval_heapalloc_int();
                    // A string has one dimension; shape[0] get that dimension's size.
                    // 1 is subtracted to disregard null terminating byte.
                    *(int*)ret->ptr = expr->shape[0] - 1;
                    return ret;
                default:
                    printf("ERROR: jmonad: unsupported verb on type string: %d\n", op);
                    exit(EXIT_FAILURE);
            }

        case JArrayType:

            if (op == JTallyOp) {
                // The tally monad is not distributed over arrays;
                // return the length of the array itself.
                ret = jval_heapalloc_int();
                *(int*)ret->ptr = expr->shape[0];
                return ret;
            }

            jvals_in = expr->ptr;
            ret = jval_heapalloc_array_dim1(expr->shape[0]);
            jvals_out = (struct JVal**)ret->ptr;

            for (int i = 0; i < expr->shape[0]; i++) {
                jvals_out[i] = jmonad(op, jvals_in[i]);
            }

            return ret;
        default:
            printf("ERROR: jmonad: unsupported expr (type:%d,len:%d)\n",
                expr->type, expr->shape[0]);
            exit(EXIT_FAILURE);
    }
}

struct JVal* jreduce(enum JDyadicVerb verb, struct JVal* expr) {

    // Reducing over a zero length or single expression has no effect.
    if (expr->rank == 0 || expr->shape[0] == 1) { return expr; }

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
            i = expr->shape[0] - 2;
            ret = jvals_in[i + 1];
            do {
                // TODO: Free the intermediate values that are
                // returned by jdyad but never used beyond this function.
                intermediate = jdyad(verb, jvals_in[i], ret);
                if (i != expr->shape[0] -2) {
                    // On the first iteration of the loop, ret points to
                    // the last element of the array. On all other iterations,
                    // ret points to an intermediate computation from the previous
                    // loop iteration. Free only those previous intermediates here.
                    jval_drop(ret, false);
                }
                ret = intermediate;
                i -= 1;
            } while (i >= 0);

            return ret;

        default:
            printf("ERROR: jreduce: unsupported expr (type:%d,len:%d)\n",
                expr->type, expr->shape[0]);
            exit(EXIT_FAILURE);
    }
}

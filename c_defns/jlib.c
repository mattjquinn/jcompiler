#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <stdbool.h>

// NOTE: To get LLVM IR of this source, run
// $ clang-7 -S jlib.c -emit-llvm -o -  (note the trailing hyphen)

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
};

enum JDyadicVerb {
  JPlusOp = 1,
  JTimesOp = 2,
  JLessThanOp = 3,
  JLargerThanOp = 4,
  JEqualOp = 5,
  JMinusOp = 6,
};

enum JMonadicVerb {
  JIncrementOp = 1,
  JSquareOp = 2,
  JNegateOp = 3,
};

void jprint(struct JVal* val, bool newline) {
  struct JVal** jvals;
  int* iptr;

  switch (val->type) {
    case JIntegerType:
      iptr = (int*) val->ptr;
      if (*iptr < 0)         { printf("_%d", abs(*iptr)); }
      else                   { printf("%d", *iptr);       }
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
    int *iptr;
    int lhsi, rhsi;

    if (lhs->type == JIntegerType && rhs->type == JIntegerType) {
        lhsi = *((int*) lhs->ptr);
        rhsi = *((int*) rhs->ptr);
        iptr = (int*) malloc(sizeof(int));

        switch(op) {
            case JPlusOp:
                *iptr = lhsi + rhsi;
                break;
            case JMinusOp:
                *iptr = lhsi - rhsi;
                break;
            case JTimesOp:
                *iptr = lhsi * rhsi;
                break;
            case JLessThanOp:
                *iptr = (lhsi < rhsi) ? 1 : 0;
                break;
            case JLargerThanOp:
                *iptr = (lhsi > rhsi) ? 1 : 0;
                break;
            case JEqualOp:
                *iptr = (lhsi == rhsi) ? 1 : 0;
                break;
            default:
                printf("ERROR: jdyad: unsupported nop: %d\n", op);
                exit(EXIT_FAILURE);
        }

        ret = (struct JVal*) malloc(sizeof(struct JVal));
        ret->type = JIntegerType;
        ret->len = 1;
        ret->ptr = iptr;
        return ret;

    } else if (lhs->type == JIntegerType && rhs->type == JArrayType) {
        jvals_a = rhs->ptr;
        jvals_out = (struct JVal**) malloc(rhs->len * sizeof(struct JVal*));

        for (int i = 0; i < rhs->len; i++) {
            jvals_out[i] = jdyad(op, lhs, jvals_a[i]);
        }

        ret = (struct JVal*) malloc(sizeof(struct JVal));
        ret->type = JArrayType;
        ret->len = rhs->len;
        ret->ptr = jvals_out;
        return ret;

    } else if (lhs->type == JArrayType && rhs->type == JIntegerType) {
        jvals_a = lhs->ptr;
        jvals_out = (struct JVal**) malloc(lhs->len * sizeof(struct JVal*));

        for (int i = 0; i < lhs->len; i++) {
            jvals_out[i] = jdyad(op, jvals_a[i], rhs);
        }

        ret = (struct JVal*) malloc(sizeof(struct JVal));
        ret->type = JArrayType;
        ret->len = lhs->len;
        ret->ptr = jvals_out;
        return ret;

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

struct JVal* jmonad(enum JMonadicVerb op, struct JVal* expr) {
    struct JVal** jvals_in;
    struct JVal** jvals_out;
    struct JVal* ret;
    int *iptr;
    int expri;

    switch (expr->type) {
        case JIntegerType:
            expri = *((int*) expr->ptr);
            iptr = (int*) malloc(sizeof(int));

            switch (op) {
                case JIncrementOp:
                    *iptr = expri + 1;
                    break;
                case JNegateOp:
                    *iptr = expri * -1;
                    break;
                case JSquareOp:
                    *iptr = pow(expri, 2);
                    break;
                default:
                    printf("ERROR: jmonad: unsupported op: %d\n", op);
                    exit(EXIT_FAILURE);
            }

            ret = (struct JVal*) malloc(sizeof(struct JVal));
            ret->type = JIntegerType;
            ret->len = 1;
            ret->ptr = iptr;
            return ret;
        case JArrayType:
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

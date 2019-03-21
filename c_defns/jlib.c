#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

// NOTE: To get LLVM IR of this source, run
// $ clang-7 -S jlib.c -emit-llvm -o -  (note the trailing hyphen)

void jprint(int* arr, int len) {
//  int m = 5;
//  int a[100];
//  a[0] = m;
//  printf("%d, %d\n", a[0], a[1]);
  for (int a = 0; a < len; a++) {
    int i = arr[a];
    if (i < 0)         { printf("_%d", abs(i)); }
    else               { printf("%d", i); }
    if (a < (len - 1)) { printf(" "); }
  }
  printf("\n");
}

int* jnegate(int* arr, int len) {
  int* neg_arr = (int*)malloc(len * sizeof(int));
  for (int i = 0; i < len; i++) {
    neg_arr[i] = arr[i] * -1;
  }
  return neg_arr;
}

int* jincrement(int* arr, int len) {
  int* inc_arr = (int*)malloc(len * sizeof(int));
  for (int i = 0; i < len; i++) {
    inc_arr[i] = arr[i] + 1;
  }
  return inc_arr;
}

int* jsquare(int* arr, int len) {
  int* sq_arr = (int*)malloc(len * sizeof(int));
  for (int i = 0; i < len; i++) {
    sq_arr[i] = pow(arr[i], 2);
  }
  return sq_arr;
}

int* jplus(int* lhs, int lhslen, int* rhs, int rhslen) {

  // Requires same-length array operands; expansion of different length
  // operands should be done in LLVM prior to adding a call to this function.
  assert(lhslen == rhslen);

  int* sum_arr = (int*)malloc(lhslen * sizeof(int));
  for (int i = 0; i < lhslen; i++) {
    sum_arr[i] = lhs[i] + rhs[i];
  }
  return sum_arr;
}

int* jminus(int* lhs, int lhslen, int* rhs, int rhslen) {

  // Requires same-length array operands; expansion of different length
  // operands should be done in LLVM prior to adding a call to this function.
  assert(lhslen == rhslen);

  int* diff_arr = (int*)malloc(lhslen * sizeof(int));
  for (int i = 0; i < lhslen; i++) {
    diff_arr[i] = lhs[i] - rhs[i];
  }
  return diff_arr;
}

int* jtimes(int* lhs, int lhslen, int* rhs, int rhslen) {

  // Requires same-length array operands; expansion of different length
  // operands should be done in LLVM prior to adding a call to this function.
  assert(lhslen == rhslen);

  int* prod_arr = (int*)malloc(lhslen * sizeof(int));
  for (int i = 0; i < lhslen; i++) {
    prod_arr[i] = lhs[i] * rhs[i];
  }
  return prod_arr;
}

int* jreduce_plus(int* expr_arr, int len) {

  // Reducing always results in a single element array.
  int* out_arr = (int*)malloc(sizeof(int));

  out_arr[0] = 0;
  for (int i = 0; i < len; i++) {
    out_arr[0] += expr_arr[i];
  }
  return out_arr;
}

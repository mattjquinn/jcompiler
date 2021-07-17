#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "jarm.h"

void jprint_int(int val, char emit_space, char emit_newline) {
  if (val < 0) {
    printf("_%d", abs(val));
  } else {
    printf("%d", val);
  }
  if (emit_space) {
    printf(" ");
  }
  if (emit_newline) {
    printf("\n");
  }
}

void jprint_double(double val, char emit_space, char emit_newline) {
  if (floor(val) != val) {
    if (val < 0) {
      printf("_%g", -1*val);
    } else {
      printf("%g", val);
    }
    if (emit_space) {
      printf(" ");
    }
    if (emit_newline) {
      printf("\n");
    }
  } else {
    jprint_int((int) val, emit_space, emit_newline);
  }
}

int jceiling(double val) {
  int out = (int) val;
  if (floor(val) != val && out >= 0) {
    out += 1;
  }
  return out;
}
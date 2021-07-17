#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "jarm.h"

void jprint_int(int val, char flags) {
  if (val < 0) {
    printf("_%d", abs(val));
  } else {
    printf("%d", val);
  }
  if (flags == 0x1) {
    printf(" ");
  }
  if (flags == 0x2) {
    printf("\n");
  }
}

void jprint_double(double val, char flags) {
  if (floor(val) != val) {
    if (val < 0) {
      printf("_%g", -1*val);
    } else {
      printf("%g", val);
    }
    if (flags == 0x1) {
      printf(" ");
    }
    if (flags == 0x2) {
      printf("\n");
    }
  } else {
    jprint_int((int) val, flags);
  }
}

int jceiling(double val) {
  int out = (int) val;
  if (floor(val) != val && out >= 0) {
    out += 1;
  }
  return out;
}
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "jarm.h"

void jprint_int(int val) {
  if (val < 0) {
    printf("_%d", abs(val));
  } else {
    printf("%d", val);
  }
}

void jprint_double(double val) {
  if (floor(val) != val) {
    if (val < 0) {
      printf("_%g", -1*val);
    } else {
      printf("%g", val);
    }
  } else {
    jprint_int((int) val);
  }
}

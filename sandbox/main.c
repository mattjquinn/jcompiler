#include <stdio.h>
#include "jarm.h"

void main() {
  int a = 2;
  double recip = 1.0 / a;
  jprint_double(recip, 0, 1);
}


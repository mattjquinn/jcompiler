#include <stdio.h>

void jprint(int i) {
    if (i < 0) { printf("_%d", abs(i)); }
    else       { printf("%d", i); }
}
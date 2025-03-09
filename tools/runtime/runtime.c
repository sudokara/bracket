#include "runtime.h"
#include <stdio.h>
#include <stdlib.h>

void write_int(int v) { 
  printf("%d\n", v);
}

int read_int() {
  int val;
  scanf("%d", &val);
  return val;
}

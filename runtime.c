#include <stdlib.h>
#include <stdio.h>

extern void miniml_main();

void *miniml_malloc(size_t size) {
  return malloc(size);
}

double miniml_printd(double d) {
  printf("%g\n", d);
  return 0.;
}


int main (int argc, char **argv) {
  miniml_main();
}

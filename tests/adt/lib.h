#include <stdio.h>
#include <inttypes.h>

typedef union {
    struct { int a; double b; } b;
    struct { int a; } c;
} lib_t;

void f_print(lib_t *);

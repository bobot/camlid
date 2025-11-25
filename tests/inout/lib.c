#include "lib.h"

void f_nat(intptr_t *x){ (*x)++; }
void f_int(int *x){ (*x)++; }
void f_double(double *x){ *x += 0.25; }
void f_int32(int32_t *x){ (*x)++; }
void f_int64(int64_t *x){ (*x)++; }

#include "lib.h"

int step;

void f(int * i) { *i=step++; }
void f2(int * i,int * j) { *i=step++; *j=step++; }
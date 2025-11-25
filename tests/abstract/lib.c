#include "lib.h"
#include "stdio.h"
#include "malloc.h"

void lib_free(int * p){ printf("finalize\n"); fflush(stdout); free(p); }
void lib_init(int ** p){ printf("initialize\n"); fflush(stdout); *p = malloc(sizeof(int)); }

void of_int(int i,int* p){ *p = i;}
int to_int(int* p){ return *p; }

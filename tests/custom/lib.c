#include "lib.h"
#include "stdio.h"
#include "malloc.h"

void finalize_ptr(int ** p){ printf("finalize\n"); fflush(stdout); free(*p); }
void initialize_ptr(int ** p){ printf("initialize\n"); fflush(stdout); *p = malloc(sizeof(int)); }
int compare_ptr(int ** p, int ** q){ return **p - **q; }
intptr_t hash_ptr(int ** p){return **p; }

void of_int(int i,int* p){ *p = i;}
int to_int(int* p){ return *p; }
#include "lib.h"
#include <stdio.h>

void f_print(lib_t *x){ printf("(%i,%f)\n",x->b.a,x->b.b); fflush(stdout); }
#include <stdlib.h>
#include <string.h>
void name_clear(name_t *n) { free(n->s); n->s = NULL; }
void f_hello(name_t *n) { printf("hello %s\n", n->s ? n->s : "anonymous"); fflush(stdout); }
void f_make(name_t *n, int i) { n->s = i ? copy_string("made") : NULL; }

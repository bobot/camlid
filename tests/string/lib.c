#define _GNU_SOURCE

#include "lib.h"
#include <stdio.h>
#include <string.h>

void f_in(char *s) { printf("(%s)\n",s); fflush(stdout); }
void f_out(char **s) { *s = strdup("Hello!"); }
void f_in3(char *s) { memcpy(s,"Hello!",3); }
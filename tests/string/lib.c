#define _GNU_SOURCE

#include "lib.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void f_in(char *s) { printf("(%s)\n",s); fflush(stdout); }
void f_in2(char *s1, char *s2) { printf("(%s,%s)\n",s1,s2); fflush(stdout); }
void f_out(char **s) { *s = strdup("Hello!"); }
void f_in3(char *s) { memcpy(s,"Hello!",3); }
void f_print_n(char *s, size_t n) { printf("[%.*s]\n", (int)n, s); fflush(stdout); }
/* prints the null bytes as '0' */
void f_len(char *s, size_t n) {
  for (size_t i = 0; i < n; i++) putchar(s[i] ? s[i] : '0');
  putchar('\n'); fflush(stdout);
}
void f_out_len(char **s, size_t *n) { *n = 4; *s = malloc(4); memcpy(*s, "a\0bc", 4); }

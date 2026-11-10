#include <stdio.h>
#include <inttypes.h>

typedef union {
    struct { int a; double b; } b;
    struct { int a; } c;
} lib_t;

void f_print(lib_t *);

#include <stdlib.h>
#include <string.h>
static inline char *copy_string(const char *s) {
  size_t len = strlen(s) + 1;
  return memcpy(malloc(len), s, len);
}

/* s == NULL for Anonymous */
typedef struct { char *s; } name_t;
void name_clear(name_t *);
void f_hello(name_t *);
void f_make(name_t *, int);

#include "lib.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static intptr_t live = 0;

void vec_init(vec *v) { v->data = NULL; v->len = 0; live++; }
void vec_clear(vec *v) { free(v->data); v->data = NULL; live--; }
void vec_set(vec *dst, vec *src) {
  if (dst == src) return;
  dst->data = realloc(dst->data, src->len * sizeof(intptr_t));
  if (src->len) memcpy(dst->data, src->data, src->len * sizeof(intptr_t));
  dst->len = src->len;
}
intptr_t vec_live(void) { return live; }

int checked_div(intptr_t a, intptr_t b, intptr_t *res) {
  if (b == 0) return -1;
  *res = a / b;
  return 0;
}
int is_negative(int rc) { return rc < 0; }
int is_negative_n(int rc, intptr_t n) { (void)n; return rc < 0; }

int make(vec *v, char **s, intptr_t n) {
  /* allocated even on failure, the stub must free them */
  v->data = malloc(sizeof(intptr_t));
  *s = malloc(32);
  if (n < 0) return -2;
  v->data = realloc(v->data, (size_t)n * sizeof(intptr_t) + 1);
  for (intptr_t i = 0; i < n; i++) v->data[i] = i;
  v->len = (size_t)n;
  snprintf(*s, 32, "%" PRIdPTR " elements", n);
  return 0;
}
const char *error_msg(int rc, intptr_t n) {
  static char buf[64];
  snprintf(buf, sizeof buf, "error %d for n=%" PRIdPTR, rc, n);
  return buf;
}

intptr_t slow_sum(intptr_t n) {
  intptr_t s = 0;
  for (intptr_t i = 0; i < n; i++) s += i;
  return s;
}

#include "lib.h"
#include <stdlib.h>
#include <string.h>

/* number of initialized and not cleared vectors */
static intptr_t live = 0;

void vec_init(vec *v) { v->data = NULL; v->len = 0; live++; }
void vec_init_n(vec *v, size_t n) {
  v->data = calloc(n, sizeof(intptr_t)); v->len = n; live++;
}
void vec_clear(vec *v) { free(v->data); v->data = NULL; live--; }
void vec_set(vec *dst, vec *src) {
  if (dst == src) return;
  dst->data = realloc(dst->data, src->len * sizeof(intptr_t));
  if (src->len) memcpy(dst->data, src->data, src->len * sizeof(intptr_t));
  dst->len = src->len;
}
intptr_t vec_hash(vec *v) { return vec_sum(v) + (intptr_t)v->len; }
int vec_compare(vec *v1, vec *v2) {
  if (v1->len != v2->len) return v1->len < v2->len ? -1 : 1;
  for (size_t i = 0; i < v1->len; i++)
    if (v1->data[i] != v2->data[i]) return v1->data[i] < v2->data[i] ? -1 : 1;
  return 0;
}

void vec_push(vec *v, intptr_t x) {
  v->data = realloc(v->data, (v->len + 1) * sizeof(intptr_t));
  v->data[v->len++] = x;
}
intptr_t vec_sum(vec *v) {
  intptr_t s = 0;
  for (size_t i = 0; i < v->len; i++) s += v->data[i];
  return s;
}
size_t vec_len(vec *v) { return v->len; }
void vec_range(vec *v, size_t n) {
  for (size_t i = 0; i < n; i++) vec_push(v, (intptr_t)i);
}
void vec_ranges(vec *a, size_t n) {
  for (size_t i = 0; i < n; i++) vec_range(&a[i], i);
}
intptr_t vec_total(vec *a, size_t n) {
  intptr_t s = 0;
  for (size_t i = 0; i < n; i++) s += vec_sum(&a[i]);
  return s;
}
vec *vec_global(void) {
  static vec g;
  static int initialized = 0;
  if (!initialized) { vec_init_n(&g, 2); g.data[1] = 100; initialized = 1; }
  return &g;
}
intptr_t vec_live(void) { return live; }
size_t vec_mem(vec *v) { return v->len * sizeof(intptr_t); }

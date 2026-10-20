#include <inttypes.h>
#include <stddef.h>

/* A growable vector of integers, handled like a C++ class */
typedef struct { intptr_t *data; size_t len; } vec;

void vec_init(vec *v);
void vec_init_n(vec *v, size_t n);
void vec_clear(vec *v);
void vec_set(vec *dst, vec *src);
intptr_t vec_hash(vec *v);
int vec_compare(vec *v1, vec *v2);

void vec_push(vec *v, intptr_t x);
intptr_t vec_sum(vec *v);
size_t vec_len(vec *v);
void vec_range(vec *v, size_t n);
void vec_ranges(vec *a, size_t n);
intptr_t vec_total(vec *a, size_t n);
vec *vec_global(void);
intptr_t vec_live(void);
size_t vec_mem(vec *v);

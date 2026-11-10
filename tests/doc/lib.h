/* The C functions of the examples of doc/index.mld, as their comments
   describe them */
#include <stddef.h>
#include <stdint.h>

typedef struct { intptr_t *data; size_t len; } vec;
void vec_init(vec *v);
void vec_clear(vec *v);
void vec_set(vec *dst, vec *src);
void vec_range(vec *v, size_t n);
void vec_push(vec *v, intptr_t x);
void vec_ranges(vec *a, size_t n);
vec *vec_global(void);

typedef struct { intptr_t *data; intptr_t rows, cols; } mat;
void mat_init(mat *m, intptr_t rows, intptr_t cols);
void mat_clear(mat *m);
void mat_set(mat *dst, mat *src);
void mat_zero(mat *m);

typedef struct db db;
typedef struct node node;
void db_close(db *d);
node *node_new(db *d, intptr_t i);
void node_free(db *d, node *n);

int make(vec *v, char **s, intptr_t n);
int is_error(int rc, intptr_t n);
const char *msg(int rc, intptr_t n);

void f(char *s, size_t len);

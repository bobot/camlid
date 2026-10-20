#include <inttypes.h>
#include <stddef.h>

typedef struct { intptr_t *data; size_t len; } vec;

void vec_init(vec *v);
void vec_clear(vec *v);
void vec_set(vec *dst, vec *src);
intptr_t vec_live(void);

/* returns -1 when b is 0 */
int checked_div(intptr_t a, intptr_t b, intptr_t *res);
int is_negative(int rc);
int is_negative_n(int rc, intptr_t n);

/* fills v and s with n elements, or returns an error code when n < 0 */
int make(vec *v, char **s, intptr_t n);
const char *error_msg(int rc, intptr_t n);

/* computation run without the runtime lock */
intptr_t slow_sum(intptr_t n);

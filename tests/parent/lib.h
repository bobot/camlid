#include <inttypes.h>
#include <stddef.h>

typedef struct { intptr_t value; intptr_t nodes; } db;
typedef struct { db *db; } stmt;
typedef struct { db *db; intptr_t id; } node;

db *db_open(intptr_t value);
void db_close(db *d);

/* a statement reads its database, which is not needed to free it */
stmt *stmt_new(db *d);
intptr_t stmt_read(stmt *s);
void stmt_free(stmt *s);

/* a node is freed with its database */
node *node_new(db *d, intptr_t id);
intptr_t node_read(node *n);
void node_free(db *d, node *n);

/* FLINT style: a context and its elements are initialized in memory given by
   the caller, the context comes last */
typedef struct { intptr_t base; intptr_t elems; } ctx;
typedef struct { intptr_t value; ctx *ctx; } elem;
void ctx_init(ctx *c);
void ctx_clear(ctx *c);
void elem_init(elem *e, ctx *c);
void elem_clear(elem *e, ctx *c);
void elem_set_si(elem *e, intptr_t v, ctx *c);
void elem_add(elem *r, elem *a, elem *b, ctx *c);
intptr_t elem_get(elem *e);

intptr_t live(void);

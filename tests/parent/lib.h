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

intptr_t live(void);

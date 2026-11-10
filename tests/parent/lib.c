#include "lib.h"
#include <stdio.h>
#include <stdlib.h>

static intptr_t count = 0;

db *db_open(intptr_t value) {
  db *d = malloc(sizeof(db));
  d->value = value; d->nodes = 0; count++;
  return d;
}
void db_close(db *d) {
  if (d->nodes) printf("error: db closed with %" PRIdPTR " nodes\n", d->nodes);
  free(d); count--;
}
stmt *stmt_new(db *d) { stmt *s = malloc(sizeof(stmt)); s->db = d; count++; return s; }
intptr_t stmt_read(stmt *s) { return s->db->value; }
void stmt_free(stmt *s) { free(s); count--; }
node *node_new(db *d, intptr_t id) {
  node *n = malloc(sizeof(node)); n->db = d; n->id = id; d->nodes++; count++;
  return n;
}
intptr_t node_read(node *n) { return n->db->value + n->id; }
void node_free(db *d, node *n) {
  if (d != n->db) printf("error: wrong db\n");
  d->nodes--; free(n); count--;
}
intptr_t live(void) { return count; }

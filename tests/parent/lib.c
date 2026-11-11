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
void ctx_init(ctx *c) { c->base = 1000; c->elems = 0; count++; }
void ctx_clear(ctx *c) {
  if (c->elems) printf("error: ctx cleared with %" PRIdPTR " elems\n", c->elems);
  count--;
}
void elem_init(elem *e, ctx *c) { e->value = 0; e->ctx = c; c->elems++; count++; }
void elem_clear(elem *e, ctx *c) {
  if (e->ctx != c) printf("error: wrong ctx\n");
  c->elems--; count--;
}
void elem_set_si(elem *e, intptr_t v, ctx *c) { e->value = c->base + v; }
void elem_add(elem *r, elem *a, elem *b, ctx *c) {
  r->value = a->value + b->value - c->base;
}
intptr_t elem_get(elem *e) { return e->value - e->ctx->base; }
intptr_t ctx_elems(ctx *c) { return c->elems; }
intptr_t live(void) { return count; }

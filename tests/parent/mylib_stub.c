// generated using generator.exe and camlid
#include <stddef.h>
#include <inttypes.h>
#include "lib.h"
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <stdio.h>
#include <string.h>
static void camlid_finalize(db* * i){ db_close(*i); }
static void camlid_finalize_op(value v){
  camlid_finalize(((db* *) Data_custom_val(v)));
  }
static struct custom_operations camlid_cops = {
NULL,
camlid_finalize_op,
custom_compare_default,
custom_hash_default,
custom_serialize_default,
custom_deserialize_default
};
static void camlid_c2ml(value * v, db* * c){
  *v = caml_alloc_custom(&camlid_cops,sizeof(db*), 0, 1);
  *(((db* *) Data_custom_val(*v))) = *c;
  }
extern value camlid_stub_db_open(intptr_t p){
  CAMLparam0();
  CAMLlocal1(vres);
  db* res;
  intptr_t p1 = ((intptr_t) { 0 });
  p1 = (p);
  res = db_open(p1);
  camlid_c2ml(&vres, &res);
  CAMLreturn(vres);
}
extern value camlid_stub_db_open_byte(value p){
  intptr_t p1;
  p1 = Long_val(p);
  return camlid_stub_db_open(p1);
  
}
static void camlid_finalize1(stmt* * i){ stmt_free(*i); }
static void camlid_finalize_op1(value v){
  camlid_finalize1(((stmt* *) Data_custom_val(v)));
  }
static struct custom_operations camlid_cops1 = {
NULL,
camlid_finalize_op1,
custom_compare_default,
custom_hash_default,
custom_serialize_default,
custom_deserialize_default
};
static void camlid_c2ml1(value * v, stmt* * c){
  *v = caml_alloc_custom(&camlid_cops1,sizeof(stmt*), 0, 1);
  *(((stmt* *) Data_custom_val(*v))) = *c;
  }
static void camlid_c2ml2(value * v, stmt* * c, value * parent){
  CAMLparam0 ();
  CAMLlocal1(cid_temp);
  camlid_c2ml1(&cid_temp, &(*c));
  *v = caml_alloc_small(2, 0);
  Field(*v, 0) = cid_temp;
  Field(*v, 1) = *parent;
  CAMLreturn0;
  }
extern value camlid_stub_stmt_new(value d){
  CAMLparam1(d);
  CAMLlocal1(vres);
  stmt* res;
  db* d1 = ((db*) { 0 });
  d1 = *(((db* *) Data_custom_val(d)));
  res = stmt_new(d1);
  camlid_c2ml2(&vres, &res, &d);
  CAMLreturn(vres);
}
static void camlid_ml2c_field(value * v, stmt* * c){
  value cid_temp = Field(*v, 0);
  (*c) = *(((stmt* *) Data_custom_val(cid_temp)));
  }
extern intptr_t camlid_stub_stmt_read(value p){
  intptr_t ures;
  intptr_t res;
  stmt* p1 = ((stmt*) { 0 });
  camlid_ml2c_field(&p, &p1);
  res = stmt_read(p1);
  ures = (res);
  return ures;
}
extern value camlid_stub_stmt_read_byte(value p){
  intptr_t ures;
  value vres;
  ures = camlid_stub_stmt_read(p);
  vres = Val_long(ures);
  return vres;
}
#include <stdatomic.h>
typedef struct { db * ptr; _Atomic intptr_t count; } camlid_rc;
static void camlid_release(camlid_rc * r){
  if(atomic_fetch_sub(&r->count, 1) == 1){ db_close(r->ptr); free(r); }
  }
static void camlid_finalize_op2(value v){
  camlid_release((*((camlid_rc **) Data_custom_val(v))));
  }
static struct custom_operations camlid_cops2 = {
NULL,
camlid_finalize_op2,
custom_compare_default,
custom_hash_default,
custom_serialize_default,
custom_deserialize_default
};
static void camlid_c2ml3(value * v, db * * c){
  camlid_rc *r = malloc(sizeof(camlid_rc));
  r->ptr = *c;
  atomic_init(&r->count, 1);
  *v = caml_alloc_custom(&camlid_cops2, sizeof(camlid_rc *), 0, 1);
  *((camlid_rc **) Data_custom_val(*v)) = r;
  }
extern value camlid_stub_rdb_open(intptr_t p){
  CAMLparam0();
  CAMLlocal1(vres);
  db * res;
  intptr_t p1 = ((intptr_t) { 0 });
  p1 = (p);
  res = db_open(p1);
  camlid_c2ml3(&vres, &res);
  CAMLreturn(vres);
}
extern value camlid_stub_rdb_open_byte(value p){
  intptr_t p1;
  p1 = Long_val(p);
  return camlid_stub_rdb_open(p1);
  
}
typedef struct { node * ptr; camlid_rc *parent; } camlid_child;
static void camlid_finalize_op3(value v){
  camlid_child *s = ((camlid_child *) Data_custom_val(v));
  node_free(s->parent->ptr, s->ptr);
  camlid_release(s->parent);
  }
static struct custom_operations camlid_cops3 = {
NULL,
camlid_finalize_op3,
custom_compare_default,
custom_hash_default,
custom_serialize_default,
custom_deserialize_default
};
static void camlid_c2ml4(value * v, node * * c, value * parent){
  camlid_rc *r = *((camlid_rc **) Data_custom_val(*parent));
  atomic_fetch_add(&r->count, 1);
  *v = caml_alloc_custom(&camlid_cops3, sizeof(camlid_child), 0, 1);
  ((camlid_child *) Data_custom_val(*v))->ptr = *c;
  ((camlid_child *) Data_custom_val(*v))->parent = r;
  }
extern value camlid_stub_node_new(value d,
  intptr_t p){
  CAMLparam1(d);
  CAMLlocal1(vres);
  node * res;
  db * d1 = ((db *) { 0 });
  intptr_t p1 = ((intptr_t) { 0 });
  d1 = (*((camlid_rc **) Data_custom_val(d)))->ptr;
  p1 = (p);
  res = node_new(d1, p1);
  camlid_c2ml4(&vres, &res, &d);
  CAMLreturn(vres);
}
extern value camlid_stub_node_new_byte(value d,
  value p){
  intptr_t p1;
  p1 = Long_val(p);
  return camlid_stub_node_new(d, p1);
  
}
extern intptr_t camlid_stub_node_read(value p){
  intptr_t ures;
  intptr_t res;
  node * p1 = ((node *) { 0 });
  p1 = ((camlid_child *) Data_custom_val(p))->ptr;
  res = node_read(p1);
  ures = (res);
  return ures;
}
extern value camlid_stub_node_read_byte(value p){
  intptr_t ures;
  value vres;
  ures = camlid_stub_node_read(p);
  vres = Val_long(ures);
  return vres;
}
static void camlid_init(ctx * * c){ *c = malloc(sizeof(ctx));ctx_init(*c); }
#include <stdatomic.h>
typedef struct { ctx * ptr; _Atomic intptr_t count; } camlid_rc1;
static void camlid_release1(camlid_rc1 * r){
  if(atomic_fetch_sub(&r->count, 1) == 1){ ctx_clear(r->ptr); free(r->ptr);
  free(r); }
  }
static void camlid_finalize_op4(value v){
  camlid_release1((*((camlid_rc1 **) Data_custom_val(v))));
  }
static struct custom_operations camlid_cops4 = {
NULL,
camlid_finalize_op4,
custom_compare_default,
custom_hash_default,
custom_serialize_default,
custom_deserialize_default
};
static void camlid_c2ml5(value * v, ctx * * c){
  camlid_rc1 *r = malloc(sizeof(camlid_rc1));
  r->ptr = *c;
  atomic_init(&r->count, 1);
  *v = caml_alloc_custom(&camlid_cops4, sizeof(camlid_rc1 *), 0, 1);
  *((camlid_rc1 **) Data_custom_val(*v)) = r;
  }
extern value camlid_stub_ctx_create(void){
  CAMLparam0();
  CAMLlocal1(p_r);
  ctx * p = ((ctx *) { 0 });
  camlid_init(&p);
  
  camlid_c2ml5(&p_r, &p);
  CAMLreturn(p_r);
}
extern intptr_t camlid_stub_live(void){
  intptr_t ures;
  intptr_t res;
  res = live();
  ures = (res);
  return ures;
}
extern value camlid_stub_live_byte(void){
  intptr_t ures;
  value vres;
  ures = camlid_stub_live();
  vres = Val_long(ures);
  return vres;
}
static void camlid_init1(ctx * parent, elem * * c){
  *c = malloc(sizeof(elem));
  elem_init(*c,
  parent);
  }
typedef struct { elem * ptr; camlid_rc1 *parent; } camlid_child1;
static void camlid_finalize_op5(value v){
  camlid_child1 *s = ((camlid_child1 *) Data_custom_val(v));
  elem_clear(s->ptr, s->parent->ptr);
  free(s->ptr);
  camlid_release1(s->parent);
  }
static struct custom_operations camlid_cops5 = {
NULL,
camlid_finalize_op5,
custom_compare_default,
custom_hash_default,
custom_serialize_default,
custom_deserialize_default
};
static void camlid_c2ml6(value * v, elem * * c, value * parent){
  camlid_rc1 *r = *((camlid_rc1 **) Data_custom_val(*parent));
  atomic_fetch_add(&r->count, 1);
  *v = caml_alloc_custom(&camlid_cops5, sizeof(camlid_child1), 0, 1);
  ((camlid_child1 *) Data_custom_val(*v))->ptr = *c;
  ((camlid_child1 *) Data_custom_val(*v))->parent = r;
  }
extern value camlid_stub_elem_of_int(value cx,
  intptr_t p){
  CAMLparam1(cx);
  CAMLlocal1(p_r);
  ctx * cx1 = ((ctx *) { 0 });
  elem * p1 = ((elem *) { 0 });
  intptr_t p2 = ((intptr_t) { 0 });
  cx1 = (*((camlid_rc1 **) Data_custom_val(cx)))->ptr;
  p2 = (p);
  camlid_init1(cx1, &p1);
  elem_set_si(p1, p2, cx1);
  camlid_c2ml6(&p_r, &p1, &cx);
  CAMLreturn(p_r);
}
extern value camlid_stub_elem_of_int_byte(value cx,
  value p){
  intptr_t p1;
  p1 = Long_val(p);
  return camlid_stub_elem_of_int(cx, p1);
  
}
extern value camlid_stub_elem_add(value cx,
  value a,
  value b){
  CAMLparam3(cx, a, b);
  CAMLlocal1(p_r);
  ctx * cx1 = ((ctx *) { 0 });
  elem * p = ((elem *) { 0 });
  elem * a1 = ((elem *) { 0 });
  elem * b1 = ((elem *) { 0 });
  cx1 = (*((camlid_rc1 **) Data_custom_val(cx)))->ptr;
  if(cx1 != (((camlid_child1 *) Data_custom_val(a))->parent->ptr)) caml_invalid_argument("elem_add: not the same context");
  if(cx1 != (((camlid_child1 *) Data_custom_val(b))->parent->ptr)) caml_invalid_argument("elem_add: not the same context");
  a1 = ((camlid_child1 *) Data_custom_val(a))->ptr;
  b1 = ((camlid_child1 *) Data_custom_val(b))->ptr;
  camlid_init1(cx1, &p);
  elem_add(p, a1, b1, cx1);
  camlid_c2ml6(&p_r, &p, &cx);
  CAMLreturn(p_r);
}
extern intptr_t camlid_stub_elem_get(value p){
  intptr_t ures;
  intptr_t res;
  elem * p1 = ((elem *) { 0 });
  p1 = ((camlid_child1 *) Data_custom_val(p))->ptr;
  res = elem_get(p1);
  ures = (res);
  return ures;
}
extern value camlid_stub_elem_get_byte(value p){
  intptr_t ures;
  value vres;
  ures = camlid_stub_elem_get(p);
  vres = Val_long(ures);
  return vres;
}
static void camlid_c2ml_parent(camlid_rc1 ** c, value * v){
  atomic_fetch_add(&(*c)->count, 1);
  *v = caml_alloc_custom(&camlid_cops4, sizeof(camlid_rc1 *), 0, 1);
  *((camlid_rc1 **) Data_custom_val(*v)) = *c;
  }
extern value camlid_stub_elem_ctx(value x){
  CAMLparam1(x);
  CAMLlocal1(vres);
  camlid_rc1 * res;
  elem * x1 = ((elem *) { 0 });
  x1 = ((camlid_child1 *) Data_custom_val(x))->ptr;
  (void) x1; res = ((camlid_child1 *) Data_custom_val(x))->parent;
  camlid_c2ml_parent(&res, &vres);
  CAMLreturn(vres);
}
extern intptr_t camlid_stub_ctx_elems(value p){
  intptr_t ures;
  intptr_t res;
  ctx * p1 = ((ctx *) { 0 });
  p1 = (*((camlid_rc1 **) Data_custom_val(p)))->ptr;
  res = ctx_elems(p1);
  ures = (res);
  return ures;
}
extern value camlid_stub_ctx_elems_byte(value p){
  intptr_t ures;
  value vres;
  ures = camlid_stub_ctx_elems(p);
  vres = Val_long(ures);
  return vres;
}


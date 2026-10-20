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
struct custom_operations camlid_cops = {
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
struct custom_operations camlid_cops1 = {
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
extern intptr_t camlid_stub_stmt_read(value p){
  intptr_t ures;
  intptr_t res;
  stmt* p1 = ((stmt*) { 0 });
  p1 = *(((stmt* *) Data_custom_val(Field(p,0))));
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
struct custom_operations camlid_cops2 = {
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
struct custom_operations camlid_cops3 = {
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


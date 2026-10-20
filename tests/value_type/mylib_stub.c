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
#include <stdio.h>
#include <string.h>
static void camlid_init(vec* * c){ *c = malloc(sizeof(vec)); vec_init(*c); }
static void camlid_finalize(vec* * i){ vec_clear(*i); free(*i); }
static void camlid_finalize_op(value v){
  camlid_finalize(((vec* *) Data_custom_val(v)));
  }
static int camlid_compare_op(value v1, value v2){
  return vec_compare(
           *((vec* *) Data_custom_val(v1)),
           *((vec* *) Data_custom_val(v2))
         );
  }
static intnat camlid_hash_op(value v){
  return vec_hash(*((vec* *) Data_custom_val(v)));;
  }
struct custom_operations camlid_cops = {
NULL,
camlid_finalize_op,
camlid_compare_op,
camlid_hash_op,
custom_serialize_default,
custom_deserialize_default
};
static void camlid_c2ml(value * v, vec* * c){
  *v = caml_alloc_custom(&camlid_cops,sizeof(vec*), 0, 1);
  *(((vec* *) Data_custom_val(*v))) = *c;
  }
extern value camlid_stub_vec_range(intptr_t p){
  CAMLparam0();
  CAMLlocal1(p_r);
  vec* p1 = ((vec*) { 0 });
  size_t p2 = ((size_t) { 0 });
  p2 = (size_t)(p);
  camlid_init(&p1);
  vec_range(p1, p2);
  camlid_c2ml(&p_r, &p1);
  CAMLreturn(p_r);
}
extern value camlid_stub_vec_range_byte(value p){
  intptr_t p1;
  p1 = Long_val(p);
  return camlid_stub_vec_range(p1);
  
}
extern value camlid_stub_vec_push(value p,
  intptr_t p1){
  CAMLparam1(p);
  vec* p2 = ((vec*) { 0 });
  intptr_t p3 = ((intptr_t) { 0 });
  p2 = *(((vec* *) Data_custom_val(p)));
  p3 = (p1);
  vec_push(p2, p3);
  CAMLreturn(Val_unit);
}
extern value camlid_stub_vec_push_byte(value p,
  value p1){
  intptr_t p2;
  p2 = Long_val(p1);
  return camlid_stub_vec_push(p, p2);
  
}
extern intptr_t camlid_stub_vec_sum(value p){
  CAMLparam1(p);
  intptr_t ures;
  intptr_t res;
  vec* p1 = ((vec*) { 0 });
  p1 = *(((vec* *) Data_custom_val(p)));
  res = vec_sum(p1);
  ures = (res);
  CAMLreturnT(intptr_t,ures);
}
extern value camlid_stub_vec_sum_byte(value p){
  intptr_t ures;
  value vres;
  ures = camlid_stub_vec_sum(p);
  vres = Val_long(ures);
  return vres;
}
static void camlid_init1(size_t array_len, vec* * c){
  *c = malloc(sizeof(vec)*array_len);
  for(size_t cid_i=0; cid_i < array_len; cid_i++){vec_init(&((*c)[cid_i]));}
  
  }
static void camlid_c2ml2(value * v, vec * c){
  vec* tmp = ((vec*) { 0 });
  camlid_init(&tmp);
  vec_set(
  *&tmp,
  c);;
  camlid_c2ml(&*v, &tmp);
  }
static void camlid_c2ml1(size_t array_len, value * v, vec* * c){
  CAMLparam0 ();
  CAMLlocal1(cid_temp);
  *v=caml_alloc(array_len,0);
  for(size_t cid_i=0; cid_i < array_len; cid_i++){
    camlid_c2ml2(&cid_temp, &((*c)[cid_i]));
    Store_field(*v,cid_i,cid_temp);
    }
  CAMLreturn0;
  }
static void camlid_free(size_t array_len, vec* c, vec* * c1){
  for(size_t cid_i=0; cid_i < array_len; cid_i++){
    vec_clear(&((*c1)[cid_i]));
    }
  free(c);
  }
extern value camlid_stub_vec_ranges(intptr_t array_len){
  CAMLparam0();
  CAMLlocal1(array_r);
  vec* array = ((vec*) { 0 });
  size_t array_len1 = ((size_t) { 0 });
  array_len1 = (size_t)(array_len);
  camlid_init1(array_len1, &array);
  vec_ranges(array, array_len1);
  camlid_c2ml1(array_len1, &array_r, &array);
  camlid_free(array_len1, array, &array);
  CAMLreturn(array_r);
}
extern value camlid_stub_vec_ranges_byte(value array_len){
  intptr_t array_len1;
  array_len1 = Long_val(array_len);
  return camlid_stub_vec_ranges(array_len1);
  
}
struct camlid_array_s { vec* t; size_t len; };
typedef struct camlid_array_s camlid_array;
static void camlid_ml2c1(value * v, vec * c){
  vec* tmp = ((vec*) { 0 });
  tmp = *(((vec* *) Data_custom_val(*v)));
  vec_init(&*c);
  vec_set(
  c,
  *&tmp);;
  }
static void camlid_ml2c(value * v, camlid_array * c){
  CAMLparam0 ();
  CAMLlocal1(cid_temp);
  c->len = Wosize_val(*v);
  c->t = malloc(sizeof(vec)*c->len);
  for(size_t cid_i=0; cid_i < c->len; cid_i++){
    cid_temp=Field(*v,cid_i);
    camlid_ml2c1(&cid_temp, &c->t[cid_i]);
    }
  CAMLreturn0;
  }
static void camlid_free1(camlid_array * c){
  for(size_t cid_i=0; cid_i < c->len; cid_i++){vec_clear(&c->t[cid_i]);}
  free(c->t);
  }
extern intptr_t camlid_stub_vec_total(value array){
  CAMLparam1(array);
  intptr_t ures;
  intptr_t res;
  camlid_array array1 = ((camlid_array) { 0 });
  camlid_ml2c(&array, &array1);
  res = vec_total(array1.t, array1.len);
  ures = (res);
  camlid_free1(&array1);
  CAMLreturnT(intptr_t,ures);
}
extern value camlid_stub_vec_total_byte(value array){
  intptr_t ures;
  value vres;
  ures = camlid_stub_vec_total(array);
  vres = Val_long(ures);
  return vres;
}
static void camlid_c2ml3(value* v, vec** c){
  vec* tmp;
  camlid_init(&tmp);
  vec_set( *&tmp, *c);
  camlid_c2ml(&*v, &tmp);
  
  }
extern value camlid_stub_vec_global(void){
  CAMLparam0();
  CAMLlocal1(vres);
  vec* res;
  res = vec_global();
  camlid_c2ml3(&vres, &res);
  CAMLreturn(vres);
}
static void camlid_init2(size_t p, vec* * c){
  *c = malloc(sizeof(vec));
  vec_init_n(*c, p);
  }
static void camlid_finalize1(vec* * i){ vec_clear(*i); free(*i); }
static void camlid_finalize_op1(value v){
  camlid_finalize1(((vec* *) Data_custom_val(v)));
  }
struct custom_operations camlid_cops1 = {
NULL,
camlid_finalize_op1,
custom_compare_default,
custom_hash_default,
custom_serialize_default,
custom_deserialize_default
};
static void camlid_c2ml4(value * v, vec* * c){
  *v = caml_alloc_custom(&camlid_cops1,sizeof(vec*), 0, 1);
  *(((vec* *) Data_custom_val(*v))) = *c;
  }
extern value camlid_stub_zeros(intptr_t p){
  CAMLparam0();
  CAMLlocal1(p_r);
  vec* p1 = ((vec*) { 0 });
  size_t p2 = ((size_t) { 0 });
  p2 = (size_t)(p);
  camlid_init2(p2, &p1);
  vec_len(p1);
  camlid_c2ml4(&p_r, &p1);
  CAMLreturn(p_r);
}
extern value camlid_stub_zeros_byte(value p){
  intptr_t p1;
  p1 = Long_val(p);
  return camlid_stub_zeros(p1);
  
}
extern intptr_t camlid_stub_zeros_len(value p){
  CAMLparam1(p);
  intptr_t ures;
  size_t res;
  vec* p1 = ((vec*) { 0 });
  p1 = *(((vec* *) Data_custom_val(p)));
  res = vec_len(p1);
  ures = (intptr_t)(res);
  CAMLreturnT(intptr_t,ures);
}
extern value camlid_stub_zeros_len_byte(value p){
  intptr_t ures;
  value vres;
  ures = camlid_stub_zeros_len(p);
  vres = Val_long(ures);
  return vres;
}
extern intptr_t camlid_stub_vec_live(void){
  CAMLparam0();
  intptr_t ures;
  intptr_t res;
  res = vec_live();
  ures = (res);
  CAMLreturnT(intptr_t,ures);
}
extern value camlid_stub_vec_live_byte(void){
  intptr_t ures;
  value vres;
  ures = camlid_stub_vec_live();
  vres = Val_long(ures);
  return vres;
}


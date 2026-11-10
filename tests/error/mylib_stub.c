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
typedef intptr_t * camlid_ref;
extern intptr_t camlid_stub_checked_div(intptr_t p,
  intptr_t p1){
  CAMLparam0();
  intptr_t p2;
  CAMLlocal1(err);
  int res;
  intptr_t p3 = ((intptr_t) { 0 });
  intptr_t p4 = ((intptr_t) { 0 });
  camlid_ref p5 = &(((struct { intptr_t a; }) { ((intptr_t) { 0 }) }).a);
  p3 = (p);
  p4 = (p1);
  res = checked_div(p3, p4, p5);
  if(is_negative(res)){
    err = caml_copy_string("checked_div failed");
    caml_failwith_value(err);
  }
  p2 = (*p5);
  CAMLreturnT(intptr_t,p2);
}
extern value camlid_stub_checked_div_byte(value p,
  value p1){
  intptr_t p2;
  value p_r;
  intptr_t p3;
  intptr_t p4;
  p3 = Long_val(p);
  p4 = Long_val(p1);
  p2 = camlid_stub_checked_div(p3, p4);
  p_r = Val_long(p2);
  return p_r;
}
static void camlid_finalize(vec* * i){ vec_clear(*i); free(*i); }
extern value camlid_vec_dispose(value v){
  vec* * p = (vec* *) Data_custom_val(v);
  if(*p != NULL){ camlid_finalize(p); }
  *p = NULL;
  return Val_unit;
}
typedef char * camlid_string_nt;
typedef camlid_string_nt * camlid_ref1;
static void camlid_init(vec* * c){ *c = malloc(sizeof(vec)); vec_init(*c); }
static void camlid_finalize_op(value v){
  if(*((vec* *) Data_custom_val(v)) != NULL){
  camlid_finalize(((vec* *) Data_custom_val(v))); }
  }
static struct custom_operations camlid_cops = {
NULL,
camlid_finalize_op,
custom_compare_default,
custom_hash_default,
custom_serialize_default,
custom_deserialize_default
};
static void camlid_c2ml(value * v, vec* * c){
  *v = caml_alloc_custom_mem(&camlid_cops,sizeof(vec*), sizeof(vec));
  *(((vec* *) Data_custom_val(*v))) = *c;
  }
extern value camlid_stub_make(intptr_t n){
  CAMLparam0();
  CAMLlocal3(ret, p_r, p_r1);
  CAMLlocal1(err);
  int res;
  vec* p = ((vec*) { 0 });
  camlid_ref1 p1 = &(((struct { camlid_string_nt a; }) { ((camlid_string_nt) { 0 }) }).a);
  intptr_t n1 = ((intptr_t) { 0 });
  n1 = (n);
  camlid_init(&p);
  res = make(p, p1, n1);
  if(is_negative_n(res, n1)){
    err = caml_copy_string(error_msg(res, n1));
    camlid_c2ml(&p_r, &p);
    free(*p1);
    /* My_error */
    const value *exn = caml_named_value("Mylib.My_error");
    if(exn == NULL) caml_invalid_argument("exception Mylib.My_error not registered");
    caml_raise_with_arg(*exn, err);
  }
  camlid_c2ml(&p_r, &p);
  p_r1 = caml_copy_string(*p1);
  ret = caml_alloc(2,0);
  Store_field(ret, 0, p_r);
  Store_field(ret, 1, p_r1);
free(*p1);
CAMLreturn(ret);
}
extern value camlid_stub_make_byte(value n){
  intptr_t n1;
  n1 = Long_val(n);
  return camlid_stub_make(n1);
  
}
extern intptr_t camlid_stub_slow_sum(intptr_t p){
  CAMLparam0();
  intptr_t ures;
  intptr_t res;
  intptr_t p1 = ((intptr_t) { 0 });
  caml_process_pending_actions();
  p1 = (p);
  caml_enter_blocking_section_no_pending();
  res = slow_sum(p1);
  caml_leave_blocking_section();
  ures = (res);
  CAMLreturnT(intptr_t,ures);
}
extern value camlid_stub_slow_sum_byte(value p){
  intptr_t ures;
  value vres;
  intptr_t p1;
  p1 = Long_val(p);
  ures = camlid_stub_slow_sum(p1);
  vres = Val_long(ures);
  return vres;
}
extern value camlid_stub_vec_signal_next_init(void){
  vec_signal_next_init();
  return Val_unit;
}
extern value camlid_stub_vec_unlocked(intptr_t p){
  CAMLparam0();
  CAMLlocal1(p_r);
  vec* p1 = ((vec*) { 0 });
  intptr_t p2 = ((intptr_t) { 0 });
  caml_process_pending_actions();
  p2 = (p);
  camlid_init(&p1);
  caml_enter_blocking_section_no_pending();
  vec_unlocked(p1, p2);
  caml_leave_blocking_section();
  camlid_c2ml(&p_r, &p1);
  CAMLreturn(p_r);
}
extern value camlid_stub_vec_unlocked_byte(value p){
  intptr_t p1;
  p1 = Long_val(p);
  return camlid_stub_vec_unlocked(p1);
  
}
extern intptr_t camlid_stub_vec_live(void){
  intptr_t ures;
  intptr_t res;
  res = vec_live();
  ures = (res);
  return ures;
}
extern value camlid_stub_vec_live_byte(void){
  intptr_t ures;
  value vres;
  ures = camlid_stub_vec_live();
  vres = Val_long(ures);
  return vres;
}
extern intptr_t camlid_stub_vec_length(value p){
  CAMLparam1(p);
  intptr_t ures;
  size_t res;
  vec* p1 = ((vec*) { 0 });
  if(*((vec* *) Data_custom_val(p)) == NULL) caml_invalid_argument("vec: used after vec_dispose");
  *&p1 = *((vec* *) Data_custom_val(p));
  res = vec_length(p1);
  ures = (intptr_t)(res);
  CAMLreturnT(intptr_t,ures);
}
extern value camlid_stub_vec_length_byte(value p){
  intptr_t ures;
  value vres;
  ures = camlid_stub_vec_length(p);
  vres = Val_long(ures);
  return vres;
}
typedef enum status * camlid_ref2;
static void camlid_c2ml1(value * v, enum status * c){
  switch(*c){
    case OK: *v = Val_long(0); break;
    case EMPTY: *v = Val_long(1); break;
    default: *v = Val_long(0);
  }
  }
extern value camlid_stub_vec_status(intptr_t p){
  CAMLparam0();
  CAMLlocal3(ret, p_r, p_r1);
  CAMLlocal1(err);
  vec* p1 = ((vec*) { 0 });
  intptr_t p2 = ((intptr_t) { 0 });
  camlid_ref2 p3 = &(((struct { enum status a; }) { ((enum status) 0) }).a);
  p2 = (p);
  camlid_init(&p1);
  vec_status(p1, p2, p3);
  if(!(*p3 == OK || *p3 == EMPTY)){
    err = caml_copy_string("unknown value of status");
    camlid_c2ml(&p_r, &p1);
    caml_failwith_value(err);
  }
  camlid_c2ml(&p_r, &p1);
  camlid_c2ml1(&p_r1, &*p3);
  ret = caml_alloc(2,0);
  Store_field(ret, 0, p_r);
  Store_field(ret, 1, p_r1);
CAMLreturn(ret);
}
extern value camlid_stub_vec_status_byte(value p){
  intptr_t p1;
  p1 = Long_val(p);
  return camlid_stub_vec_status(p1);
  
}
extern intptr_t camlid_stub_vec_live_alloc(void){
  CAMLparam0();
  intptr_t ures;
  intptr_t res;
  res = vec_live();
  ures = (res);
  CAMLreturnT(intptr_t,ures);
}
extern value camlid_stub_vec_live_alloc_byte(void){
  intptr_t ures;
  value vres;
  ures = camlid_stub_vec_live_alloc();
  vres = Val_long(ures);
  return vres;
}


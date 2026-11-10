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
static void camlid_finalize_op(value v){
  finalize_ptr(((int * *) Data_custom_val(v)));
  }
static int camlid_compare_op(value v1, value v2){
  return compare_ptr(
           ((int * *) Data_custom_val(v1)),
           ((int * *) Data_custom_val(v2))
         );
  }
static intnat camlid_hash_op(value v){
  return hash_ptr(((int * *) Data_custom_val(v)));;
  }
static struct custom_operations camlid_cops = {
NULL,
camlid_finalize_op,
camlid_compare_op,
camlid_hash_op,
custom_serialize_default,
custom_deserialize_default
};
static void camlid_c2ml(value * v, int * * c){
  *v = caml_alloc_custom(&camlid_cops,sizeof(int *), 0, 1);
  *(((int * *) Data_custom_val(*v))) = *c;
  }
extern value camlid_stub_of_int(intptr_t p){
  CAMLparam0();
  CAMLlocal1(p_r);
  int p1 = ((int) { 0 });
  int * p2 = ((int *) { 0 });
  p1 = (int)(p);
  initialize_ptr(&p2);
  of_int(p1, p2);
  camlid_c2ml(&p_r, &p2);
  CAMLreturn(p_r);
}
extern value camlid_stub_of_int_byte(value p){
  intptr_t p1;
  p1 = Int_val(p);
  return camlid_stub_of_int(p1);
  
}
extern intptr_t camlid_stub_to_int(value p){
  intptr_t ures;
  int res;
  int * p1 = ((int *) { 0 });
  p1 = *(((int * *) Data_custom_val(p)));
  res = to_int(p1);
  ures = (intptr_t)(res);
  return ures;
}
extern value camlid_stub_to_int_byte(value p){
  intptr_t ures;
  value vres;
  ures = camlid_stub_to_int(p);
  vres = Val_int(ures);
  return vres;
}
extern value camlid_stub_two(intptr_t p){
  CAMLparam0();
  CAMLlocal3(ret, p_r, p_r1);
  int p1 = ((int) { 0 });
  int * p2 = ((int *) { 0 });
  int * p3 = ((int *) { 0 });
  p1 = (int)(p);
  initialize_ptr(&p2);
  initialize_ptr(&p3);
  two(p1, p2, p3);
  camlid_c2ml(&p_r, &p2);
  camlid_c2ml(&p_r1, &p3);
  ret = caml_alloc(2,0);
  Store_field(ret, 0, p_r);
  Store_field(ret, 1, p_r1);
CAMLreturn(ret);
}
extern value camlid_stub_two_byte(value p){
  intptr_t p1;
  p1 = Int_val(p);
  return camlid_stub_two(p1);
  
}


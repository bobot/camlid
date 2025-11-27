// generated using generator.exe and camlid
#include <stddef.h>
#include <inttypes.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <stdio.h>
#include <string.h>
#include "lib.h"
typedef intptr_t camlid_int;
static void camlid_ml2u(value * v, camlid_int * c){ *c = Int_val(*v); }
typedef int camlid_int1;
typedef int * camlid_custom;
static void camlid_u2c(camlid_int1 * c, camlid_int * c1){ *c = (int)(*c1); }
static void camlid_init(camlid_custom * c){ initialize_ptr(c); }
static void camlid_finalize_op(value v){
  finalize_ptr((camlid_custom *) Data_custom_val(v));
  }
static int camlid_compare_op(value v1, value v2){
  return compare_ptr(
           (camlid_custom *) Data_custom_val(v1),
           (camlid_custom *) Data_custom_val(v2)
         );
  }
static intptr_t camlid_hash_op(value v){
  return hash_ptr((camlid_custom *) Data_custom_val(v));
  }
struct custom_operations camlid_cops = {
NULL,
camlid_finalize_op,
camlid_compare_op,
camlid_hash_op,
custom_serialize_default,
custom_deserialize_default
};
static void camlid_c2ml(value * v, camlid_custom * c){
  *v = caml_alloc_custom(&camlid_cops,sizeof(camlid_custom), 0, 1);
  *((camlid_custom *) Data_custom_val(*v)) = *c;
  }
extern value camlid_stub_of_int(camlid_int p){
  CAMLparam0();
  CAMLlocal1(p_r);
  camlid_int1 p1 = ((camlid_int1) { 0 });
  camlid_custom p2 = ((camlid_custom) { 0 });
  camlid_u2c(&p1, &p);
  camlid_init(&p2);
  of_int(p1, p2);
  camlid_c2ml(&p_r, &p2);
  CAMLreturn(p_r);
}
extern value camlid_stub_of_int_byte(value p){
  camlid_int p1;
  camlid_ml2u(&p, &p1);
  return camlid_stub_of_int(p1);
  
}
static void camlid_ml2c(value * v, camlid_custom * c){
  *c = *((camlid_custom *) Data_custom_val(*v));
  }
static void camlid_c2u(camlid_int1 * c, camlid_int * c1){
  *c1 = (intptr_t)(*c);
  }
extern camlid_int camlid_stub_to_int(value p){
  CAMLparam1(p);
  camlid_int ures;
  camlid_int1 res;
  camlid_custom p1 = ((camlid_custom) { 0 });
  camlid_ml2c(&p, &p1);
  res = to_int(p1);
  camlid_c2u(&res, &ures);
  CAMLreturnT(camlid_int,ures);
}
static void camlid_u2ml(value * v, camlid_int * c){ *v = Val_int(*c); }
extern value camlid_stub_to_int_byte(value p){
  camlid_int ures;
  value vres;
  ures = camlid_stub_to_int(p);
  camlid_u2ml(&vres, &ures);
  return vres;
}

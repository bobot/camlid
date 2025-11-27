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
typedef int * camlid_abstract;
static void camlid_u2c(camlid_int1 * c, camlid_int * c1){ *c = (int)(*c1); }
void lib_init(camlid_abstract *);
static void camlid_init(camlid_abstract * c){ lib_init(c); }
static void camlid_c2ml(value * v, camlid_abstract * c){
  *v = caml_alloc((sizeof(camlid_abstract) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((camlid_abstract *) Bp_val(*v)) = *c;
  }
extern value camlid_stub_of_int(camlid_int p){
  CAMLparam0();
  CAMLlocal1(p_r);
  camlid_int1 p1 = ((camlid_int1) { 0 });
  camlid_abstract p2 = ((camlid_abstract) { 0 });
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
static void camlid_ml2c(value * v, camlid_abstract * c){
  *c = *((camlid_abstract *) Bp_val(*v));
  }
static void camlid_c2u(camlid_int1 * c, camlid_int * c1){
  *c1 = (intptr_t)(*c);
  }
extern camlid_int camlid_stub_to_int(value p){
  CAMLparam1(p);
  camlid_int ures;
  camlid_int1 res;
  camlid_abstract p1 = ((camlid_abstract) { 0 });
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
extern value camlid_stub_lib_free(value p){
  CAMLparam1(p);
  camlid_abstract p1 = ((camlid_abstract) { 0 });
  camlid_ml2c(&p, &p1);
  lib_free(p1);
  CAMLreturn(Val_unit);
}

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
typedef int * camlid_abstract;
static void camlid_c2ml(value * v, camlid_abstract * c){
  *v = caml_alloc((sizeof(camlid_abstract) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((camlid_abstract *) Bp_val(*v)) = *c;
  }
extern value camlid_stub_of_int(intptr_t p){
  CAMLparam0();
  CAMLlocal1(p_r);
  int p1 = ((int) { 0 });
  camlid_abstract p2 = ((camlid_abstract) { 0 });
  p1 = (int)(p);
  lib_init(&p2);
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
  CAMLparam1(p);
  intptr_t ures;
  int res;
  camlid_abstract p1 = ((camlid_abstract) { 0 });
  p1 = *((camlid_abstract *) Bp_val(p));
  res = to_int(p1);
  ures = (intptr_t)(res);
  CAMLreturnT(intptr_t,ures);
}
extern value camlid_stub_to_int_byte(value p){
  intptr_t ures;
  value vres;
  ures = camlid_stub_to_int(p);
  vres = Val_int(ures);
  return vres;
}
extern value camlid_stub_lib_free(value p){
  CAMLparam1(p);
  camlid_abstract p1 = ((camlid_abstract) { 0 });
  p1 = *((camlid_abstract *) Bp_val(p));
  lib_free(p1);
  CAMLreturn(Val_unit);
}


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
static void camlid_c2u(intptr_t * c, intptr_t * c1){ *c1 = (*c); }
extern intptr_t camlid_stub_f(){
  CAMLparam0();
  intptr_t ures;
  intptr_t res;
  res = f();
  camlid_c2u(&res, &ures);
  CAMLreturnT(intptr_t,ures);
}
static void camlid_u2ml(value * v, intptr_t * c){ *v = Val_long(*c); }
extern value camlid_stub_f_byte(){
  intptr_t ures;
  value vres;
  ures = camlid_stub_f();
  camlid_u2ml(&vres, &ures);
  return vres;
}

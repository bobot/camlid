// generated using generator.exe and camlid
#include <stddef.h>
#include <inttypes.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <string.h>
#include "lib.h"
typedef intptr_t camlid_int;
static void camlid_c2u(camlid_int * c, camlid_int * c1){ *c1 = (*c); }
extern camlid_int camlid_stub_f(){
  CAMLparam0();
  camlid_int ures;
  camlid_int res;
  res = f();
  camlid_c2u(&res, &ures);
  CAMLreturnT(camlid_int,ures);
}
static void camlid_u2ml(value * v, camlid_int * c){ *v = Val_long(*c); }
extern value camlid_stub_f_byte(){
  camlid_int ures;
  value vres;
  ures = camlid_stub_f();
  camlid_u2ml(&vres, &ures);
  return vres;
}

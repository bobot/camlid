// generated using generator.exe and camlid
#include <stddef.h>
#include <inttypes.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <string.h>
#include "./lib.h"
typedef int camlid_int;
static void camlid_ml2c(value * v, camlid_int * c){ *c = Int_val(*v); };
extern value camlid_stub_f(value p){
  CAMLparam1(p);
  CAMLlocal1(ret);
  camlid_int p1 = ((camlid_int) { });
  camlid_ml2c(&p, &p1);
  f(p1);
  ret = Val_unit;
  CAMLreturn(ret);
};

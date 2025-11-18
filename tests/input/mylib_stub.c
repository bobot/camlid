#include <stddef.h>
#include <inttypes.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include "./lib.h"
typedef int camlid_int;
static void camlid_ml2c(value * v, camlid_int * c){ *c = Int_val(*v); };
static void camlid_free(camlid_int * c){  };
extern value camlid_stub_f(value p){
  camlid_int p1 = ((camlid_int) { });
  value ret;
  camlid_ml2c(&p, &p1);
  f(p1);
  ret = Val_unit;
  camlid_free(&p1);
  return ret;
};

#include <stddef.h>
#include <inttypes.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include "lib.h"
typedef intptr_t camlid_int;
static void camlid_c2ml(value * v, camlid_int * c){ *v = Val_long(*c); };
extern value camlid_stub_f(){
  camlid_int res;
  value ret;
  res = f();
  camlid_c2ml(&ret, &res);
  return ret;
};

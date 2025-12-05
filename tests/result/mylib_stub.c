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
extern intptr_t camlid_stub_f(void){
  CAMLparam0();
  intptr_t ures;
  intptr_t res;
  res = f();
  ures = (res);
  CAMLreturnT(intptr_t,ures);
}
extern value camlid_stub_f_byte(void){
  intptr_t ures;
  value vres;
  ures = camlid_stub_f();
  vres = Val_long(ures);
  return vres;
}


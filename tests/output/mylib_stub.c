// generated using generator.exe and camlid
#include <stddef.h>
#include <inttypes.h>
#include "lib.h"
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <stdio.h>
#include <string.h>
typedef int * camlid_ref;
extern intptr_t camlid_stub_f(void){
  CAMLparam0();
  intptr_t p;
  camlid_ref p1 = &(((struct { int a; }) { ((int) { 0 }) }).a);
  f(p1);
  p = (intptr_t)(*p1);
  CAMLreturnT(intptr_t,p);
}
extern value camlid_stub_f_byte(void){
  intptr_t p;
  value p_r;
  p = camlid_stub_f();
  p_r = Val_int(p);
  return p_r;
}
typedef int * camlid_ref1;
typedef int * camlid_ref2;
extern value camlid_stub_f2(void){
  CAMLparam0();
  CAMLlocal3(ret, p_r, p_r1);
  camlid_ref1 p = &(((struct { int a; }) { ((int) { 0 }) }).a);
  camlid_ref2 p1 = &(((struct { int a; }) { ((int) { 0 }) }).a);
  f2(p, p1);
  p_r = Val_int(*p);
  p_r1 = Val_int(*p1);
  ret = caml_alloc(2,0);
  Store_field(ret, 0, p_r);
  Store_field(ret, 1, p_r1);
CAMLreturn(ret);
}


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
typedef int * camlid_ref;
static void camlid_c2u1(int * c, intptr_t * c1){ *c1 = (intptr_t)(*c); }
static void camlid_c2u(intptr_t * c, camlid_ref * c1){ camlid_c2u1(*c1, c); }
extern intptr_t camlid_stub_f(){
  CAMLparam0();
  intptr_t p;
  camlid_ref p1 = &(((struct { int a; }) { ((int) { 0 }) }).a);
  f(p1);
  camlid_c2u(&p, &p1);
  CAMLreturnT(intptr_t,p);
}
static void camlid_u2ml(value * v, intptr_t * c){ *v = Val_int(*c); }
extern value camlid_stub_f_byte(){
  intptr_t p;
  value p_r;
  p = camlid_stub_f();
  camlid_u2ml(&p_r, &p);
  return p_r;
}
typedef int * camlid_ref1;
typedef int * camlid_ref2;
static void camlid_c2ml2(value * v, int * c){ *v = Val_int(*c); }
static void camlid_c2ml(value * v, camlid_ref1 * c){ camlid_c2ml2(v, *c); }
static void camlid_c2ml1(value * v, camlid_ref2 * c){ camlid_c2ml2(v, *c); }
extern value camlid_stub_f2(){
  CAMLparam0();
  CAMLlocal3(ret, p_r, p_r1);
  camlid_ref1 p = &(((struct { int a; }) { ((int) { 0 }) }).a);
  camlid_ref2 p1 = &(((struct { int a; }) { ((int) { 0 }) }).a);
  f2(p, p1);
  camlid_c2ml(&p_r, &p);
  camlid_c2ml1(&p_r1, &p1);
  ret = caml_alloc(2,0);
  Store_field(ret, 0, p_r);
  Store_field(ret, 1, p_r1);
CAMLreturn(ret);
}

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
typedef int camlid_int1;
typedef camlid_int1 * camlid_ref;
static void camlid_c2u1(camlid_int1 * c, camlid_int * c1){
  *c1 = (intptr_t)(*c);
  }
static void camlid_c2u(camlid_int * c, camlid_ref * c1){
  camlid_c2u1(*c1, c);
  }
extern camlid_int camlid_stub_f(){
  CAMLparam0();
  camlid_int p;
  camlid_ref p1 = &(((struct { camlid_int1 a; }) { ((camlid_int1) { 0 }) }).a);
  f(p1);
  camlid_c2u(&p, &p1);
  CAMLreturnT(camlid_int,p);
}
static void camlid_u2ml(value * v, camlid_int * c){ *v = Val_int(*c); }
extern value camlid_stub_f_byte(){
  camlid_int p;
  value p_r;
  p = camlid_stub_f();
  camlid_u2ml(&p_r, &p);
  return p_r;
}
typedef camlid_int1 * camlid_ref1;
typedef camlid_int1 * camlid_ref2;
static void camlid_c2ml2(value * v, camlid_int1 * c){ *v = Val_int(*c); }
static void camlid_c2ml(value * v, camlid_ref1 * c){ camlid_c2ml2(v, *c); }
static void camlid_c2ml1(value * v, camlid_ref2 * c){ camlid_c2ml2(v, *c); }
extern value camlid_stub_f2(){
  CAMLparam0();
  CAMLlocal3(ret, p_r, p_r1);
  camlid_ref1 p = &(((struct { camlid_int1 a; }) { ((camlid_int1) { 0 }) }).a);
  camlid_ref2 p1 = &(((struct { camlid_int1 a; }) { ((camlid_int1) { 0 }) }).a);
  f2(p, p1);
  camlid_c2ml(&p_r, &p);
  camlid_c2ml1(&p_r1, &p1);
  ret = caml_alloc(2,0);
  Store_field(ret, 0, p_r);
  Store_field(ret, 1, p_r1);
CAMLreturn(ret);
}

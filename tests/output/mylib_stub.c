// generated using generator.exe and camlid
#include <stddef.h>
#include <inttypes.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <string.h>
#include "lib.h"
typedef int camlid_int;
typedef camlid_int * camlid_ref;
static void camlid_c2ml1(value * v, camlid_int * c){ *v = Val_int(*c); };
static void camlid_c2ml(value * v, camlid_ref * c){ camlid_c2ml1(v, *c); };
extern value camlid_stub_f(){
  CAMLparam0();
  CAMLlocal1(p_r);
  camlid_ref p = &(((struct { camlid_int a; }) { ((camlid_int) { }) }).a);
  f(p);
  camlid_c2ml(&p_r, &p);
  CAMLreturnT(value,p_r);
};
typedef camlid_int * camlid_ref1;
typedef camlid_int * camlid_ref2;
static void camlid_c2ml2(value * v, camlid_ref1 * c){ camlid_c2ml1(v, *c); };
static void camlid_c2ml3(value * v, camlid_ref2 * c){ camlid_c2ml1(v, *c); };
extern value camlid_stub_f2(){
  CAMLparam0();
  CAMLlocal3(ret, p_r, p_r1);
  camlid_ref1 p = &(((struct { camlid_int a; }) { ((camlid_int) { }) }).a);
  camlid_ref2 p1 = &(((struct { camlid_int a; }) { ((camlid_int) { }) }).a);
  f2(p, p1);
  camlid_c2ml2(&p_r, &p);
  camlid_c2ml3(&p_r1, &p1);
  ret = caml_alloc(2,0);
  Store_field(ret, 0, p_r);
  Store_field(ret, 1, p_r1);
CAMLreturn(ret);
};

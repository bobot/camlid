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
typedef int * camlid_abstract;
static void camlid_ml2c(value * v, camlid_int * c){ *c = Long_val(*v); };
void lib_init(camlid_abstract *);
static void camlid_init(camlid_abstract * c){ lib_init(c); };
static void camlid_c2ml(value * v, camlid_abstract * c){
  *v = caml_alloc((sizeof(camlid_abstract) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((camlid_abstract *) Bp_val(*v)) = *c;
  };
extern value camlid_stub_of_int(value p){
  CAMLparam1(p);
  CAMLlocal2(ret, p_r);
  camlid_int p1 = ((camlid_int) { });
  camlid_abstract p2 = ((camlid_abstract) { });
  camlid_ml2c(&p, &p1);
  camlid_init(&p2);
  of_int(p1, p2);
  camlid_c2ml(&p_r, &p2);
  ret = p_r;
  CAMLreturn(ret);
};
static void camlid_ml2c1(value * v, camlid_abstract * c){
  *c = *((camlid_abstract *) Bp_val(*v));
  };
static void camlid_c2ml1(value * v, camlid_int * c){ *v = Val_long(*c); };
extern value camlid_stub_to_int(value p){
  CAMLparam1(p);
  CAMLlocal2(ret, vres);
  camlid_int res;
  camlid_abstract p1 = ((camlid_abstract) { });
  camlid_ml2c1(&p, &p1);
  res = to_int(p1);
  camlid_c2ml1(&vres, &res);
  ret = vres;
  CAMLreturn(ret);
};
extern value camlid_stub_lib_free(value p){
  CAMLparam1(p);
  CAMLlocal1(ret);
  camlid_abstract p1 = ((camlid_abstract) { });
  camlid_ml2c1(&p, &p1);
  lib_free(p1);
  ret = Val_unit;
  CAMLreturn(ret);
};

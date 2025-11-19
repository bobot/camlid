// generated using generator.exe and camlid
#include <stddef.h>
#include <inttypes.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
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
static void camlid_free(camlid_int * c){  };
static void camlid_free1(camlid_abstract * c){  };
extern value camlid_stub_of_int(value p){
  camlid_int p1 = ((camlid_int) { });
  camlid_abstract p2 = ((camlid_abstract) { });
  value ret;
  camlid_ml2c(&p, &p1);
  camlid_init(&p2);
  of_int(p1, p2);
  camlid_c2ml(&ret, &p2);
  camlid_free(&p1);
  camlid_free1(&p2);
  return ret;
};
static void camlid_ml2c1(value * v, camlid_abstract * c){
  *c = *((camlid_abstract *) Bp_val(*v));
  };
static void camlid_c2ml1(value * v, camlid_int * c){ *v = Val_long(*c); };
extern value camlid_stub_to_int(value p){
  camlid_abstract p1 = ((camlid_abstract) { });
  camlid_int res;
  value ret;
  camlid_ml2c1(&p, &p1);
  res = to_int(p1);
  camlid_c2ml1(&ret, &res);
  camlid_free1(&p1);
  return ret;
};
extern value camlid_stub_lib_free(value p){
  camlid_abstract p1 = ((camlid_abstract) { });
  value ret;
  camlid_ml2c1(&p, &p1);
  lib_free(p1);
  ret = Val_unit;
  camlid_free1(&p1);
  return ret;
};

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
  camlid_int p1 = ((camlid_int) { });
  camlid_ml2c(&p, &p1);
  f(p1);
  CAMLreturn(Val_unit);
};
extern value camlid_stub_f7(value p,
  value p1,
  value p2,
  value p3,
  value p4,
  value p5,
  value p6){
  CAMLparam5(p, p1, p2, p3, p4);
  CAMLxparam2(p5, p6);
  camlid_int p7 = ((camlid_int) { });
  camlid_int p8 = ((camlid_int) { });
  camlid_int p9 = ((camlid_int) { });
  camlid_int p10 = ((camlid_int) { });
  camlid_int p11 = ((camlid_int) { });
  camlid_int p12 = ((camlid_int) { });
  camlid_int p13 = ((camlid_int) { });
  camlid_ml2c(&p, &p7);
  camlid_ml2c(&p1, &p8);
  camlid_ml2c(&p2, &p9);
  camlid_ml2c(&p3, &p10);
  camlid_ml2c(&p4, &p11);
  camlid_ml2c(&p5, &p12);
  camlid_ml2c(&p6, &p13);
  f7(p7, p8, p9, p10, p11, p12, p13);
  CAMLreturn(Val_unit);
};
extern value camlid_stub_f7_byte(value * argv, int argn){
  value p = argv[0];
  value p1 = argv[1];
  value p2 = argv[2];
  value p3 = argv[3];
  value p4 = argv[4];
  value p5 = argv[5];
  value p6 = argv[6];
  return camlid_stub_f7(p, p1, p2, p3, p4, p5, p6);
  
};

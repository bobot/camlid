// generated using generator.exe and camlid
#include <stddef.h>
#include <inttypes.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <string.h>
#include "./lib.h"
typedef intptr_t camlid_int;
static void camlid_ml2u(value * v, camlid_int * c){ *c = Int_val(*v); }
typedef int camlid_int1;
static void camlid_u2c(camlid_int1 * c, camlid_int * c1){ *c = (int)(*c1); }
extern value camlid_stub_f(camlid_int p){
  CAMLparam0();
  camlid_int1 p1 = ((camlid_int1) { });
  camlid_u2c(&p1, &p);
  f(p1);
  CAMLreturn(Val_unit);
}
extern value camlid_stub_f_byte(value p){
  camlid_int p1;
  camlid_ml2u(&p, &p1);
  return camlid_stub_f(p1);
  
}
extern value camlid_stub_f7(camlid_int p,
  camlid_int p1,
  camlid_int p2,
  camlid_int p3,
  camlid_int p4,
  camlid_int p5,
  camlid_int p6){
  CAMLparam0();
  camlid_int1 p7 = ((camlid_int1) { });
  camlid_int1 p8 = ((camlid_int1) { });
  camlid_int1 p9 = ((camlid_int1) { });
  camlid_int1 p10 = ((camlid_int1) { });
  camlid_int1 p11 = ((camlid_int1) { });
  camlid_int1 p12 = ((camlid_int1) { });
  camlid_int1 p13 = ((camlid_int1) { });
  camlid_u2c(&p7, &p);
  camlid_u2c(&p8, &p1);
  camlid_u2c(&p9, &p2);
  camlid_u2c(&p10, &p3);
  camlid_u2c(&p11, &p4);
  camlid_u2c(&p12, &p5);
  camlid_u2c(&p13, &p6);
  f7(p7, p8, p9, p10, p11, p12, p13);
  CAMLreturn(Val_unit);
}
extern value camlid_stub_f7_byte(value * argv, int argn){
  value p = argv[0];
  value p1 = argv[1];
  value p2 = argv[2];
  value p3 = argv[3];
  value p4 = argv[4];
  value p5 = argv[5];
  value p6 = argv[6];
  camlid_int p7;
  camlid_int p8;
  camlid_int p9;
  camlid_int p10;
  camlid_int p11;
  camlid_int p12;
  camlid_int p13;
  camlid_ml2u(&p, &p7);
  camlid_ml2u(&p1, &p8);
  camlid_ml2u(&p2, &p9);
  camlid_ml2u(&p3, &p10);
  camlid_ml2u(&p4, &p11);
  camlid_ml2u(&p5, &p12);
  camlid_ml2u(&p6, &p13);
  return camlid_stub_f7(p7, p8, p9, p10, p11, p12, p13);
  
}

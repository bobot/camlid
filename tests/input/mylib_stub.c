// generated using generator.exe and camlid
#include <stddef.h>
#include <inttypes.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <stdio.h>
#include <string.h>
#include "./lib.h"
static void camlid_ml2u(value * v, intptr_t * c){ *c = Int_val(*v); }
static void camlid_u2c(int * c, intptr_t * c1){ *c = (int)(*c1); }
extern value camlid_stub_f(intptr_t p){
  CAMLparam0();
  int p1 = ((int) { 0 });
  camlid_u2c(&p1, &p);
  f(p1);
  CAMLreturn(Val_unit);
}
extern value camlid_stub_f_byte(value p){
  intptr_t p1;
  camlid_ml2u(&p, &p1);
  return camlid_stub_f(p1);
  
}
extern value camlid_stub_f7(intptr_t p,
  intptr_t p1,
  intptr_t p2,
  intptr_t p3,
  intptr_t p4,
  intptr_t p5,
  intptr_t p6){
  CAMLparam0();
  int p7 = ((int) { 0 });
  int p8 = ((int) { 0 });
  int p9 = ((int) { 0 });
  int p10 = ((int) { 0 });
  int p11 = ((int) { 0 });
  int p12 = ((int) { 0 });
  int p13 = ((int) { 0 });
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
  intptr_t p7;
  intptr_t p8;
  intptr_t p9;
  intptr_t p10;
  intptr_t p11;
  intptr_t p12;
  intptr_t p13;
  camlid_ml2u(&p, &p7);
  camlid_ml2u(&p1, &p8);
  camlid_ml2u(&p2, &p9);
  camlid_ml2u(&p3, &p10);
  camlid_ml2u(&p4, &p11);
  camlid_ml2u(&p5, &p12);
  camlid_ml2u(&p6, &p13);
  return camlid_stub_f7(p7, p8, p9, p10, p11, p12, p13);
  
}

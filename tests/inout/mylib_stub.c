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
static void camlid_ml2u(value * v, intptr_t * c){ *c = Long_val(*v); }
typedef intptr_t * camlid_ref;
static void camlid_u2c1(intptr_t * c, intptr_t * c1){ *c = (*c1); }
static void camlid_u2c(intptr_t * c, camlid_ref * c1){ camlid_u2c1(*c1, c); }
static void camlid_c2u1(intptr_t * c, intptr_t * c1){ *c1 = (*c); }
static void camlid_c2u(intptr_t * c, camlid_ref * c1){ camlid_c2u1(*c1, c); }
extern intptr_t camlid_stub_f_nat(intptr_t p){
  CAMLparam0();
  intptr_t p1;
  camlid_ref p2 = &(((struct { intptr_t a; }) { ((intptr_t) { 0 }) }).a);
  camlid_u2c(&p, &p2);
  f_nat(p2);
  camlid_c2u(&p1, &p2);
  CAMLreturnT(intptr_t,p1);
}
static void camlid_u2ml(value * v, intptr_t * c){ *v = Val_long(*c); }
extern value camlid_stub_f_nat_byte(value p){
  intptr_t p1;
  value p_r;
  intptr_t p2;
  camlid_ml2u(&p, &p2);
  p1 = camlid_stub_f_nat(p2);
  camlid_u2ml(&p_r, &p1);
  return p_r;
}
static void camlid_ml2u1(value * v, intptr_t * c){ *c = Int_val(*v); }
typedef int * camlid_ref1;
static void camlid_u2c3(int * c, intptr_t * c1){ *c = (int)(*c1); }
static void camlid_u2c2(intptr_t * c, camlid_ref1 * c1){
  camlid_u2c3(*c1, c);
  }
static void camlid_c2u3(int * c, intptr_t * c1){ *c1 = (intptr_t)(*c); }
static void camlid_c2u2(intptr_t * c, camlid_ref1 * c1){
  camlid_c2u3(*c1, c);
  }
extern intptr_t camlid_stub_f_int(intptr_t p){
  CAMLparam0();
  intptr_t p1;
  camlid_ref1 p2 = &(((struct { int a; }) { ((int) { 0 }) }).a);
  camlid_u2c2(&p, &p2);
  f_int(p2);
  camlid_c2u2(&p1, &p2);
  CAMLreturnT(intptr_t,p1);
}
static void camlid_u2ml1(value * v, intptr_t * c){ *v = Val_int(*c); }
extern value camlid_stub_f_int_byte(value p){
  intptr_t p1;
  value p_r;
  intptr_t p2;
  camlid_ml2u1(&p, &p2);
  p1 = camlid_stub_f_int(p2);
  camlid_u2ml1(&p_r, &p1);
  return p_r;
}
static void camlid_ml2u2(value * v, double * c){ *c = Double_val(*v); }
typedef double * camlid_ref2;
static void camlid_u2c5(double * c, double * c1){ *c = (*c1); }
static void camlid_u2c4(double * c, camlid_ref2 * c1){ camlid_u2c5(*c1, c); }
static void camlid_c2u5(double * c, double * c1){ *c1 = (*c); }
static void camlid_c2u4(double * c, camlid_ref2 * c1){ camlid_c2u5(*c1, c); }
extern double camlid_stub_f_double(double p){
  CAMLparam0();
  double p1;
  camlid_ref2 p2 = &(((struct { double a; }) { ((double) { 0 }) }).a);
  camlid_u2c4(&p, &p2);
  f_double(p2);
  camlid_c2u4(&p1, &p2);
  CAMLreturnT(double,p1);
}
static void camlid_u2ml2(value * v, double * c){ *v = caml_copy_double(*c); }
extern value camlid_stub_f_double_byte(value p){
  double p1;
  value p_r;
  double p2;
  camlid_ml2u2(&p, &p2);
  p1 = camlid_stub_f_double(p2);
  camlid_u2ml2(&p_r, &p1);
  return p_r;
}
static void camlid_ml2u3(value * v, int32_t * c){ *c = Int32_val(*v); }
typedef int32_t * camlid_ref3;
static void camlid_u2c7(int32_t * c, int32_t * c1){ *c = (*c1); }
static void camlid_u2c6(int32_t * c, camlid_ref3 * c1){
  camlid_u2c7(*c1, c);
  }
static void camlid_c2u7(int32_t * c, int32_t * c1){ *c1 = (*c); }
static void camlid_c2u6(int32_t * c, camlid_ref3 * c1){
  camlid_c2u7(*c1, c);
  }
extern int32_t camlid_stub_f_int32(int32_t p){
  CAMLparam0();
  int32_t p1;
  camlid_ref3 p2 = &(((struct { int32_t a; }) { ((int32_t) { 0 }) }).a);
  camlid_u2c6(&p, &p2);
  f_int32(p2);
  camlid_c2u6(&p1, &p2);
  CAMLreturnT(int32_t,p1);
}
static void camlid_u2ml3(value * v, int32_t * c){ *v = caml_copy_int32(*c); }
extern value camlid_stub_f_int32_byte(value p){
  int32_t p1;
  value p_r;
  int32_t p2;
  camlid_ml2u3(&p, &p2);
  p1 = camlid_stub_f_int32(p2);
  camlid_u2ml3(&p_r, &p1);
  return p_r;
}
static void camlid_ml2u4(value * v, int64_t * c){ *c = Int64_val(*v); }
typedef int64_t * camlid_ref4;
static void camlid_u2c9(int64_t * c, int64_t * c1){ *c = (*c1); }
static void camlid_u2c8(int64_t * c, camlid_ref4 * c1){
  camlid_u2c9(*c1, c);
  }
static void camlid_c2u9(int64_t * c, int64_t * c1){ *c1 = (*c); }
static void camlid_c2u8(int64_t * c, camlid_ref4 * c1){
  camlid_c2u9(*c1, c);
  }
extern int64_t camlid_stub_f_int64(int64_t p){
  CAMLparam0();
  int64_t p1;
  camlid_ref4 p2 = &(((struct { int64_t a; }) { ((int64_t) { 0 }) }).a);
  camlid_u2c8(&p, &p2);
  f_int64(p2);
  camlid_c2u8(&p1, &p2);
  CAMLreturnT(int64_t,p1);
}
static void camlid_u2ml4(value * v, int64_t * c){ *v = caml_copy_int64(*c); }
extern value camlid_stub_f_int64_byte(value p){
  int64_t p1;
  value p_r;
  int64_t p2;
  camlid_ml2u4(&p, &p2);
  p1 = camlid_stub_f_int64(p2);
  camlid_u2ml4(&p_r, &p1);
  return p_r;
}
static void camlid_ml2u5(value * v, intptr_t * c){ *c = Nativeint_val(*v); }
typedef intptr_t * camlid_ref5;
static void camlid_u2c11(intptr_t * c, intptr_t * c1){ *c = (*c1); }
static void camlid_u2c10(intptr_t * c, camlid_ref5 * c1){
  camlid_u2c11(*c1, c);
  }
static void camlid_c2u11(intptr_t * c, intptr_t * c1){ *c1 = (*c); }
static void camlid_c2u10(intptr_t * c, camlid_ref5 * c1){
  camlid_c2u11(*c1, c);
  }
extern intptr_t camlid_stub_f_nat1(intptr_t p){
  CAMLparam0();
  intptr_t p1;
  camlid_ref5 p2 = &(((struct { intptr_t a; }) { ((intptr_t) { 0 }) }).a);
  camlid_u2c10(&p, &p2);
  f_nat(p2);
  camlid_c2u10(&p1, &p2);
  CAMLreturnT(intptr_t,p1);
}
static void camlid_u2ml5(value * v, intptr_t * c){
  *v = caml_copy_nativeint(*c);
  }
extern value camlid_stub_f_nat_byte1(value p){
  intptr_t p1;
  value p_r;
  intptr_t p2;
  camlid_ml2u5(&p, &p2);
  p1 = camlid_stub_f_nat1(p2);
  camlid_u2ml5(&p_r, &p1);
  return p_r;
}

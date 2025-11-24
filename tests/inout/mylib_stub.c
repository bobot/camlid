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
static void camlid_ml2u(value * v, camlid_int * c){ *c = Long_val(*v); };
typedef camlid_int * camlid_ref;
static void camlid_u2c1(camlid_int * c, camlid_int * c1){ *c = (*c1); };
static void camlid_u2c(camlid_int * c, camlid_ref * c1){
  camlid_u2c1(*c1, c);
  };
static void camlid_c2u1(camlid_int * c, camlid_int * c1){ *c1 = (*c); };
static void camlid_c2u(camlid_int * c, camlid_ref * c1){
  camlid_c2u1(*c1, c);
  };
extern camlid_int camlid_stub_f_nat(camlid_int p){
  CAMLparam0();
  camlid_int p1;
  camlid_ref p2 = &(((struct { camlid_int a; }) { ((camlid_int) { }) }).a);
  camlid_u2c(&p, &p2);
  f_nat(p2);
  camlid_c2u(&p1, &p2);
  CAMLreturnT(camlid_int,p1);
};
static void camlid_u2ml(value * v, camlid_int * c){ *v = Val_long(*c); };
extern value camlid_stub_f_nat_byte(value p){
  camlid_int p1;
  value p_r;
  camlid_int p2;
  camlid_ml2u(&p, &p2);
  p1 = camlid_stub_f_nat(p2);
  camlid_u2ml(&p_r, &p1);
  return p_r;
};
typedef intptr_t camlid_int1;
static void camlid_ml2u1(value * v, camlid_int1 * c){ *c = Int_val(*v); };
typedef int camlid_int2;
typedef camlid_int2 * camlid_ref1;
static void camlid_u2c3(camlid_int2 * c, camlid_int1 * c1){
  *c = (int)(*c1);
  };
static void camlid_u2c2(camlid_int1 * c, camlid_ref1 * c1){
  camlid_u2c3(*c1, c);
  };
static void camlid_c2u3(camlid_int2 * c, camlid_int1 * c1){
  *c1 = (intptr_t)(*c);
  };
static void camlid_c2u2(camlid_int1 * c, camlid_ref1 * c1){
  camlid_c2u3(*c1, c);
  };
extern camlid_int1 camlid_stub_f_int(camlid_int1 p){
  CAMLparam0();
  camlid_int1 p1;
  camlid_ref1 p2 = &(((struct { camlid_int2 a; }) { ((camlid_int2) { }) }).a);
  camlid_u2c2(&p, &p2);
  f_int(p2);
  camlid_c2u2(&p1, &p2);
  CAMLreturnT(camlid_int1,p1);
};
static void camlid_u2ml1(value * v, camlid_int1 * c){ *v = Val_int(*c); };
extern value camlid_stub_f_int_byte(value p){
  camlid_int1 p1;
  value p_r;
  camlid_int1 p2;
  camlid_ml2u1(&p, &p2);
  p1 = camlid_stub_f_int(p2);
  camlid_u2ml1(&p_r, &p1);
  return p_r;
};
typedef double camlid_float;
static void camlid_ml2u2(value * v, camlid_float * c){
  *c = Double_val(*v);
  };
typedef camlid_float * camlid_ref2;
static void camlid_u2c5(camlid_float * c, camlid_float * c1){ *c = (*c1); };
static void camlid_u2c4(camlid_float * c, camlid_ref2 * c1){
  camlid_u2c5(*c1, c);
  };
static void camlid_c2u5(camlid_float * c, camlid_float * c1){ *c1 = (*c); };
static void camlid_c2u4(camlid_float * c, camlid_ref2 * c1){
  camlid_c2u5(*c1, c);
  };
extern camlid_float camlid_stub_f_double(camlid_float p){
  CAMLparam0();
  camlid_float p1;
  camlid_ref2 p2 = &(((struct { camlid_float a; }) { ((camlid_float) { }) }).a);
  camlid_u2c4(&p, &p2);
  f_double(p2);
  camlid_c2u4(&p1, &p2);
  CAMLreturnT(camlid_float,p1);
};
static void camlid_u2ml2(value * v, camlid_float * c){
  *v = caml_copy_double(*c);
  };
extern value camlid_stub_f_double_byte(value p){
  camlid_float p1;
  value p_r;
  camlid_float p2;
  camlid_ml2u2(&p, &p2);
  p1 = camlid_stub_f_double(p2);
  camlid_u2ml2(&p_r, &p1);
  return p_r;
};
typedef int32_t camlid_int32;
static void camlid_ml2u3(value * v, camlid_int32 * c){ *c = Int32_val(*v); };
typedef camlid_int32 * camlid_ref3;
static void camlid_u2c7(camlid_int32 * c, camlid_int32 * c1){ *c = (*c1); };
static void camlid_u2c6(camlid_int32 * c, camlid_ref3 * c1){
  camlid_u2c7(*c1, c);
  };
static void camlid_c2u7(camlid_int32 * c, camlid_int32 * c1){ *c1 = (*c); };
static void camlid_c2u6(camlid_int32 * c, camlid_ref3 * c1){
  camlid_c2u7(*c1, c);
  };
extern camlid_int32 camlid_stub_f_int32(camlid_int32 p){
  CAMLparam0();
  camlid_int32 p1;
  camlid_ref3 p2 = &(((struct { camlid_int32 a; }) { ((camlid_int32) { }) }).a);
  camlid_u2c6(&p, &p2);
  f_int32(p2);
  camlid_c2u6(&p1, &p2);
  CAMLreturnT(camlid_int32,p1);
};
static void camlid_u2ml3(value * v, camlid_int32 * c){
  *v = caml_copy_int32(*c);
  };
extern value camlid_stub_f_int32_byte(value p){
  camlid_int32 p1;
  value p_r;
  camlid_int32 p2;
  camlid_ml2u3(&p, &p2);
  p1 = camlid_stub_f_int32(p2);
  camlid_u2ml3(&p_r, &p1);
  return p_r;
};
typedef int64_t camlid_int64;
static void camlid_ml2u4(value * v, camlid_int64 * c){ *c = Int64_val(*v); };
typedef camlid_int64 * camlid_ref4;
static void camlid_u2c9(camlid_int64 * c, camlid_int64 * c1){ *c = (*c1); };
static void camlid_u2c8(camlid_int64 * c, camlid_ref4 * c1){
  camlid_u2c9(*c1, c);
  };
static void camlid_c2u9(camlid_int64 * c, camlid_int64 * c1){ *c1 = (*c); };
static void camlid_c2u8(camlid_int64 * c, camlid_ref4 * c1){
  camlid_c2u9(*c1, c);
  };
extern camlid_int64 camlid_stub_f_int64(camlid_int64 p){
  CAMLparam0();
  camlid_int64 p1;
  camlid_ref4 p2 = &(((struct { camlid_int64 a; }) { ((camlid_int64) { }) }).a);
  camlid_u2c8(&p, &p2);
  f_int64(p2);
  camlid_c2u8(&p1, &p2);
  CAMLreturnT(camlid_int64,p1);
};
static void camlid_u2ml4(value * v, camlid_int64 * c){
  *v = caml_copy_int64(*c);
  };
extern value camlid_stub_f_int64_byte(value p){
  camlid_int64 p1;
  value p_r;
  camlid_int64 p2;
  camlid_ml2u4(&p, &p2);
  p1 = camlid_stub_f_int64(p2);
  camlid_u2ml4(&p_r, &p1);
  return p_r;
};
typedef intptr_t camlid_nativeint;
static void camlid_ml2u5(value * v, camlid_nativeint * c){
  *c = Nativeint_val(*v);
  };
typedef camlid_nativeint * camlid_ref5;
static void camlid_u2c11(camlid_nativeint * c, camlid_nativeint * c1){
  *c = (*c1);
  };
static void camlid_u2c10(camlid_nativeint * c, camlid_ref5 * c1){
  camlid_u2c11(*c1, c);
  };
static void camlid_c2u11(camlid_nativeint * c, camlid_nativeint * c1){
  *c1 = (*c);
  };
static void camlid_c2u10(camlid_nativeint * c, camlid_ref5 * c1){
  camlid_c2u11(*c1, c);
  };
extern camlid_nativeint camlid_stub_f_nat1(camlid_nativeint p){
  CAMLparam0();
  camlid_nativeint p1;
  camlid_ref5 p2 = &(((struct { camlid_nativeint a; }) { ((camlid_nativeint) { }) }).a);
  camlid_u2c10(&p, &p2);
  f_nat(p2);
  camlid_c2u10(&p1, &p2);
  CAMLreturnT(camlid_nativeint,p1);
};
static void camlid_u2ml5(value * v, camlid_nativeint * c){
  *v = caml_copy_nativeint(*c);
  };
extern value camlid_stub_f_nat_byte1(value p){
  camlid_nativeint p1;
  value p_r;
  camlid_nativeint p2;
  camlid_ml2u5(&p, &p2);
  p1 = camlid_stub_f_nat1(p2);
  camlid_u2ml5(&p_r, &p1);
  return p_r;
};

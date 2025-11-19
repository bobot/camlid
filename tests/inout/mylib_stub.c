// generated using generator.exe and camlid
#include <stddef.h>
#include <inttypes.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include "lib.h"
typedef intptr_t camlid_int;
typedef camlid_int * camlid_ref;
static void camlid_ml2c1(value * v, camlid_int * c){ *c = Long_val(*v); };
static void camlid_ml2c(value * v, camlid_ref * c){ camlid_ml2c1(v, *c); };
static void camlid_c2ml1(value * v, camlid_int * c){ *v = Val_long(*c); };
static void camlid_c2ml(value * v, camlid_ref * c){ camlid_c2ml1(v, *c); };
static void camlid_free(camlid_ref * c){  };
extern value camlid_stub_f_nat(value p){
  camlid_ref p1 = &(((struct { camlid_int a; }) { ((camlid_int) { }) }).a);
  value ret;
  camlid_ml2c(&p, &p1);
  f_nat(p1);
  camlid_c2ml(&ret, &p1);
  camlid_free(&p1);
  return ret;
};
typedef int camlid_int1;
typedef camlid_int1 * camlid_ref1;
static void camlid_ml2c3(value * v, camlid_int1 * c){ *c = Int_val(*v); };
static void camlid_ml2c2(value * v, camlid_ref1 * c){ camlid_ml2c3(v, *c); };
static void camlid_c2ml3(value * v, camlid_int1 * c){ *v = Val_int(*c); };
static void camlid_c2ml2(value * v, camlid_ref1 * c){ camlid_c2ml3(v, *c); };
static void camlid_free1(camlid_ref1 * c){  };
extern value camlid_stub_f_int(value p){
  camlid_ref1 p1 = &(((struct { camlid_int1 a; }) { ((camlid_int1) { }) }).a);
  value ret;
  camlid_ml2c2(&p, &p1);
  f_int(p1);
  camlid_c2ml2(&ret, &p1);
  camlid_free1(&p1);
  return ret;
};
typedef double camlid_float;
typedef camlid_float * camlid_ref2;
static void camlid_ml2c5(value * v, camlid_float * c){
  *c = Double_val(*v);
  };
static void camlid_ml2c4(value * v, camlid_ref2 * c){ camlid_ml2c5(v, *c); };
static void camlid_c2ml5(value * v, camlid_float * c){
  *v = caml_copy_double(*c);
  };
static void camlid_c2ml4(value * v, camlid_ref2 * c){ camlid_c2ml5(v, *c); };
static void camlid_free2(camlid_ref2 * c){  };
extern value camlid_stub_f_double(value p){
  camlid_ref2 p1 = &(((struct { camlid_float a; }) { ((camlid_float) { }) }).a);
  value ret;
  camlid_ml2c4(&p, &p1);
  f_double(p1);
  camlid_c2ml4(&ret, &p1);
  camlid_free2(&p1);
  return ret;
};
typedef int32_t camlid_int32;
typedef camlid_int32 * camlid_ref3;
static void camlid_ml2c7(value * v, camlid_int32 * c){ *c = Int32_val(*v); };
static void camlid_ml2c6(value * v, camlid_ref3 * c){ camlid_ml2c7(v, *c); };
static void camlid_c2ml7(value * v, camlid_int32 * c){
  *v = caml_copy_int32(*c);
  };
static void camlid_c2ml6(value * v, camlid_ref3 * c){ camlid_c2ml7(v, *c); };
static void camlid_free3(camlid_ref3 * c){  };
extern value camlid_stub_f_int32(value p){
  camlid_ref3 p1 = &(((struct { camlid_int32 a; }) { ((camlid_int32) { }) }).a);
  value ret;
  camlid_ml2c6(&p, &p1);
  f_int32(p1);
  camlid_c2ml6(&ret, &p1);
  camlid_free3(&p1);
  return ret;
};
typedef int64_t camlid_int64;
typedef camlid_int64 * camlid_ref4;
static void camlid_ml2c9(value * v, camlid_int64 * c){ *c = Int64_val(*v); };
static void camlid_ml2c8(value * v, camlid_ref4 * c){ camlid_ml2c9(v, *c); };
static void camlid_c2ml9(value * v, camlid_int64 * c){
  *v = caml_copy_int64(*c);
  };
static void camlid_c2ml8(value * v, camlid_ref4 * c){ camlid_c2ml9(v, *c); };
static void camlid_free4(camlid_ref4 * c){  };
extern value camlid_stub_f_int64(value p){
  camlid_ref4 p1 = &(((struct { camlid_int64 a; }) { ((camlid_int64) { }) }).a);
  value ret;
  camlid_ml2c8(&p, &p1);
  f_int64(p1);
  camlid_c2ml8(&ret, &p1);
  camlid_free4(&p1);
  return ret;
};
typedef intptr_t camlid_nativeint;
typedef camlid_nativeint * camlid_ref5;
static void camlid_ml2c11(value * v, camlid_nativeint * c){
  *c = Nativeint_val(*v);
  };
static void camlid_ml2c10(value * v, camlid_ref5 * c){
  camlid_ml2c11(v, *c);
  };
static void camlid_c2ml11(value * v, camlid_nativeint * c){
  *v = caml_copy_nativeint(*c);
  };
static void camlid_c2ml10(value * v, camlid_ref5 * c){
  camlid_c2ml11(v, *c);
  };
static void camlid_free5(camlid_ref5 * c){  };
extern value camlid_stub_f_nat1(value p){
  camlid_ref5 p1 = &(((struct { camlid_nativeint a; }) { ((camlid_nativeint) { }) }).a);
  value ret;
  camlid_ml2c10(&p, &p1);
  f_nat(p1);
  camlid_c2ml10(&ret, &p1);
  camlid_free5(&p1);
  return ret;
};

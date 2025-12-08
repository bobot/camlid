// generated using generator.exe and camlid
#include <stddef.h>
#include <inttypes.h>
#include "lib.h"
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <stdio.h>
#include <string.h>
typedef intptr_t * camlid_ref;
extern intptr_t camlid_stub_f_nat(intptr_t p){
  CAMLparam0();
  intptr_t p1;
  camlid_ref p2 = &(((struct { intptr_t a; }) { ((intptr_t) { 0 }) }).a);
  *p2 = (p);
  f_nat(p2);
  p1 = (*p2);
  CAMLreturnT(intptr_t,p1);
}
extern value camlid_stub_f_nat_byte(value p){
  intptr_t p1;
  value p_r;
  intptr_t p2;
  p2 = Long_val(p);
  p1 = camlid_stub_f_nat(p2);
  p_r = Val_long(p1);
  return p_r;
}
typedef int * camlid_ref1;
extern intptr_t camlid_stub_f_int(intptr_t p){
  CAMLparam0();
  intptr_t p1;
  camlid_ref1 p2 = &(((struct { int a; }) { ((int) { 0 }) }).a);
  *p2 = (int)(p);
  f_int(p2);
  p1 = (intptr_t)(*p2);
  CAMLreturnT(intptr_t,p1);
}
extern value camlid_stub_f_int_byte(value p){
  intptr_t p1;
  value p_r;
  intptr_t p2;
  p2 = Int_val(p);
  p1 = camlid_stub_f_int(p2);
  p_r = Val_int(p1);
  return p_r;
}
typedef double * camlid_ref2;
extern double camlid_stub_f_double(double p){
  CAMLparam0();
  double p1;
  camlid_ref2 p2 = &(((struct { double a; }) { ((double) { 0 }) }).a);
  *p2 = (p);
  f_double(p2);
  p1 = (*p2);
  CAMLreturnT(double,p1);
}
extern value camlid_stub_f_double_byte(value p){
  double p1;
  value p_r;
  double p2;
  p2 = Double_val(p);
  p1 = camlid_stub_f_double(p2);
  p_r = caml_copy_double(p1);
  return p_r;
}
typedef int32_t * camlid_ref3;
extern int32_t camlid_stub_f_int32(int32_t p){
  CAMLparam0();
  int32_t p1;
  camlid_ref3 p2 = &(((struct { int32_t a; }) { ((int32_t) { 0 }) }).a);
  *p2 = (p);
  f_int32(p2);
  p1 = (*p2);
  CAMLreturnT(int32_t,p1);
}
extern value camlid_stub_f_int32_byte(value p){
  int32_t p1;
  value p_r;
  int32_t p2;
  p2 = Int32_val(p);
  p1 = camlid_stub_f_int32(p2);
  p_r = caml_copy_int32(p1);
  return p_r;
}
typedef int64_t * camlid_ref4;
extern int64_t camlid_stub_f_int64(int64_t p){
  CAMLparam0();
  int64_t p1;
  camlid_ref4 p2 = &(((struct { int64_t a; }) { ((int64_t) { 0 }) }).a);
  *p2 = (p);
  f_int64(p2);
  p1 = (*p2);
  CAMLreturnT(int64_t,p1);
}
extern value camlid_stub_f_int64_byte(value p){
  int64_t p1;
  value p_r;
  int64_t p2;
  p2 = Int64_val(p);
  p1 = camlid_stub_f_int64(p2);
  p_r = caml_copy_int64(p1);
  return p_r;
}
typedef intptr_t * camlid_ref5;
extern intptr_t camlid_stub_f_nativeint(intptr_t p){
  CAMLparam0();
  intptr_t p1;
  camlid_ref5 p2 = &(((struct { intptr_t a; }) { ((intptr_t) { 0 }) }).a);
  *p2 = (p);
  f_nat(p2);
  p1 = (*p2);
  CAMLreturnT(intptr_t,p1);
}
extern value camlid_stub_f_nativeint_byte(value p){
  intptr_t p1;
  value p_r;
  intptr_t p2;
  p2 = Nativeint_val(p);
  p1 = camlid_stub_f_nativeint(p2);
  p_r = caml_copy_nativeint(p1);
  return p_r;
}


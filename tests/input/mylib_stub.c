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
extern value camlid_stub_f(intptr_t p){
  CAMLparam0();
  int p1 = ((int) { 0 });
  p1 = (int)(p);
  f(p1);
  CAMLreturn(Val_unit);
}
extern value camlid_stub_f_byte(value p){
  intptr_t p1;
  p1 = Int_val(p);
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
  p7 = (int)(p);
  p8 = (int)(p1);
  p9 = (int)(p2);
  p10 = (int)(p3);
  p11 = (int)(p4);
  p12 = (int)(p5);
  p13 = (int)(p6);
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
  p7 = Int_val(p);
  p8 = Int_val(p1);
  p9 = Int_val(p2);
  p10 = Int_val(p3);
  p11 = Int_val(p4);
  p12 = Int_val(p5);
  p13 = Int_val(p6);
  return camlid_stub_f7(p7, p8, p9, p10, p11, p12, p13);
  
}


// generated using generator.exe and camlid
#include <stddef.h>
#include <inttypes.h>
#include "lib.h"
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <stdio.h>
#include <string.h>
static void camlid_ml2c(value * v, enum color * c){
  switch(Long_val(*v)){
    case 0: *c = RED; break;
    case 1: *c = GREEN; break;
    case 2: *c = BLUE; break;
    }
  }
static void camlid_c2ml(value * v, enum color * c){
  switch(*c){
    case RED: *v = Val_long(0); break;
    case GREEN: *v = Val_long(1); break;
    case BLUE: *v = Val_long(2); break;
    default: *v = Val_long(0);
  }
  }
extern value camlid_stub_next_color(value p){
  CAMLparam1(p);
  CAMLlocal1(vres);
  CAMLlocal1(err);
  enum color res;
  enum color p1 = ((enum color) 0);
  camlid_ml2c(&p, &p1);
  res = next_color(p1);
  if(!(res == RED || res == GREEN || res == BLUE)){
    err = caml_copy_string("unknown value of color");
    caml_failwith_value(err);
  }
  camlid_c2ml(&vres, &res);
  CAMLreturn(vres);
}
extern value camlid_stub_bad_color(void){
  CAMLparam0();
  CAMLlocal1(vres);
  CAMLlocal1(err);
  enum color res;
  res = bad_color();
  if(!(res == RED || res == GREEN || res == BLUE)){
    err = caml_copy_string("unknown value of color");
    caml_failwith_value(err);
  }
  camlid_c2ml(&vres, &res);
  CAMLreturn(vres);
}
static void camlid_ml2c1(value * v, int * c){
  static const int cid_table[] = { F_READ, F_WRITE, F_EXEC };
  *c = 0;
  for(value cid_l = *v; cid_l != Val_emptylist; cid_l = Field(cid_l, 1)){
    *c |= cid_table[Long_val(Field(cid_l, 0))];
  }
  }
static void camlid_c2ml1(value * v, int * c){
  static const int cid_table[] = { F_READ, F_WRITE, F_EXEC };
  CAMLparam0();
  CAMLlocal1(cid_cell);
  *v = Val_emptylist;
  for(intptr_t cid_i = 2; cid_i >= 0; cid_i--){
    if((*c & cid_table[cid_i]) == cid_table[cid_i]){
      cid_cell = caml_alloc_small(2, 0);
      Field(cid_cell, 0) = Val_long(cid_i);
      Field(cid_cell, 1) = *v;
      *v = cid_cell;
    }
  }
  CAMLreturn0;
  }
extern value camlid_stub_flags_add_exec(value p){
  CAMLparam1(p);
  CAMLlocal1(vres);
  int res;
  int p1 = ((int) 0);
  camlid_ml2c1(&p, &p1);
  res = flags_add_exec(p1);
  camlid_c2ml1(&vres, &res);
  CAMLreturn(vres);
}
typedef char * camlid_string_nt;
static void camlid_ml2c2(value * v, camlid_string_nt * c){
  size_t len=caml_string_length(*v)+1;
  *c=malloc(len);
  memcpy(*c,String_val(*v),len);
  }
static void camlid_ml2c_field(value * v, camlid_string_nt * c){
  value cid_temp = Field(*v, 0);
  camlid_ml2c2(&cid_temp, &(*c));
  }
static void camlid_c2ml2(value * v, camlid_string_nt * c){
  if(*c == NULL) *v = Val_int(0);
  else {
    CAMLparam0();
    CAMLlocal1(cid_temp);
    cid_temp = caml_copy_string((*c));
    *v = caml_alloc_small(1, 0);
    Field(*v, 0) = cid_temp;
    CAMLreturn0;
  }
  }
extern value camlid_stub_non_empty(value p){
  CAMLparam1(p);
  CAMLlocal1(vres);
  camlid_string_nt res;
  camlid_string_nt p1 = ((camlid_string_nt) { 0 });
  if((Is_block(p) && (!caml_string_is_c_safe(Field(p, 0))))) caml_failwith("string contains null bytes");
  if(Is_long(p)) p1 = NULL; else { camlid_ml2c_field(&p, &p1); }
  res = non_empty(p1);
  camlid_c2ml2(&vres, &res);
  if(p1 != NULL){ free(p1); }
  CAMLreturn(vres);
}


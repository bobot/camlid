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
struct camlid_array_s { intptr_t* t; size_t len; };
typedef struct camlid_array_s camlid_array;
static void camlid_ml2c(value * v, camlid_array * c){
  CAMLparam0 ();
  CAMLlocal1(cid_temp);
  c->len = Wosize_val(*v);
  c->t = malloc(sizeof(intptr_t)*c->len);
  for(size_t cid_i=0; cid_i < c->len; cid_i++){
    cid_temp=Field(*v,cid_i);
    c->t[cid_i] = Long_val(cid_temp);
    }
  CAMLreturn0;
  }
static void camlid_c2ml(value * v, camlid_array * c){
  CAMLparam0 ();
  CAMLlocal1(cid_temp);
  *v=caml_alloc(c->len,0);
  for(size_t cid_i=0; cid_i < c->len; cid_i++){
    cid_temp = Val_long(c->t[cid_i]);
    Store_field(*v,cid_i,cid_temp);
    }
  CAMLreturn0;
  }
extern value camlid_stub_f_output_input(value array){
  CAMLparam1(array);
  CAMLlocal1(array_r);
  camlid_array array1 = ((camlid_array) { 0 });
  camlid_ml2c(&array, &array1);
  f(&(array1.t), &(array1.len));
  camlid_c2ml(&array_r, &array1);
  free(array1.t);
  CAMLreturn(array_r);
}
extern value camlid_stub_f_output(void){
  CAMLparam0();
  CAMLlocal1(array_r);
  camlid_array array = ((camlid_array) { 0 });
  f(&(array.t), &(array.len));
  camlid_c2ml(&array_r, &array);
  free(array.t);
  CAMLreturn(array_r);
}
extern value camlid_stub_f_input_output(value array){
  CAMLparam1(array);
  CAMLlocal1(array_r);
  camlid_array array1 = ((camlid_array) { 0 });
  camlid_ml2c(&array, &array1);
  f2(array1.t, array1.len);
  camlid_c2ml(&array_r, &array1);
  free(array1.t);
  CAMLreturn(array_r);
}
extern value camlid_stub_f_input(value array){
  CAMLparam1(array);
  camlid_array array1 = ((camlid_array) { 0 });
  camlid_ml2c(&array, &array1);
  f2(array1.t, array1.len);
  free(array1.t);
  CAMLreturn(Val_unit);
}
typedef char * camlid_string_nt;
struct camlid_array_s1 { camlid_string_nt* t; size_t len; };
typedef struct camlid_array_s1 camlid_array1;
static int camlid_check_ml(value v){
  for(size_t cid_i=0; cid_i < Wosize_val(v); cid_i++)
    if(!caml_string_is_c_safe(Field(v, cid_i))) return 1;
  return 0;
  }
static void camlid_ml2c1(value * v, camlid_string_nt * c){
  size_t len=caml_string_length(*v)+1;
  *c=malloc(len);
  memcpy(*c,String_val(*v),len);
  }
static void camlid_ml2c2(value * v, camlid_array1 * c){
  CAMLparam0 ();
  CAMLlocal1(cid_temp);
  c->len = Wosize_val(*v);
  c->t = malloc(sizeof(camlid_string_nt)*c->len);
  for(size_t cid_i=0; cid_i < c->len; cid_i++){
    cid_temp=Field(*v,cid_i);
    camlid_ml2c1(&cid_temp, &c->t[cid_i]);
    }
  CAMLreturn0;
  }
static void camlid_free(camlid_array1 * c){
  for(size_t cid_i=0; cid_i < c->len; cid_i++){free(c->t[cid_i]);}
  free(c->t);
  }
extern intptr_t camlid_stub_f_strings(value array){
  CAMLparam1(array);
  intptr_t ures;
  size_t res;
  camlid_array1 array1 = ((camlid_array1) { 0 });
  if(camlid_check_ml(array)) caml_failwith("string contains null bytes");
  camlid_ml2c2(&array, &array1);
  res = f_strings(array1.t, array1.len);
  ures = (intptr_t)(res);
  camlid_free(&array1);
  CAMLreturnT(intptr_t,ures);
}
extern value camlid_stub_f_strings_byte(value array){
  intptr_t ures;
  value vres;
  ures = camlid_stub_f_strings(array);
  vres = Val_long(ures);
  return vres;
}
static void camlid_init(size_t array_len, intptr_t* * c){
  *c = malloc(sizeof(intptr_t)*array_len);
  
  }
static void camlid_c2ml1(size_t array_len, value * v, intptr_t* * c){
  CAMLparam0 ();
  CAMLlocal1(cid_temp);
  *v=caml_alloc(array_len,0);
  for(size_t cid_i=0; cid_i < array_len; cid_i++){
    cid_temp = Val_long(((*c)[cid_i]));
    Store_field(*v,cid_i,cid_temp);
    }
  CAMLreturn0;
  }
extern value camlid_stub_f4(intptr_t array_len){
  CAMLparam0();
  CAMLlocal1(array_r);
  size_t array_len1 = ((size_t) { 0 });
  intptr_t* array = ((intptr_t*) { 0 });
  array_len1 = (size_t)(array_len);
  camlid_init(array_len1, &array);
  f4(array);
  camlid_c2ml1(array_len1, &array_r, &array);
  free(array);
  CAMLreturn(array_r);
}
extern value camlid_stub_f4_byte(value array_len){
  intptr_t array_len1;
  array_len1 = Long_val(array_len);
  return camlid_stub_f4(array_len1);
  
}


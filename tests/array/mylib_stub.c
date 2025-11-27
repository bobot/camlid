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
typedef intptr_t camlid_int;
struct camlid_array_s { camlid_int* t; size_t len; };
typedef struct camlid_array_s camlid_array;
static void camlid_ml2c1(value * v, camlid_int * c){ *c = Long_val(*v); }
static void camlid_ml2c(value * v, camlid_array * c){
  CAMLparam0 ();
  CAMLlocal1(cid_temp);
  c->len = Wosize_val(*v);
  c->t = malloc(sizeof(camlid_int)*c->len);
  for(size_t cid_i=0; cid_i < c->len; cid_i++){
    cid_temp=Field(*v,cid_i);
    camlid_ml2c1(&cid_temp, &c->t[cid_i]);
    }
  CAMLreturn0;
  }
static void camlid_c2ml1(value * v, camlid_int * c){ *v = Val_long(*c); }
static void camlid_c2ml(value * v, camlid_array * c){
  CAMLparam0 ();
  CAMLlocal1(cid_temp);
  *v=caml_alloc(c->len,0);
  for(size_t cid_i=0; cid_i < c->len; cid_i++){
    camlid_c2ml1(&cid_temp, &c->t[cid_i]);
    Store_field(*v,cid_i,cid_temp);
    }
  CAMLreturn0;
  }
static void camlid_free(camlid_array * c){ free(c->t); }
extern value camlid_stub_f(value array){
  CAMLparam1(array);
  CAMLlocal1(array_r);
  camlid_array array1 = ((camlid_array) { 0 });
  camlid_ml2c(&array, &array1);
  f(&(array1.t), &(array1.len));
  camlid_c2ml(&array_r, &array1);
  camlid_free(&array1);
  CAMLreturn(array_r);
}
struct camlid_array_s1 { camlid_int* t; size_t len; };
typedef struct camlid_array_s1 camlid_array1;
static void camlid_c2ml2(value * v, camlid_array1 * c){
  CAMLparam0 ();
  CAMLlocal1(cid_temp);
  *v=caml_alloc(c->len,0);
  for(size_t cid_i=0; cid_i < c->len; cid_i++){
    camlid_c2ml1(&cid_temp, &c->t[cid_i]);
    Store_field(*v,cid_i,cid_temp);
    }
  CAMLreturn0;
  }
static void camlid_free1(camlid_array1 * c){ free(c->t); }
extern value camlid_stub_f1(){
  CAMLparam0();
  CAMLlocal1(array_r);
  camlid_array1 array = ((camlid_array1) { 0 });
  f(&(array.t), &(array.len));
  camlid_c2ml2(&array_r, &array);
  camlid_free1(&array);
  CAMLreturn(array_r);
}
struct camlid_array_s2 { camlid_int* t; size_t len; };
typedef struct camlid_array_s2 camlid_array2;
static void camlid_ml2c2(value * v, camlid_array2 * c){
  CAMLparam0 ();
  CAMLlocal1(cid_temp);
  c->len = Wosize_val(*v);
  c->t = malloc(sizeof(camlid_int)*c->len);
  for(size_t cid_i=0; cid_i < c->len; cid_i++){
    cid_temp=Field(*v,cid_i);
    camlid_ml2c1(&cid_temp, &c->t[cid_i]);
    }
  CAMLreturn0;
  }
static void camlid_c2ml3(value * v, camlid_array2 * c){
  CAMLparam0 ();
  CAMLlocal1(cid_temp);
  *v=caml_alloc(c->len,0);
  for(size_t cid_i=0; cid_i < c->len; cid_i++){
    camlid_c2ml1(&cid_temp, &c->t[cid_i]);
    Store_field(*v,cid_i,cid_temp);
    }
  CAMLreturn0;
  }
static void camlid_free2(camlid_array2 * c){ free(c->t); }
extern value camlid_stub_f2(value array){
  CAMLparam1(array);
  CAMLlocal1(array_r);
  camlid_array2 array1 = ((camlid_array2) { 0 });
  camlid_ml2c2(&array, &array1);
  f2(array1.t, array1.len);
  camlid_c2ml3(&array_r, &array1);
  camlid_free2(&array1);
  CAMLreturn(array_r);
}
struct camlid_array_s3 { camlid_int* t; size_t len; };
typedef struct camlid_array_s3 camlid_array3;
static void camlid_ml2c3(value * v, camlid_array3 * c){
  CAMLparam0 ();
  CAMLlocal1(cid_temp);
  c->len = Wosize_val(*v);
  c->t = malloc(sizeof(camlid_int)*c->len);
  for(size_t cid_i=0; cid_i < c->len; cid_i++){
    cid_temp=Field(*v,cid_i);
    camlid_ml2c1(&cid_temp, &c->t[cid_i]);
    }
  CAMLreturn0;
  }
static void camlid_free3(camlid_array3 * c){ free(c->t); }
extern value camlid_stub_f21(value array){
  CAMLparam1(array);
  camlid_array3 array1 = ((camlid_array3) { 0 });
  camlid_ml2c3(&array, &array1);
  f2(array1.t, array1.len);
  camlid_free3(&array1);
  CAMLreturn(Val_unit);
}
typedef size_t camlid_int1;
static void camlid_ml2u(value * v, camlid_int1 * c){
  *c = (size_t)Long_val(*v);
  }
static void camlid_u2c(camlid_int1 * c, camlid_int1 * c1){ *c = (*c1); }
static void camlid_init(camlid_int1 array_len, camlid_int* * c){
  *c = malloc(sizeof(camlid_int)*array_len);
  
  }
static void camlid_c2ml4(camlid_int1 array_len, value * v, camlid_int* * c){
  CAMLparam0 ();
  CAMLlocal1(cid_temp);
  *v=caml_alloc(array_len,0);
  for(size_t cid_i=0; cid_i < array_len; cid_i++){
    camlid_c2ml1(&cid_temp, &((*c)[cid_i]));
    Store_field(*v,cid_i,cid_temp);
    }
  CAMLreturn0;
  }
static void camlid_free4(camlid_int* * c){ free(*c); }
extern value camlid_stub_f4(camlid_int1 array_len){
  CAMLparam0();
  CAMLlocal1(array_r);
  camlid_int1 array_len1 = ((camlid_int1) { 0 });
  camlid_int* array = ((camlid_int*) { 0 });
  camlid_u2c(&array_len1, &array_len);
  camlid_init(array_len1, &array);
  f4(array);
  camlid_c2ml4(array_len1, &array_r, &array);
  camlid_free4(&array);
  CAMLreturn(array_r);
}
extern value camlid_stub_f4_byte(value array_len){
  camlid_int1 array_len1;
  camlid_ml2u(&array_len, &array_len1);
  return camlid_stub_f4(array_len1);
  
}

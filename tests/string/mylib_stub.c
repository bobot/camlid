// generated using generator.exe and camlid
#include <stddef.h>
#include <inttypes.h>
#include "./lib.h"
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
typedef char * camlid_string_nt;
static void camlid_ml2c(value * v, camlid_string_nt * c){
  size_t len=caml_string_length(*v)+1;
  *c=malloc(len);
  memcpy(*c,String_val(*v),len);
  }
extern value camlid_stub_f_in(value p){
  CAMLparam1(p);
  camlid_string_nt p1 = ((camlid_string_nt) { 0 });
  if(!caml_string_is_c_safe(p)) caml_failwith("string contains null bytes");
  camlid_ml2c(&p, &p1);
  f_in(p1);
  free(p1);
  CAMLreturn(Val_unit);
}
extern value camlid_stub_f_in2(value p,
  value p1){
  CAMLparam2(p, p1);
  camlid_string_nt p2 = ((camlid_string_nt) { 0 });
  camlid_string_nt p3 = ((camlid_string_nt) { 0 });
  if(!caml_string_is_c_safe(p)) caml_failwith("string contains null bytes");
  if(!caml_string_is_c_safe(p1)) caml_failwith("string contains null bytes");
  camlid_ml2c(&p, &p2);
  camlid_ml2c(&p1, &p3);
  f_in2(p2, p3);
  free(p2);
  free(p3);
  CAMLreturn(Val_unit);
}
static void camlid_ml2c1(value * v, camlid_string_nt * c){
  size_t len=strlen(String_val(*v))+1;
  *c=malloc(len);
  memcpy(*c,String_val(*v),len);
  }
extern value camlid_stub_f_in_prefix(value p){
  camlid_string_nt p1 = ((camlid_string_nt) { 0 });
  camlid_ml2c1(&p, &p1);
  f_in(p1);
  free(p1);
  return Val_unit;
}
typedef camlid_string_nt * camlid_ref;
extern value camlid_stub_f_out(void){
  CAMLparam0();
  CAMLlocal1(p_r);
  camlid_ref p = &(((struct { camlid_string_nt a; }) { ((camlid_string_nt) { 0 }) }).a);
  f_out(p);
  p_r = caml_copy_string(*p);
  free(*p);
  CAMLreturn(p_r);
}
typedef char * camlid_string_fs;
static void camlid_c2ml(size_t string_len, value* v, camlid_string_fs* c){
  *v = caml_alloc_string(string_len);memcpy(&Byte(*v,0),*c,string_len);
  }
extern value camlid_stub_f_in3(intptr_t string_len){
  CAMLparam0();
  CAMLlocal1(string_r);
  camlid_string_fs string = ((camlid_string_fs) { 0 });
  size_t string_len1 = ((size_t) { 0 });
  string_len1 = (size_t)(string_len);
  string = malloc(string_len1);
  f_in3(string);
  camlid_c2ml(string_len1, &string_r, &string);
  free(string);
  CAMLreturn(string_r);
}
extern value camlid_stub_f_in3_byte(value string_len){
  intptr_t string_len1;
  string_len1 = Long_val(string_len);
  return camlid_stub_f_in3(string_len1);
  
}
static void camlid_ml2c2(size_t string_len, value* v, camlid_string_fs* c){
  *c=malloc(string_len);
  memcpy(*c,String_val(*v),string_len);
  }
extern value camlid_stub_f_print_n(value string,
  intptr_t string_len){
  CAMLparam1(string);
  camlid_string_fs string1 = ((camlid_string_fs) { 0 });
  size_t string_len1 = ((size_t) { 0 });
  string_len1 = (size_t)(string_len);
  if(caml_string_length(string) < string_len1) caml_invalid_argument("string shorter than its length");
  camlid_ml2c2(string_len1, &string, &string1);
  f_print_n(string1, string_len1);
  free(string1);
  CAMLreturn(Val_unit);
}
extern value camlid_stub_f_print_n_byte(value string,
  value string_len){
  intptr_t string_len1;
  string_len1 = Long_val(string_len);
  return camlid_stub_f_print_n(string, string_len1);
  
}


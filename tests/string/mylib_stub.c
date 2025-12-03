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
typedef char * camlid_string_nt;
static void camlid_ml2c(value * v, camlid_string_nt * c){
  size_t len=strlen(String_val(*v))+1;
  *c=malloc(len);
  memcpy(*c,String_val(*v),len);
  }
extern value camlid_stub_f_in(value p){
  CAMLparam1(p);
  camlid_string_nt p1 = ((camlid_string_nt) { 0 });
  camlid_ml2c(&p, &p1);
  f_in(p1);
  free(p1);
  CAMLreturn(Val_unit);
}
typedef camlid_string_nt * camlid_ref;
extern value camlid_stub_f_out(){
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
extern value camlid_stub_f_in3(size_t string_len){
  CAMLparam0();
  CAMLlocal1(string_r);
  camlid_string_fs string = ((camlid_string_fs) { 0 });
  size_t string_len1 = ((size_t) { 0 });
  string_len1 = (string_len);
  string = malloc(string_len1);
  f_in3(string);
  camlid_c2ml(string_len1, &string_r, &string);
  free(string);
  CAMLreturn(string_r);
}
extern value camlid_stub_f_in3_byte(value string_len){
  size_t string_len1;
  string_len1 = (size_t)Long_val(string_len);
  return camlid_stub_f_in3(string_len1);
  
}

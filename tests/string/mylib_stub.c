// generated using generator.exe and camlid
#include <stddef.h>
#include <inttypes.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <string.h>
#include "./lib.h"
typedef char * camlid_string_nt;
static void camlid_ml2c(value * v, camlid_string_nt * c){
  size_t len=strlen(String_val(*v))+1;
  *c=malloc(len);
  memcpy(*c,String_val(*v),len);
  };
static void camlid_free(camlid_string_nt * c){ free(*c); };
extern value camlid_stub_f_in(value p){
  CAMLparam1(p);
  CAMLlocal1(ret);
  camlid_string_nt p1 = ((camlid_string_nt) { });
  camlid_ml2c(&p, &p1);
  f_in(p1);
  ret = Val_unit;
  camlid_free(&p1);
  CAMLreturn(ret);
};
typedef camlid_string_nt * camlid_ref;
static void camlid_c2ml1(value * v, camlid_string_nt * c){
  *v = caml_copy_string(*c);
  };
static void camlid_c2ml(value * v, camlid_ref * c){ camlid_c2ml1(v, *c); };
static void camlid_free1(camlid_ref * c){ camlid_free(*c); };
extern value camlid_stub_f_out(){
  CAMLparam0();
  CAMLlocal2(ret, p_r);
  camlid_ref p = &(((struct { camlid_string_nt a; }) { ((camlid_string_nt) { }) }).a);
  f_out(p);
  camlid_c2ml(&p_r, &p);
  ret = p_r;
  camlid_free1(&p);
  CAMLreturn(ret);
};
typedef char * camlid_string_fs;
typedef intptr_t camlid_int;
static void camlid_ml2c1(value * v, camlid_int * c){ *c = Long_val(*v); };
static void camlid_init(camlid_int string_len, camlid_string_fs * c){
  *c=malloc(string_len);
  
  };
static void camlid_c2ml2(camlid_int string_len, value * v,
camlid_string_fs * c){
  *v = caml_alloc_string(string_len);memcpy(&Byte(*v,0),*c,string_len);
  };
static void camlid_free2(camlid_string_fs * c){ free(*c); };
extern value camlid_stub_f_in3(value string_len){
  CAMLparam1(string_len);
  CAMLlocal2(ret, string_r);
  camlid_string_fs string = ((camlid_string_fs) { });
  camlid_int string_len1 = ((camlid_int) { });
  camlid_ml2c1(&string_len, &string_len1);
  camlid_init(string_len1, &string);
  f_in3(string);
  camlid_c2ml2(string_len1, &string_r, &string);
  ret = string_r;
  camlid_free2(&string);
  CAMLreturn(ret);
};

// generated using generator.exe and camlid
#include "mylib_stub.h"
#include "defs.h"
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
static void camlid_ml2c(value * v, camlid_t * c){
  if(Is_long(*v)){switch(Long_val(*v)){
  case 0: /* T2 */ c->tag = camlid_t_T2; break;
  };
  return;}else{CAMLparam0();
  CAMLlocal1(tmp);
  switch(Tag_val(*v)){
  case 0: /* T1 */
    c->tag=camlid_t_T1;
    tmp=Field(*v,0);
    c->u.T1.a = Int_val(tmp);
    tmp=Field(*v,1);
    c->u.T1.b = Double_val(tmp);
    break;
  case 1: /* T3 */
    c->tag=camlid_t_T3;
    tmp=Field(*v,0);
    c->u.T3.c = Int_val(tmp);
    break;
  };
  CAMLreturn0;}
  }
static void camlid_ml2c1(value * v, lib_t * c){
  camlid_algdata tmp = ((camlid_t) { 0 });
  camlid_ml2c(&*v, &tmp);
  lib_t_of_adt(c, &tmp);;
  }
extern value camlid_stub_f_print(value p){
  CAMLparam1(p);
  camlid_ref p1 = &(((struct { lib_t a; }) { ((lib_t) { { 0 } }) }).a);
  camlid_ml2c1(&p, &*p1);
  f_print(p1);
  CAMLreturn(Val_unit);
}
static int camlid_check_ml(value v){
  if(Is_long(v)) return 0;
  if(Tag_val(v) == 0 && (!caml_string_is_c_safe(Field(v, 0)))) return 1;
  return 0;
  }
static void camlid_ml2c2(value * v, camlid_string_nt * c){
  size_t len=caml_string_length(*v)+1;
  *c=malloc(len);
  memcpy(*c,String_val(*v),len);
  }
static void camlid_ml2c3(value * v, camlid_name * c){
  if(Is_long(*v)){switch(Long_val(*v)){
  case 0: /* Anonymous */ c->tag = camlid_name_Anonymous; break;
  };
  return;}else{CAMLparam0();
  CAMLlocal1(tmp);
  switch(Tag_val(*v)){
  case 0: /* Named */
    c->tag=camlid_name_Named;
    tmp=Field(*v,0);
    camlid_ml2c2(&tmp, &c->u.Named.s);
    break;
  };
  CAMLreturn0;}
  }
static void camlid_free(camlid_name * c){
  switch(c->tag){
    case camlid_name_Named:
      free(c->u.Named.s);
      break;
    default: break;
  }
  }
static void camlid_ml2c4(value * v, name_t * c){
  camlid_algdata1 tmp = ((camlid_name) { .tag = camlid_name_none });
  camlid_ml2c3(&*v, &tmp);
  name_t_of_name(c, &tmp);;
  camlid_free(&tmp);
  }
extern value camlid_stub_f_hello(value p){
  CAMLparam1(p);
  camlid_ref1 p1 = &(((struct { name_t a; }) { ((name_t) { 0 }) }).a);
  if(camlid_check_ml(p)) caml_failwith("string contains null bytes");
  camlid_ml2c4(&p, &*p1);
  f_hello(p1);
  name_clear(&*p1);
  CAMLreturn(Val_unit);
}
static void camlid_c2ml(value * v, camlid_name * c){
  CAMLparam0();
  CAMLlocal1(tmp);
  switch(c->tag){
  case camlid_name_Anonymous: /* Anonymous */ *v = Val_int(0); break;
  case camlid_name_Named: /* Named */
    *v=caml_alloc(1,0);
    tmp = caml_copy_string(c->u.Named.s);
    Store_field(*v,0,tmp);
    break;
  default: break;};
  CAMLreturn0;
  }
static void camlid_c2ml1(value * v, name_t * c){
  camlid_algdata1 tmp = ((camlid_name) { .tag = camlid_name_none });
  name_of_name_t(&tmp, c);;
  camlid_c2ml(&*v, &tmp);
  camlid_free(&tmp);
  }
extern value camlid_stub_f_make(intptr_t p){
  CAMLparam0();
  CAMLlocal1(p_r);
  camlid_ref1 p1 = &(((struct { name_t a; }) { ((name_t) { 0 }) }).a);
  int p2 = ((int) { 0 });
  p2 = (int)(p);
  f_make(p1, p2);
  camlid_c2ml1(&p_r, &*p1);
  name_clear(&*p1);
  CAMLreturn(p_r);
}
extern value camlid_stub_f_make_byte(value p){
  intptr_t p1;
  p1 = Int_val(p);
  return camlid_stub_f_make(p1);
  
}


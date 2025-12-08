// generated using generator.exe and camlid
#include "mylib_stub.h"
#include "defs.h"
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <stdio.h>
#include <string.h>
static void camlid_c2ml1(value * v, camlid_result * c){
  CAMLparam0();
  CAMLlocal1(tmp);
  switch(c->tag){
  case camlid_result_Data: /* Data */
    *v=caml_alloc(1,0);
    tmp = Val_long(c->u.Data.data);
    Store_field(*v,0,tmp);
    break;
  case camlid_result_Error: /* Error */
    *v=caml_alloc(1,1);
    tmp = Val_long(c->u.Error.error);
    Store_field(*v,0,tmp);
    break;};
  CAMLreturn0;
  }
static void camlid_c2ml(camlid_ref data, value * v, intptr_t * c){
  camlid_algdata tmp;combine_data_or_status(&tmp, c, data);;
   camlid_c2ml1(&*v, &tmp);
  }
extern value camlid_stub_f(intptr_t p){
  CAMLparam0();
  CAMLlocal1(vres);
  intptr_t res;
  intptr_t p1 = ((intptr_t) { 0 });
  camlid_ref data = &(((struct { intptr_t a; }) { ((intptr_t) { 0 }) }).a);
  p1 = (p);
  res = f(p1, data);
  camlid_c2ml(data, &vres, &res);
  CAMLreturn(vres);
}
extern value camlid_stub_f_byte(value p){
  intptr_t p1;
  p1 = Long_val(p);
  return camlid_stub_f(p1);
  
}


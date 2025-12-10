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
static void camlid_ml2c1(value * v, camlid_t * c){
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
static void camlid_ml2c(value * v, lib_t * c){
  camlid_algdata tmp;
  camlid_ml2c1(&*v, &tmp);
  lib_t_of_adt(c, &tmp);;
  }
extern value camlid_stub_f_print(value p){
  CAMLparam1(p);
  camlid_ref p1 = &(((struct { lib_t a; }) { ((lib_t) { { 0 } }) }).a);
  camlid_ml2c(&p, &*p1);
  f_print(p1);
  CAMLreturn(Val_unit);
}


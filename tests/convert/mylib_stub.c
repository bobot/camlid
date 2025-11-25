// generated using generator.exe and camlid
#include "mylib_stub.h"
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <string.h>
#include "defs.h"
static void camlid_ml2u(value * v, camlid_int * c){ *c = Long_val(*v); }
static void camlid_u2c(camlid_int * c, camlid_int * c1){ *c = (*c1); }
camlid_int f(camlid_int, camlid_ref);
static void camlid_c2ml2(value * v, camlid_int * c){ *v = Val_long(*c); }
static void camlid_c2ml1(value * v, camlid_result * c){
  CAMLparam0();
  CAMLlocal1(tmp);
  switch(c->tag){
  case camlid_result_Data: /* Data */
    *v=caml_alloc(1,0);
    camlid_c2ml2(&tmp, &c->u.Data.data);
    Store_field(*v,0,tmp);
    break;
  case camlid_result_Error: /* Error */
    *v=caml_alloc(1,1);
    camlid_c2ml2(&tmp, &c->u.Error.error);
    Store_field(*v,0,tmp);
    break;};
  CAMLreturn0;
  }
static void camlid_c2ml(camlid_int * c, value * v, camlid_ref data){
  camlid_result tmp; combine_data_or_status(&tmp, c, data);
  camlid_c2ml1(v, &tmp);
  }
extern value camlid_stub_f(camlid_int p){
  CAMLparam0();
  CAMLlocal1(vres);
  camlid_int res;
  camlid_int p1 = ((camlid_int) { });
  camlid_ref data = &(((struct { camlid_int a; }) { ((camlid_int) { }) }).a);
  camlid_u2c(&p1, &p);
  res = f(p1, data);
  camlid_c2ml(&res, &vres, data);
  CAMLreturn(vres);
}
extern value camlid_stub_f_byte(value p){
  camlid_int p1;
  camlid_ml2u(&p, &p1);
  return camlid_stub_f(p1);
  
}

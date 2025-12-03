// generated using generator.exe and camlid
#include "mylib_stub.h"
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <stdio.h>
#include <string.h>
#include "./defs.h"
static void caml_cudd_init(DdManager* * i){ Cudd_Quit(*i); free(*i); }
static void caml_cudd_finalize_op(value v){
  caml_cudd_init(((DdManager* *) Data_custom_val(v)));;
  }
struct custom_operations caml_cudd_cops = {
NULL,
caml_cudd_finalize_op,
custom_compare_default,
custom_hash_default,
custom_serialize_default,
custom_deserialize_default
};
static void caml_cudd_c2ml(value * v, DdManager* * c){
  *v = caml_alloc_custom(&caml_cudd_cops,sizeof(DdManager*), 0, 1);
  *(((DdManager* *) Data_custom_val(*v))) = *c;
  }
extern value caml_cudd_stub_init(){
  CAMLparam0();
  CAMLlocal1(vres);
  DdManager* res;
  res = cudd_init();
  caml_cudd_c2ml(&vres, &res);
  CAMLreturn(vres);
}
static void caml_cudd_finalize_op1(value v){
  Cudd_RecursiveDeref(((caml_cudd_bdd_wrapper *) Data_custom_val(v))->manager,((caml_cudd_bdd_wrapper *) Data_custom_val(v))->ptr);;
  }
struct custom_operations caml_cudd_cops1 = {
NULL,
caml_cudd_finalize_op1,
custom_compare_default,
custom_hash_default,
custom_serialize_default,
custom_deserialize_default
};
static void caml_cudd_c2ml1(DdManager* man, value * v, caml_cudd_bdd_t * c){
  *v = caml_alloc_custom(&caml_cudd_cops1,sizeof(caml_cudd_bdd_wrapper), 0, 1);
  Cudd_Ref(*c); ((caml_cudd_bdd_wrapper *) Data_custom_val(*v))->manager=man;
  ((caml_cudd_bdd_wrapper *) Data_custom_val(*v))->ptr=*c;;
  }
extern value caml_cudd_stub_bdd_true(value man){
  CAMLparam1(man);
  CAMLlocal1(vres);
  caml_cudd_bdd_t res;
  DdManager* man1 = ((DdManager*) { 0 });
  man1 = *(((DdManager* *) Data_custom_val(man)));
  res = Cudd_ReadOne(man1);
  caml_cudd_c2ml1(man1, &vres, &res);
  CAMLreturn(vres);
}
extern value caml_cudd_stub_bdd_false(value man){
  CAMLparam1(man);
  CAMLlocal1(vres);
  caml_cudd_bdd_t res;
  DdManager* man1 = ((DdManager*) { 0 });
  man1 = *(((DdManager* *) Data_custom_val(man)));
  res = Cudd_ReadLogicZero(man1);
  caml_cudd_c2ml1(man1, &vres, &res);
  CAMLreturn(vres);
}
extern value caml_cudd_stub_bdd_var(value man,
  intptr_t p){
  CAMLparam1(man);
  CAMLlocal1(vres);
  caml_cudd_bdd_t res;
  DdManager* man1 = ((DdManager*) { 0 });
  int p1 = ((int) { 0 });
  man1 = *(((DdManager* *) Data_custom_val(man)));
  p1 = (int)(p);
  res = Cudd_bddIthVar(man1, p1);
  caml_cudd_c2ml1(man1, &vres, &res);
  CAMLreturn(vres);
}
extern value caml_cudd_stub_bdd_var_byte(value man,
  value p){
  intptr_t p1;
  p1 = Int_val(p);
  return caml_cudd_stub_bdd_var(man, p1);
  
}
extern value caml_cudd_stub_bdd_newvar(value man){
  CAMLparam1(man);
  CAMLlocal1(vres);
  caml_cudd_bdd_t res;
  DdManager* man1 = ((DdManager*) { 0 });
  man1 = *(((DdManager* *) Data_custom_val(man)));
  res = Cudd_bddNewVar(man1);
  caml_cudd_c2ml1(man1, &vres, &res);
  CAMLreturn(vres);
}
extern value caml_cudd_stub_bdd_and(value man,
  value p,
  value p1){
  CAMLparam3(man, p, p1);
  CAMLlocal1(vres);
  caml_cudd_bdd_t res;
  DdManager* man1 = ((DdManager*) { 0 });
  caml_cudd_bdd_t p2 = ((caml_cudd_bdd_t) { 0 });
  caml_cudd_bdd_t p3 = ((caml_cudd_bdd_t) { 0 });
  man1 = *(((DdManager* *) Data_custom_val(man)));
  *&p2=((caml_cudd_bdd_wrapper *) Data_custom_val(p))->ptr;;
  *&p3=((caml_cudd_bdd_wrapper *) Data_custom_val(p1))->ptr;;
  res = Cudd_bddAnd(man1, p2, p3);
  caml_cudd_c2ml1(man1, &vres, &res);
  CAMLreturn(vres);
}
extern value caml_cudd_stub_bdd_or(value man,
  value p,
  value p1){
  CAMLparam3(man, p, p1);
  CAMLlocal1(vres);
  caml_cudd_bdd_t res;
  DdManager* man1 = ((DdManager*) { 0 });
  caml_cudd_bdd_t p2 = ((caml_cudd_bdd_t) { 0 });
  caml_cudd_bdd_t p3 = ((caml_cudd_bdd_t) { 0 });
  man1 = *(((DdManager* *) Data_custom_val(man)));
  *&p2=((caml_cudd_bdd_wrapper *) Data_custom_val(p))->ptr;;
  *&p3=((caml_cudd_bdd_wrapper *) Data_custom_val(p1))->ptr;;
  res = Cudd_bddOr(man1, p2, p3);
  caml_cudd_c2ml1(man1, &vres, &res);
  CAMLreturn(vres);
}
extern value caml_cudd_stub_bdd_not(value man,
  value p){
  CAMLparam2(man, p);
  CAMLlocal1(vres);
  caml_cudd_bdd_t res;
  DdManager* man1 = ((DdManager*) { 0 });
  caml_cudd_bdd_t p1 = ((caml_cudd_bdd_t) { 0 });
  man1 = *(((DdManager* *) Data_custom_val(man)));
  *&p1=((caml_cudd_bdd_wrapper *) Data_custom_val(p))->ptr;;
  res = Cudd_Not(p1);
  caml_cudd_c2ml1(man1, &vres, &res);
  CAMLreturn(vres);
}
extern int caml_cudd_stub_bdd_is_equal(value p,
  value p1){
  CAMLparam2(p, p1);
  int ures;
  int res;
  caml_cudd_bdd_t p2 = ((caml_cudd_bdd_t) { 0 });
  caml_cudd_bdd_t p3 = ((caml_cudd_bdd_t) { 0 });
  *&p2=((caml_cudd_bdd_wrapper *) Data_custom_val(p))->ptr;;
  *&p3=((caml_cudd_bdd_wrapper *) Data_custom_val(p1))->ptr;;
  res = equal_bdd(p2, p3);
  ures = (res);
  CAMLreturnT(int,ures);
}
extern value caml_cudd_stub_bdd_is_equal_byte(value p,
  value p1){
  int ures;
  value vres;
  ures = caml_cudd_stub_bdd_is_equal(p, p1);
  vres = Val_bool(ures);
  return vres;
}
extern int caml_cudd_stub_bdd_leq(value man,
  value p,
  value p1){
  CAMLparam3(man, p, p1);
  int ures;
  int res;
  DdManager* man1 = ((DdManager*) { 0 });
  caml_cudd_bdd_t p2 = ((caml_cudd_bdd_t) { 0 });
  caml_cudd_bdd_t p3 = ((caml_cudd_bdd_t) { 0 });
  man1 = *(((DdManager* *) Data_custom_val(man)));
  *&p2=((caml_cudd_bdd_wrapper *) Data_custom_val(p))->ptr;;
  *&p3=((caml_cudd_bdd_wrapper *) Data_custom_val(p1))->ptr;;
  res = Cudd_bddLeq(man1, p2, p3);
  ures = (res);
  CAMLreturnT(int,ures);
}
extern value caml_cudd_stub_bdd_leq_byte(value man,
  value p,
  value p1){
  int ures;
  value vres;
  ures = caml_cudd_stub_bdd_leq(man, p, p1);
  vres = Val_bool(ures);
  return vres;
}
extern value caml_cudd_stub_print(value man,
  value p){
  CAMLparam2(man, p);
  DdManager* man1 = ((DdManager*) { 0 });
  caml_cudd_bdd_t p1 = ((caml_cudd_bdd_t) { 0 });
  man1 = *(((DdManager* *) Data_custom_val(man)));
  *&p1=((caml_cudd_bdd_wrapper *) Data_custom_val(p))->ptr;;
  bdd_print(man1, p1);
  CAMLreturn(Val_unit);
}
static void caml_cudd_c2ml2(DdManager* man, value * v, caml_cudd_result * c){
  CAMLparam0();
  CAMLlocal1(tmp);
  switch(c->tag){
  case caml_cudd_result_False: /* False */ *v = Val_int(0); break;
  case caml_cudd_result_True: /* True */ *v = Val_int(1); break;
  case caml_cudd_result_Ifte: /* Ifte */
    *v=caml_alloc(3,0);
    tmp = Val_long(c->u.Ifte.cond);
    Store_field(*v,0,tmp);
    
    caml_cudd_c2ml1(man, &tmp, &c->u.Ifte.then_);
    Store_field(*v,1,tmp);
    
    caml_cudd_c2ml1(man, &tmp, &c->u.Ifte.else_);
    Store_field(*v,2,tmp);
    break;};
  CAMLreturn0;
  }
extern value caml_cudd_stub_inspect(value man,
  value p){
  CAMLparam2(man, p);
  CAMLlocal1(p_r);
  DdManager* man1 = ((DdManager*) { 0 });
  caml_cudd_ref p1 = &(((struct { caml_cudd_algdata a; }) { ((caml_cudd_result) { 0 }) }).a);
  caml_cudd_bdd_t p2 = ((caml_cudd_bdd_t) { 0 });
  man1 = *(((DdManager* *) Data_custom_val(man)));
  *&p2=((caml_cudd_bdd_wrapper *) Data_custom_val(p))->ptr;;
  bdd_inspect(man1, p1, p2);
  caml_cudd_c2ml2(man1, &p_r, &*p1);
  CAMLreturn(p_r);
}

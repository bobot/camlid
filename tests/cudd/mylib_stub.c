// generated using generator.exe and camlid
#include "mylib_stub.h"
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <string.h>
#include "./defs.h"
void Cudd_Quit(caml_cudd_custom);
static void caml_cudd_finalize_op(value v){
  Cudd_Quit(*(caml_cudd_custom *) Data_custom_val(v));
  };
struct custom_operations caml_cudd_cops = {
NULL,
caml_cudd_finalize_op,
custom_compare_default,
custom_hash_default,
custom_serialize_default,
custom_deserialize_default
};
static void caml_cudd_c2ml(value * v, caml_cudd_custom * c){
  *v = caml_alloc_custom(&caml_cudd_cops,sizeof(caml_cudd_custom), 0, 1);
  *((caml_cudd_custom *) Data_custom_val(*v)) = *c;
  };
extern value caml_cudd_stub_cudd_init(){
  CAMLparam0();
  CAMLlocal1(vres);
  caml_cudd_custom res;
  res = cudd_init();
  caml_cudd_c2ml(&vres, &res);
  CAMLreturn(vres);
};
static void caml_cudd_ml2c(value * v, caml_cudd_custom * c){
  *c = *((caml_cudd_custom *) Data_custom_val(*v));
  };
static void caml_cudd_bdd_finalize(caml_cudd_bdd_wrapper* i){
  Cudd_RecursiveDeref(i->manager,i->ptr);
  };
static void caml_cudd_finalize_op1(value v){
  caml_cudd_bdd_finalize((caml_cudd_bdd_wrapper *) Data_custom_val(v));
  };
struct custom_operations caml_cudd_cops1 = {
NULL,
caml_cudd_finalize_op1,
custom_compare_default,
custom_hash_default,
custom_serialize_default,
custom_deserialize_default
};
static void caml_cudd_bdd_set(caml_cudd_custom man, caml_cudd_bdd_wrapper* i,
caml_cudd_bdd_t* c){
  Cudd_Ref(*c);
  i->manager=man;
  i->ptr=*c;
  };
static void caml_cudd_c2ml1(caml_cudd_custom man, value * v,
caml_cudd_bdd_t * c){
  *v = caml_alloc_custom(&caml_cudd_cops1,sizeof(caml_cudd_bdd_wrapper), 0, 1);
  caml_cudd_bdd_set(man, (caml_cudd_bdd_wrapper *) Data_custom_val(*v), c);
  };
extern value caml_cudd_stub_Cudd_ReadOne(value man){
  CAMLparam1(man);
  CAMLlocal1(vres);
  caml_cudd_bdd_t res;
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_ml2c(&man, &man1);
  res = Cudd_ReadOne(man1);
  caml_cudd_c2ml1(man1, &vres, &res);
  CAMLreturn(vres);
};
extern value caml_cudd_stub_Cudd_ReadLogicZero(value man){
  CAMLparam1(man);
  CAMLlocal1(vres);
  caml_cudd_bdd_t res;
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_ml2c(&man, &man1);
  res = Cudd_ReadLogicZero(man1);
  caml_cudd_c2ml1(man1, &vres, &res);
  CAMLreturn(vres);
};
static void caml_cudd_ml2u(value * v, caml_cudd_int * c){
  *c = Int_val(*v);
  };
static void caml_cudd_u2c(caml_cudd_int1 * c, caml_cudd_int * c1){
  *c = (int)(*c1);
  };
extern value caml_cudd_stub_Cudd_bddIthVar(value man,
  caml_cudd_int p){
  CAMLparam1(man);
  CAMLlocal1(vres);
  caml_cudd_bdd_t res;
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_int1 p1 = ((caml_cudd_int1) { });
  caml_cudd_ml2c(&man, &man1);
  caml_cudd_u2c(&p1, &p);
  res = Cudd_bddIthVar(man1, p1);
  caml_cudd_c2ml1(man1, &vres, &res);
  CAMLreturn(vres);
};
extern value caml_cudd_stub_Cudd_bddIthVar_byte(value man,
  value p){
  caml_cudd_int p1;
  caml_cudd_ml2u(&p, &p1);
  return caml_cudd_stub_Cudd_bddIthVar(man, p1);
  
};
extern value caml_cudd_stub_Cudd_bddNewVar(value man){
  CAMLparam1(man);
  CAMLlocal1(vres);
  caml_cudd_bdd_t res;
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_ml2c(&man, &man1);
  res = Cudd_bddNewVar(man1);
  caml_cudd_c2ml1(man1, &vres, &res);
  CAMLreturn(vres);
};
static void caml_cudd_bdd_get(caml_cudd_bdd_wrapper* i, caml_cudd_bdd_t* c){
  *c=i->ptr;
  };
static void caml_cudd_ml2c1(value * v, caml_cudd_bdd_t * c){
  caml_cudd_bdd_get((caml_cudd_bdd_wrapper *) Data_custom_val(*v), c);
  };
extern value caml_cudd_stub_Cudd_bddAnd(value man,
  value p,
  value p1){
  CAMLparam3(man, p, p1);
  CAMLlocal1(vres);
  caml_cudd_bdd_t res;
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_bdd_t p2 = ((caml_cudd_bdd_t) { });
  caml_cudd_bdd_t p3 = ((caml_cudd_bdd_t) { });
  caml_cudd_ml2c(&man, &man1);
  caml_cudd_ml2c1(&p, &p2);
  caml_cudd_ml2c1(&p1, &p3);
  res = Cudd_bddAnd(man1, p2, p3);
  caml_cudd_c2ml1(man1, &vres, &res);
  CAMLreturn(vres);
};
extern value caml_cudd_stub_Cudd_bddOr(value man,
  value p,
  value p1){
  CAMLparam3(man, p, p1);
  CAMLlocal1(vres);
  caml_cudd_bdd_t res;
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_bdd_t p2 = ((caml_cudd_bdd_t) { });
  caml_cudd_bdd_t p3 = ((caml_cudd_bdd_t) { });
  caml_cudd_ml2c(&man, &man1);
  caml_cudd_ml2c1(&p, &p2);
  caml_cudd_ml2c1(&p1, &p3);
  res = Cudd_bddOr(man1, p2, p3);
  caml_cudd_c2ml1(man1, &vres, &res);
  CAMLreturn(vres);
};
extern value caml_cudd_stub_Cudd_Not(value man,
  value p){
  CAMLparam2(man, p);
  CAMLlocal1(vres);
  caml_cudd_bdd_t res;
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_bdd_t p1 = ((caml_cudd_bdd_t) { });
  caml_cudd_ml2c(&man, &man1);
  caml_cudd_ml2c1(&p, &p1);
  res = Cudd_Not(p1);
  caml_cudd_c2ml1(man1, &vres, &res);
  CAMLreturn(vres);
};
static void caml_cudd_c2u(caml_cudd_bool * c, caml_cudd_bool * c1){
  *c1 = (*c);
  };
extern caml_cudd_bool caml_cudd_stub_equal_bdd(value p,
  value p1){
  CAMLparam2(p, p1);
  caml_cudd_bool ures;
  caml_cudd_bool res;
  caml_cudd_bdd_t p2 = ((caml_cudd_bdd_t) { });
  caml_cudd_bdd_t p3 = ((caml_cudd_bdd_t) { });
  caml_cudd_ml2c1(&p, &p2);
  caml_cudd_ml2c1(&p1, &p3);
  res = equal_bdd(p2, p3);
  caml_cudd_c2u(&res, &ures);
  CAMLreturnT(caml_cudd_bool,ures);
};
static void caml_cudd_u2ml(value * v, caml_cudd_bool * c){
  *v = Val_bool(*c);
  };
extern value caml_cudd_stub_equal_bdd_byte(value p,
  value p1){
  caml_cudd_bool ures;
  value vres;
  ures = caml_cudd_stub_equal_bdd(p, p1);
  caml_cudd_u2ml(&vres, &ures);
  return vres;
};
extern caml_cudd_bool caml_cudd_stub_Cudd_bddLeq(value man,
  value p,
  value p1){
  CAMLparam3(man, p, p1);
  caml_cudd_bool ures;
  caml_cudd_bool res;
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_bdd_t p2 = ((caml_cudd_bdd_t) { });
  caml_cudd_bdd_t p3 = ((caml_cudd_bdd_t) { });
  caml_cudd_ml2c(&man, &man1);
  caml_cudd_ml2c1(&p, &p2);
  caml_cudd_ml2c1(&p1, &p3);
  res = Cudd_bddLeq(man1, p2, p3);
  caml_cudd_c2u(&res, &ures);
  CAMLreturnT(caml_cudd_bool,ures);
};
extern value caml_cudd_stub_Cudd_bddLeq_byte(value man,
  value p,
  value p1){
  caml_cudd_bool ures;
  value vres;
  ures = caml_cudd_stub_Cudd_bddLeq(man, p, p1);
  caml_cudd_u2ml(&vres, &ures);
  return vres;
};
extern value caml_cudd_stub_bdd_print(value man,
  value p){
  CAMLparam2(man, p);
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_bdd_t p1 = ((caml_cudd_bdd_t) { });
  caml_cudd_ml2c(&man, &man1);
  caml_cudd_ml2c1(&p, &p1);
  bdd_print(man1, p1);
  CAMLreturn(Val_unit);
};
static void caml_cudd_c2ml4(value * v, caml_cudd_int2 * c){
  *v = Val_long(*c);
  };
static void caml_cudd_c2ml3(caml_cudd_custom man, value * v,
caml_cudd_result * c){
  CAMLparam0();
  CAMLlocal1(tmp);
  switch(c->tag){
  case caml_cudd_result_False: /* False */ *v = Val_int(0); break;
  case caml_cudd_result_True: /* True */ *v = Val_int(1); break;
  case caml_cudd_result_Ifte: /* Ifte */
    *v=caml_alloc(3,0);
    caml_cudd_c2ml4(&tmp, &c->u.Ifte.cond);
    Store_field(*v,0,tmp);
    
    caml_cudd_c2ml1(man, &tmp, &c->u.Ifte.then_);
    Store_field(*v,1,tmp);
    
    caml_cudd_c2ml1(man, &tmp, &c->u.Ifte.else_);
    Store_field(*v,2,tmp);
    break;};
  CAMLreturn0;
  };
static void caml_cudd_c2ml2(caml_cudd_custom man, value * v,
caml_cudd_ref * c){
  caml_cudd_c2ml3(man, v, *c);
  };
extern value caml_cudd_stub_bdd_inspect(value man,
  value p){
  CAMLparam2(man, p);
  CAMLlocal1(p_r);
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_ref p1 = &(((struct { caml_cudd_result a; }) { ((caml_cudd_result) { }) }).a);
  caml_cudd_bdd_t p2 = ((caml_cudd_bdd_t) { });
  caml_cudd_ml2c(&man, &man1);
  caml_cudd_ml2c1(&p, &p2);
  bdd_inspect(man1, p1, p2);
  caml_cudd_c2ml2(man1, &p_r, &p1);
  CAMLreturn(p_r);
};

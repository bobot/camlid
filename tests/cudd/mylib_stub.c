#include "mylib_stub.h"
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include "./defs.h"
static DdManager* caml_cudd_cudd_init(){
  return Cudd_Init(0, 0, CUDD_UNIQUE_SLOTS, CUDD_CACHE_SLOTS, 0);
  };
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
  caml_cudd_custom res;
  value ret;
  res = caml_cudd_cudd_init();
  caml_cudd_c2ml(&ret, &res);
  return ret;
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
static void caml_cudd_free(caml_cudd_custom * c){  };
extern value caml_cudd_stub_Cudd_ReadOne(value man){
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_bdd_t res;
  value ret;
  caml_cudd_ml2c(&man, &man1);
  res = Cudd_ReadOne(man1);
  caml_cudd_c2ml1(man1, &ret, &res);
  caml_cudd_free(&man1);
  return ret;
};
extern value caml_cudd_stub_Cudd_ReadLogicZero(value man){
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_bdd_t res;
  value ret;
  caml_cudd_ml2c(&man, &man1);
  res = Cudd_ReadLogicZero(man1);
  caml_cudd_c2ml1(man1, &ret, &res);
  caml_cudd_free(&man1);
  return ret;
};
static void caml_cudd_ml2c1(value * v, caml_cudd_int * c){
  *c = Int_val(*v);
  };
static void caml_cudd_free1(caml_cudd_int * c){  };
extern value caml_cudd_stub_Cudd_bddIthVar(value man,
  value p){
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_int p1 = ((caml_cudd_int) { });
  caml_cudd_bdd_t res;
  value ret;
  caml_cudd_ml2c(&man, &man1);
  caml_cudd_ml2c1(&p, &p1);
  res = Cudd_bddIthVar(man1, p1);
  caml_cudd_c2ml1(man1, &ret, &res);
  caml_cudd_free(&man1);
  caml_cudd_free1(&p1);
  return ret;
};
extern value caml_cudd_stub_Cudd_bddNewVar(value man){
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_bdd_t res;
  value ret;
  caml_cudd_ml2c(&man, &man1);
  res = Cudd_bddNewVar(man1);
  caml_cudd_c2ml1(man1, &ret, &res);
  caml_cudd_free(&man1);
  return ret;
};
static void caml_cudd_bdd_get(caml_cudd_bdd_wrapper* i, caml_cudd_bdd_t* c){
  *c=i->ptr;
  };
static void caml_cudd_ml2c2(value * v, caml_cudd_bdd_t * c){
  caml_cudd_bdd_get((caml_cudd_bdd_wrapper *) Data_custom_val(*v), c);
  };
static void caml_cudd_free2(caml_cudd_bdd_t * c){  };
extern value caml_cudd_stub_Cudd_bddAnd(value man,
  value p,
  value p1){
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_bdd_t p2 = ((caml_cudd_bdd_t) { });
  caml_cudd_bdd_t p3 = ((caml_cudd_bdd_t) { });
  caml_cudd_bdd_t res;
  value ret;
  caml_cudd_ml2c(&man, &man1);
  caml_cudd_ml2c2(&p, &p2);
  caml_cudd_ml2c2(&p1, &p3);
  res = Cudd_bddAnd(man1, p2, p3);
  caml_cudd_c2ml1(man1, &ret, &res);
  caml_cudd_free(&man1);
  caml_cudd_free2(&p2);
  caml_cudd_free2(&p3);
  return ret;
};
extern value caml_cudd_stub_Cudd_bddOr(value man,
  value p,
  value p1){
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_bdd_t p2 = ((caml_cudd_bdd_t) { });
  caml_cudd_bdd_t p3 = ((caml_cudd_bdd_t) { });
  caml_cudd_bdd_t res;
  value ret;
  caml_cudd_ml2c(&man, &man1);
  caml_cudd_ml2c2(&p, &p2);
  caml_cudd_ml2c2(&p1, &p3);
  res = Cudd_bddOr(man1, p2, p3);
  caml_cudd_c2ml1(man1, &ret, &res);
  caml_cudd_free(&man1);
  caml_cudd_free2(&p2);
  caml_cudd_free2(&p3);
  return ret;
};
extern value caml_cudd_stub_Cudd_Not(value man,
  value p){
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_bdd_t p1 = ((caml_cudd_bdd_t) { });
  caml_cudd_bdd_t res;
  value ret;
  caml_cudd_ml2c(&man, &man1);
  caml_cudd_ml2c2(&p, &p1);
  res = Cudd_Not(p1);
  caml_cudd_c2ml1(man1, &ret, &res);
  caml_cudd_free(&man1);
  caml_cudd_free2(&p1);
  return ret;
};
static int caml_cudd_equal_bdd(caml_cudd_bdd_t p, caml_cudd_bdd_t p1){
  return (p == p1);
  
  };
static void caml_cudd_c2ml2(value * v, caml_cudd_bool * c){
  *v = Val_bool(*c);
  };
extern value caml_cudd_stub_equal_bdd(value p,
  value p1){
  caml_cudd_bdd_t p2 = ((caml_cudd_bdd_t) { });
  caml_cudd_bdd_t p3 = ((caml_cudd_bdd_t) { });
  caml_cudd_bool res;
  value ret;
  caml_cudd_ml2c2(&p, &p2);
  caml_cudd_ml2c2(&p1, &p3);
  res = caml_cudd_equal_bdd(p2, p3);
  caml_cudd_c2ml2(&ret, &res);
  caml_cudd_free2(&p2);
  caml_cudd_free2(&p3);
  return ret;
};
extern value caml_cudd_stub_Cudd_bddLeq(value man,
  value p,
  value p1){
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_bdd_t p2 = ((caml_cudd_bdd_t) { });
  caml_cudd_bdd_t p3 = ((caml_cudd_bdd_t) { });
  caml_cudd_bool res;
  value ret;
  caml_cudd_ml2c(&man, &man1);
  caml_cudd_ml2c2(&p, &p2);
  caml_cudd_ml2c2(&p1, &p3);
  res = Cudd_bddLeq(man1, p2, p3);
  caml_cudd_c2ml2(&ret, &res);
  caml_cudd_free(&man1);
  caml_cudd_free2(&p2);
  caml_cudd_free2(&p3);
  return ret;
};
static void caml_cudd_print(caml_cudd_custom man, caml_cudd_bdd_t p){
  fflush(stdout);
  Cudd_PrintMinterm(man,p);
  fflush(stdout);
  };
extern value caml_cudd_stub_print(value man,
  value p){
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_bdd_t p1 = ((caml_cudd_bdd_t) { });
  value ret;
  caml_cudd_ml2c(&man, &man1);
  caml_cudd_ml2c2(&p, &p1);
  caml_cudd_print(man1, p1);
  ret = Val_unit;
  caml_cudd_free(&man1);
  caml_cudd_free2(&p1);
  return ret;
};
static void caml_cudd_init1(caml_cudd_result * c){  };
static void caml_cudd_init(caml_cudd_ref * c){ caml_cudd_init1(*c); };
static void caml_cudd_c2ml5(value * v, caml_cudd_int1 * c){
  *v = Val_long(*c);
  };
static void caml_cudd_c2ml4(caml_cudd_custom man, value * v,
caml_cudd_result * c){
  CAMLparam0();
  CAMLlocal1(tmp);
  switch(c->tag){
  case caml_cudd_result_False: /* False */ *v = Val_int(0); break;
  case caml_cudd_result_True: /* True */ *v = Val_int(1); break;
  case caml_cudd_result_Ifte: /* Ifte */
    *v=caml_alloc(3,0);
    caml_cudd_c2ml5(&tmp, &c->u.Ifte.cond);
    Store_field(*v,0,tmp);
    
    caml_cudd_c2ml1(man, &tmp, &c->u.Ifte.then_);
    Store_field(*v,1,tmp);
    
    caml_cudd_c2ml1(man, &tmp, &c->u.Ifte.else_);
    Store_field(*v,2,tmp);
    break;};
  CAMLreturn0;
  };
static void caml_cudd_c2ml3(caml_cudd_custom man, value * v,
caml_cudd_ref * c){
  caml_cudd_c2ml4(man, v, *c);
  };
static void caml_cudd_free3(caml_cudd_ref * c){  };
extern value caml_cudd_stub_bdd_inspect(value man,
  value p){
  caml_cudd_custom man1 = ((caml_cudd_custom) { });
  caml_cudd_ref p1 = &(((struct { caml_cudd_result a; }) { ((caml_cudd_result) { }) }).a);
  caml_cudd_bdd_t p2 = ((caml_cudd_bdd_t) { });
  value ret;
  caml_cudd_ml2c(&man, &man1);
  caml_cudd_ml2c2(&p, &p2);
  caml_cudd_init(&p1);
  bdd_inspect(man1, p1, p2);
  caml_cudd_c2ml3(man1, &ret, &p1);
  caml_cudd_free(&man1);
  caml_cudd_free3(&p1);
  caml_cudd_free2(&p2);
  return ret;
};

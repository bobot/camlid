  $ ../camlid_toplevel.exe -stdin < gen.ml

  $ cat cudd_core_stub.c
  #include <caml/mlvalues.h>
  #include <caml/memory.h>
  #include <caml/alloc.h>
  #include <caml/custom.h>
  #include "./cudd.h"
  typedef DdManager* caml_cudd_custom;
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
  typedef DdNode* caml_cudd_bdd_t;
  static void caml_cudd_ml2c(value * v, caml_cudd_custom * c){
    *c = *((caml_cudd_custom *) Data_custom_val(*v));
    };
  struct caml_cudd_bdd_wrapper1 {caml_cudd_bdd_t ptr;DdManager* manager;};
  typedef struct caml_cudd_bdd_wrapper1 caml_cudd_bdd_wrapper;
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
  typedef int caml_cudd_int;
  static void caml_cudd_ml2c1(value * v, caml_cudd_int * c){
    *c = Int_val(*v);
    };
  static void caml_cudd_free1(caml_cudd_int * c){  };
  extern value caml_cudd_stub_Cudd_bddIthVar(value man,
    value v){
    caml_cudd_custom man1 = ((caml_cudd_custom) { });
    caml_cudd_int v1 = ((caml_cudd_int) { });
    caml_cudd_bdd_t res;
    value ret;
    caml_cudd_ml2c(&man, &man1);
    caml_cudd_ml2c1(&v, &v1);
    res = Cudd_bddIthVar(man1, v1);
    caml_cudd_c2ml1(man1, &ret, &res);
    caml_cudd_free(&man1);
    caml_cudd_free1(&v1);
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
    value v,
    value v1){
    caml_cudd_custom man1 = ((caml_cudd_custom) { });
    caml_cudd_bdd_t v2 = ((caml_cudd_bdd_t) { });
    caml_cudd_bdd_t v3 = ((caml_cudd_bdd_t) { });
    caml_cudd_bdd_t res;
    value ret;
    caml_cudd_ml2c(&man, &man1);
    caml_cudd_ml2c2(&v, &v2);
    caml_cudd_ml2c2(&v1, &v3);
    res = Cudd_bddAnd(man1, v2, v3);
    caml_cudd_c2ml1(man1, &ret, &res);
    caml_cudd_free(&man1);
    caml_cudd_free2(&v2);
    caml_cudd_free2(&v3);
    return ret;
  };
  extern value caml_cudd_stub_Cudd_bddOr(value man,
    value v,
    value v1){
    caml_cudd_custom man1 = ((caml_cudd_custom) { });
    caml_cudd_bdd_t v2 = ((caml_cudd_bdd_t) { });
    caml_cudd_bdd_t v3 = ((caml_cudd_bdd_t) { });
    caml_cudd_bdd_t res;
    value ret;
    caml_cudd_ml2c(&man, &man1);
    caml_cudd_ml2c2(&v, &v2);
    caml_cudd_ml2c2(&v1, &v3);
    res = Cudd_bddOr(man1, v2, v3);
    caml_cudd_c2ml1(man1, &ret, &res);
    caml_cudd_free(&man1);
    caml_cudd_free2(&v2);
    caml_cudd_free2(&v3);
    return ret;
  };
  extern value caml_cudd_stub_Cudd_Not(value man,
    value v){
    caml_cudd_custom man1 = ((caml_cudd_custom) { });
    caml_cudd_bdd_t v1 = ((caml_cudd_bdd_t) { });
    caml_cudd_bdd_t res;
    value ret;
    caml_cudd_ml2c(&man, &man1);
    caml_cudd_ml2c2(&v, &v1);
    res = Cudd_Not(v1);
    caml_cudd_c2ml1(man1, &ret, &res);
    caml_cudd_free(&man1);
    caml_cudd_free2(&v1);
    return ret;
  };
  typedef int caml_cudd_bool;
  static int caml_cudd_equal_bdd(caml_cudd_bdd_t b, caml_cudd_bdd_t b1){
    return (b == b1);
    
    };
  static void caml_cudd_c2ml2(value * v, caml_cudd_bool * c){
    *v = Val_bool(*c);
    };
  extern value caml_cudd_stub_equal_bdd(value b,
    value b1){
    caml_cudd_bdd_t b2 = ((caml_cudd_bdd_t) { });
    caml_cudd_bdd_t b3 = ((caml_cudd_bdd_t) { });
    caml_cudd_bool res;
    value ret;
    caml_cudd_ml2c2(&b, &b2);
    caml_cudd_ml2c2(&b1, &b3);
    res = caml_cudd_equal_bdd(b2, b3);
    caml_cudd_c2ml2(&ret, &res);
    caml_cudd_free2(&b2);
    caml_cudd_free2(&b3);
    return ret;
  };
  extern value caml_cudd_stub_Cudd_bddLeq(value man,
    value v,
    value v1){
    caml_cudd_custom man1 = ((caml_cudd_custom) { });
    caml_cudd_bdd_t v2 = ((caml_cudd_bdd_t) { });
    caml_cudd_bdd_t v3 = ((caml_cudd_bdd_t) { });
    caml_cudd_bool res;
    value ret;
    caml_cudd_ml2c(&man, &man1);
    caml_cudd_ml2c2(&v, &v2);
    caml_cudd_ml2c2(&v1, &v3);
    res = Cudd_bddLeq(man1, v2, v3);
    caml_cudd_c2ml2(&ret, &res);
    caml_cudd_free(&man1);
    caml_cudd_free2(&v2);
    caml_cudd_free2(&v3);
    return ret;
  };
  static void caml_cudd_print(caml_cudd_custom man, caml_cudd_bdd_t b){
    fflush(stdout);
    Cudd_PrintMinterm(man,b);
    fflush(stdout);
    };
  extern value caml_cudd_stub_print(value man,
    value b){
    caml_cudd_custom man1 = ((caml_cudd_custom) { });
    caml_cudd_bdd_t b1 = ((caml_cudd_bdd_t) { });
    value ret;
    caml_cudd_ml2c(&man, &man1);
    caml_cudd_ml2c2(&b, &b1);
    caml_cudd_print(man1, b1);
    ret = Val_unit;
    caml_cudd_free(&man1);
    caml_cudd_free2(&b1);
    return ret;
  };
  typedef intnat caml_cudd_int1;
  typedef struct {
    enum {
      caml_cudd_result_False,
      caml_cudd_result_True,
      caml_cudd_result_Ifte,
      } tag;
    union {
      struct {
        caml_cudd_int1 cond;
        caml_cudd_bdd_t then_;
        caml_cudd_bdd_t else_;
        
        } Ifte;
        } u; } caml_cudd_result;
  typedef caml_cudd_result * caml_cudd_ref;
  static void caml_cudd_init1(caml_cudd_result * c){  };
  static void caml_cudd_init(caml_cudd_ref * c){ caml_cudd_init1(*c); };
  static void caml_cudd_True(caml_cudd_result* dst){
    dst->tag=caml_cudd_result_True/*65*/;
    };
  static void caml_cudd_False(caml_cudd_result* dst){
    dst->tag=caml_cudd_result_False/*66*/;
    };
  static void caml_cudd_Ifte(caml_cudd_result* dst, caml_cudd_int1* cond,
  caml_cudd_bdd_t* then_, caml_cudd_bdd_t* else_){
    dst->tag=caml_cudd_result_Ifte;
    dst->u.Ifte.cond = *cond;
    dst->u.Ifte.then_ = *then_;
    dst->u.Ifte.else_ = *else_;
    
    };
  static void caml_cudd_bdd_inspect(caml_cudd_custom man, caml_cudd_bdd_t vbdd,
  caml_cudd_ref dst){
    if(Cudd_IsConstant(vbdd)){
      if(vbdd == Cudd_ReadOne(man)){
        caml_cudd_True(dst);
        } else {
        caml_cudd_False(dst);
        };
      } else {
      caml_cudd_bdd_t th = Cudd_T(vbdd);
      caml_cudd_bdd_t el = Cudd_E(vbdd);
      if(Cudd_IsComplement(vbdd)){ th = Cudd_Not(th);el = Cudd_Not(el); };
      caml_cudd_Ifte(dst, Cudd_NodeReadIndex(vbdd), th, el);
      };
    };
  static void caml_cudd_c2ml5(value * v, caml_cudd_int1 * c){
    *v = Val_long(*c);
    };
  static void caml_cudd_c2ml4(caml_cudd_custom man, value * v,
  caml_cudd_result * c){
    CAMLparam0();
    CAMLlocal1(tmp);
    switch(c->tag){
    case caml_cudd_result_Ifte: /* Ifte */
      *v=caml_alloc(3,0);
      caml_cudd_c2ml5(&tmp, &c->u.Ifte.cond);
      Store_field(*v,0,tmp);
      caml_cudd_c2ml1(man, &tmp, &c->u.Ifte.then_);
      Store_field(*v,1,tmp);
      caml_cudd_c2ml1(man, &tmp, &c->u.Ifte.else_);
      Store_field(*v,2,tmp);
      break;
    case caml_cudd_result_True: /* True */ *v = Val_int(1); break;
    case caml_cudd_result_False: /* False */ *v = Val_int(0); break;
    };
    CAMLreturn0;
    };
  static void caml_cudd_c2ml3(caml_cudd_custom man, value * v,
  caml_cudd_ref * c){
    caml_cudd_c2ml4(man, v, *c);
    };
  static void caml_cudd_free3(caml_cudd_ref * c){  };
  extern value caml_cudd_stub_bdd_inspect(value man,
    value vbdd){
    caml_cudd_custom man1 = ((caml_cudd_custom) { });
    caml_cudd_bdd_t vbdd1 = ((caml_cudd_bdd_t) { });
    caml_cudd_ref dst = &(((struct { caml_cudd_result a; }) { ((caml_cudd_result) { }) }).a);
    value ret;
    caml_cudd_ml2c(&man, &man1);
    caml_cudd_ml2c2(&vbdd, &vbdd1);
    caml_cudd_init(&dst);
    caml_cudd_bdd_inspect(man1, vbdd1, dst);
    caml_cudd_c2ml3(man1, &ret, &dst);
    caml_cudd_free(&man1);
    caml_cudd_free2(&vbdd1);
    caml_cudd_free3(&dst);
    return ret;
  };

  $ ocamlc -ccopt --warn-all -c cudd_core_stub.c
  cudd_core_stub.c: In function 'caml_cudd_bdd_inspect':
  cudd_core_stub.c:273:25: error: passing argument 2 of 'caml_cudd_Ifte' makes pointer from integer without a cast [-Wint-conversion]
    273 |     caml_cudd_Ifte(dst, Cudd_NodeReadIndex(vbdd), th, el);
        |                         ^~~~~~~~~~~~~~~~~~~~~~~~
        |                         |
        |                         unsigned int
  cudd_core_stub.c:253:67: note: expected 'caml_cudd_int1 *' {aka 'long int *'} but argument is of type 'unsigned int'
    253 | static void caml_cudd_Ifte(caml_cudd_result* dst, caml_cudd_int1* cond,
        |                                                   ~~~~~~~~~~~~~~~~^~~~
  cudd_core_stub.c:273:51: error: passing argument 3 of 'caml_cudd_Ifte' from incompatible pointer type [-Wincompatible-pointer-types]
    273 |     caml_cudd_Ifte(dst, Cudd_NodeReadIndex(vbdd), th, el);
        |                                                   ^~
        |                                                   |
        |                                                   caml_cudd_bdd_t {aka DdNode *}
  cudd_core_stub.c:254:18: note: expected 'DdNode **' but argument is of type 'caml_cudd_bdd_t' {aka 'DdNode *'}
    254 | caml_cudd_bdd_t* then_, caml_cudd_bdd_t* else_){
        | ~~~~~~~~~~~~~~~~~^~~~~
  cudd_core_stub.c:273:55: error: passing argument 4 of 'caml_cudd_Ifte' from incompatible pointer type [-Wincompatible-pointer-types]
    273 |     caml_cudd_Ifte(dst, Cudd_NodeReadIndex(vbdd), th, el);
        |                                                       ^~
        |                                                       |
        |                                                       caml_cudd_bdd_t {aka DdNode *}
  cudd_core_stub.c:254:42: note: expected 'DdNode **' but argument is of type 'caml_cudd_bdd_t' {aka 'DdNode *'}
    254 | caml_cudd_bdd_t* then_, caml_cudd_bdd_t* else_){
        |                         ~~~~~~~~~~~~~~~~~^~~~~
  [2]

  $ cat cudd_core.ml
  type caml_cudd_man
  type caml_cudd_bdd
  type caml_cudd_int = int
  type caml_cudd_bool = bool
  type caml_cudd_int1 = int
  type caml_cudd_result =
    | False
    | True
    | Ifte of caml_cudd_int1 * caml_cudd_bdd * caml_cudd_bdd
  type caml_cudd_ptr_ref = caml_cudd_result
  external init: unit -> caml_cudd_man = "caml_cudd_stub_cudd_init"
  external bdd_true:
    caml_cudd_man ->
    caml_cudd_bdd
    = "caml_cudd_stub_Cudd_ReadOne"
  external bdd_false:
    caml_cudd_man ->
    caml_cudd_bdd
    = "caml_cudd_stub_Cudd_ReadLogicZero"
  external bdd_var:
    caml_cudd_man ->
    caml_cudd_int ->
    caml_cudd_bdd
    = "caml_cudd_stub_Cudd_bddIthVar"
  external bdd_newvar:
    caml_cudd_man ->
    caml_cudd_bdd
    = "caml_cudd_stub_Cudd_bddNewVar"
  external bdd_and:
    caml_cudd_man ->
    caml_cudd_bdd ->
    caml_cudd_bdd ->
    caml_cudd_bdd
    = "caml_cudd_stub_Cudd_bddAnd"
  external bdd_or:
    caml_cudd_man ->
    caml_cudd_bdd ->
    caml_cudd_bdd ->
    caml_cudd_bdd
    = "caml_cudd_stub_Cudd_bddOr"
  external bdd_not:
    caml_cudd_man ->
    caml_cudd_bdd ->
    caml_cudd_bdd
    = "caml_cudd_stub_Cudd_Not"
  external bdd_is_equal:
    caml_cudd_bdd ->
    caml_cudd_bdd ->
    caml_cudd_bool
    = "caml_cudd_stub_equal_bdd"
  external bdd_leq:
    caml_cudd_man ->
    caml_cudd_bdd ->
    caml_cudd_bdd ->
    caml_cudd_bool
    = "caml_cudd_stub_Cudd_bddLeq"
  external print:
    caml_cudd_man ->
    caml_cudd_bdd ->
    unit
    = "caml_cudd_stub_print"
  external inspect:
    caml_cudd_man ->
    caml_cudd_bdd ->
    caml_cudd_ptr_ref
    = "caml_cudd_stub_bdd_inspect"

  $ ocamlc -c cudd_core.ml

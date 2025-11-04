  $ cat > cudd.h <<EOF
  > #include <inttypes.h>
  > typedef struct DdNode DdNode;
  > typedef struct DdManager DdManager;
  > #define CUDD_UNIQUE_SLOTS	256	/**< Initial size of subtables */
  > #define CUDD_CACHE_SLOTS	262144	/**< Default size of the cache */
  > extern DdManager * Cudd_Init(unsigned int numVars, unsigned int numVarsZ, unsigned int numSlots, unsigned int cacheSize, size_t maxMemory);
  > extern void Cudd_Quit(DdManager *unique);
  > extern void Cudd_RecursiveDeref(DdManager *table, DdNode *n);
  > extern DdNode * Cudd_ReadOne(DdManager *dd);
  > extern DdNode * Cudd_ReadZero(DdManager *dd);
  > extern DdNode * Cudd_bddAnd(DdManager *dd, DdNode *f, DdNode *g);
  > extern DdNode * Cudd_bddOr(DdManager *dd, DdNode *f, DdNode *g);
  > #define Cudd_Not(node) ((DdNode *)((uintptr_t)(node) ^ (uintptr_t) 01))
  > extern void Cudd_Ref(DdNode *n);
  > extern int Cudd_PrintMinterm(DdManager *manager, DdNode *node);
  > EOF

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
    }
  void Cudd_Quit(caml_cudd_custom);
  static void caml_cudd_finalize_op(value v){
    Cudd_Quit(*(caml_cudd_custom *) Data_custom_val(v));
    }
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
    }
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
    }
  caml_cudd_bdd_t Cudd_ReadOne(caml_cudd_custom);
  struct caml_cudd_bdd_wrapper1 {caml_cudd_bdd_t ptr;DdManager* manager;};
  typedef struct caml_cudd_bdd_wrapper1 caml_cudd_bdd_wrapper;
  static void caml_cudd_bdd_finalize(caml_cudd_bdd_wrapper* i){
    Cudd_RecursiveDeref(i->manager,i->ptr);
    }
  static void caml_cudd_finalize_op1(value v){
    caml_cudd_bdd_finalize((caml_cudd_bdd_wrapper *) Data_custom_val(v));
    }
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
    }
  static void caml_cudd_c2ml1(caml_cudd_custom man, value * v,
  caml_cudd_bdd_t * c){
    *v = caml_alloc_custom(&caml_cudd_cops1,sizeof(caml_cudd_bdd_wrapper), 0, 1);
    caml_cudd_bdd_set(man, (caml_cudd_bdd_wrapper *) Data_custom_val(*v), c);
    }
  static void caml_cudd_free(caml_cudd_custom * c){  }
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
  caml_cudd_bdd_t Cudd_ReadLogicZero(caml_cudd_custom);
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
    }
  caml_cudd_bdd_t Cudd_bddIthVar(caml_cudd_custom, caml_cudd_int);
  static void caml_cudd_free1(caml_cudd_int * c){  }
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
  caml_cudd_bdd_t Cudd_bddNewVar(caml_cudd_custom);
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
    }
  static void caml_cudd_ml2c2(value * v, caml_cudd_bdd_t * c){
    caml_cudd_bdd_get((caml_cudd_bdd_wrapper *) Data_custom_val(*v), c);
    }
  caml_cudd_bdd_t Cudd_bddAnd(caml_cudd_custom,
    caml_cudd_bdd_t,
    caml_cudd_bdd_t);
  static void caml_cudd_free2(caml_cudd_bdd_t * c){  }
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
  caml_cudd_bdd_t Cudd_bddOr(caml_cudd_custom,
    caml_cudd_bdd_t,
    caml_cudd_bdd_t);
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
  caml_cudd_bdd_t Cudd_bddNot(caml_cudd_custom, caml_cudd_bdd_t);
  extern value caml_cudd_stub_Cudd_bddNot(value man,
    value v){
    caml_cudd_custom man1 = ((caml_cudd_custom) { });
    caml_cudd_bdd_t v1 = ((caml_cudd_bdd_t) { });
    caml_cudd_bdd_t res;
    value ret;
    caml_cudd_ml2c(&man, &man1);
    caml_cudd_ml2c2(&v, &v1);
    res = Cudd_bddNot(man1, v1);
    caml_cudd_c2ml1(man1, &ret, &res);
    caml_cudd_free(&man1);
    caml_cudd_free2(&v1);
    return ret;
  };
  static int caml_cudd_equal_bdd(caml_cudd_bdd_t b, caml_cudd_bdd_t b1){
    return (b == b1);
    
    }
  extern value caml_cudd_stub_equal_bdd(value b,
    value b1){
    caml_cudd_bdd_t b2 = ((caml_cudd_bdd_t) { });
    caml_cudd_bdd_t b3 = ((caml_cudd_bdd_t) { });
    value ret;
    caml_cudd_ml2c2(&b, &b2);
    caml_cudd_ml2c2(&b1, &b3);
    caml_cudd_equal_bdd(b2, b3);
    ret = Val_unit;
    caml_cudd_free2(&b2);
    caml_cudd_free2(&b3);
    return ret;
  };
  static void caml_cudd_print(caml_cudd_custom man, caml_cudd_bdd_t b){
    fflush(stdout);
    Cudd_PrintMinterm(man,b);
    fflush(stdout);
    }
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

  $ ocamlc -ccopt --warn-all -c cudd_core_stub.c

  $ cat cudd_core.ml
  type caml_cudd_man
  type caml_cudd_bdd
  type caml_cudd_int = int
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
    = "caml_cudd_stub_Cudd_bddNot"
  external bdd_is_equal:
    caml_cudd_bdd ->
    caml_cudd_bdd ->
    unit
    = "caml_cudd_stub_equal_bdd"
  external print:
    caml_cudd_man ->
    caml_cudd_bdd ->
    unit
    = "caml_cudd_stub_print"

  $ ocamlc -c cudd_core.ml

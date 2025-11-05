#include "./cudd_core_stub.h"

static void bdd_inspect(DdManager* man, caml_cudd_result* dst, DdNode* vbdd){
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

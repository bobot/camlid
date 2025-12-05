#include "mylib_stub.h"

static DdManager* cudd_init(void){
    return Cudd_Init(0, 0, CUDD_UNIQUE_SLOTS, CUDD_CACHE_SLOTS, 0);
}

static void bdd_inspect(DdManager *man, caml_cudd_result *dst, DdNode *src)
{

    if (Cudd_IsConstant(src))
    {
        if (src == Cudd_ReadOne(man))
            caml_cudd_mk_result_True(dst);
        else
            caml_cudd_mk_result_False(dst);
    }
    else
    {
        DdNode *th = Cudd_T(src);
        DdNode *el = Cudd_E(src);
        intptr_t v = (intptr_t) Cudd_NodeReadIndex(src);
        if (Cudd_IsComplement(src))
        {
            th = Cudd_Not(th);
            el = Cudd_Not(el);
        };
        caml_cudd_mk_result_Ifte(dst, &v, &th, &el);
    }
}

static int equal_bdd(DdNode *a, DdNode *b){
    return (a == b);
}

static void bdd_print(DdManager *man, DdNode *a){
    fflush(stdout);
    Cudd_PrintMinterm(man,a);
    fflush(stdout);
}

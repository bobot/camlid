#include "mylib_stub.h"

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
        intptr_t v = Cudd_NodeReadIndex(src);
        if (Cudd_IsComplement(src))
        {
            th = Cudd_Not(th);
            el = Cudd_Not(el);
        };
        caml_cudd_mk_result_Ifte(dst, &v, &th, &el);
    }
}
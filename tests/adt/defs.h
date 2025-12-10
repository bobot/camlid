#include "mylib_stub.h"

static inline void lib_t_of_adt(lib_t * dst, camlid_t * src){
    switch(src->tag){
        case camlid_t_T1:
            dst->b.a = src->u.T1.a;
            dst->b.b = src->u.T1.b;
            break;
        case camlid_t_T2:
            break;
        case camlid_t_T3:
            dst->b.a = src->u.T3.c;
            break;
    }
}

static inline void adt_of_lib_t(camlid_t * dst, lib_t * src){
    if(src->b.a == 0 && src->b.b == 0) camlid_mk_t_T2(dst);
    else if(src->b.b == 0) camlid_mk_t_T3(dst,&(src->c.a));
    else camlid_mk_t_T1(dst,&(src->b.a),&(src->b.b));
}

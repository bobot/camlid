#include "mylib_stub.h"

static inline void lib_t_of_adt(lib_t * dst, camlid_t * src){
    camlid_t_match(src) {
        camlid_t_of_T1(a, b) {
            dst->b.a = *a;
            dst->b.b = *b;
        }
        camlid_t_of_T2() {}
        camlid_t_of_T3(c) { dst->b.a = *c; }
    }
}

static inline void adt_of_lib_t(camlid_t * dst, lib_t * src){
    if(src->b.a == 0 && src->b.b == 0) camlid_mk_t_T2(dst);
    else if(src->b.b == 0) camlid_mk_t_T3(dst,&(src->c.a));
    else camlid_mk_t_T1(dst,&(src->b.a),&(src->b.b));
}

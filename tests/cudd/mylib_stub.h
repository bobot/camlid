// generated using generator.exe and camlid
#ifndef mylib
#define mylib
#include <stddef.h>
#include <inttypes.h>
#include "./cudd.h"
typedef DdNode* caml_cudd_bdd_t;
struct caml_cudd_bdd_wrapper1 {caml_cudd_bdd_t ptr;DdManager* manager;};
typedef struct caml_cudd_bdd_wrapper1 caml_cudd_bdd_wrapper;
typedef struct {
  enum {
    caml_cudd_result_False,
    caml_cudd_result_True,
    caml_cudd_result_Ifte} tag;
  union {
    
    
    struct {
      intptr_t cond;
      caml_cudd_bdd_t then_;
      caml_cudd_bdd_t else_;
      } Ifte;} u; } caml_cudd_result;
// @brief Fill [dst] with the constant case False of result
// @param dst structure to fill
static inline void caml_cudd_mk_result_False(caml_cudd_result* dst){
  dst->tag=caml_cudd_result_False;
  }
// @brief Fill [dst] with the constant case True of result
// @param dst structure to fill
static inline void caml_cudd_mk_result_True(caml_cudd_result* dst){
  dst->tag=caml_cudd_result_True;
  }
// @brief Fill [dst] with the constructor Ifte of result
// @param dst structure to fill
// @param cond reference is assigned to the constructor field
// @param then_ reference is assigned to the constructor field
// @param else_ reference is assigned to the constructor field

static inline void caml_cudd_mk_result_Ifte(caml_cudd_result* dst,
intptr_t* cond, caml_cudd_bdd_t* then_, caml_cudd_bdd_t* else_){
  dst->tag=caml_cudd_result_Ifte;
  dst->u.Ifte.cond = *cond;
  dst->u.Ifte.then_ = *then_;
  dst->u.Ifte.else_ = *else_;
  }
typedef caml_cudd_result caml_cudd_algdata;
typedef caml_cudd_algdata * caml_cudd_ref;
#endif
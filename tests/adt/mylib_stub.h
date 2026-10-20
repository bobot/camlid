// generated using generator.exe and camlid
#ifndef mylib
#define mylib
#include <stddef.h>
#include <inttypes.h>
#include "lib.h"
typedef lib_t * camlid_ref;
typedef struct {
  enum { camlid_t_T1, camlid_t_T2, camlid_t_T3} tag;
  union { struct { int a; double b; } T1;  struct { int c; } T3;} u; 
  } camlid_t;
#define camlid_t_match(x) for(camlid_t *cid_m = (x); cid_m != NULL; cid_m = NULL) switch((int)cid_m->tag) case -1:
#define camlid_t_otherwise break; default:
#define camlid_t_of_T1(a, b) break; case camlid_t_T1: \
  for(int cid_once = 1; cid_once; cid_once = 0) \
  for(int *a = &cid_m->u.T1.a; cid_once; cid_once = 0) \
  for(double *b = &cid_m->u.T1.b; cid_once; cid_once = 0)
#define camlid_t_of_T2() break; case camlid_t_T2:
#define camlid_t_of_T3(c) break; case camlid_t_T3: \
  for(int cid_once = 1; cid_once; cid_once = 0) \
  for(int *c = &cid_m->u.T3.c; cid_once; cid_once = 0)
// @brief Fill [dst] with the constructor T1 of t
// @param dst structure to fill
// @param a reference is assigned to the constructor field
// @param b reference is assigned to the constructor field

static inline void camlid_mk_t_T1(camlid_t* dst, int* a, double* b){
  dst->tag=camlid_t_T1;
  dst->u.T1.a = *a;
  dst->u.T1.b = *b;
  }
// @brief Fill [dst] with the constant case T2 of t
// @param dst structure to fill
static inline void camlid_mk_t_T2(camlid_t* dst){ dst->tag=camlid_t_T2; }
// @brief Fill [dst] with the constructor T3 of t
// @param dst structure to fill
// @param c reference is assigned to the constructor field

static inline void camlid_mk_t_T3(camlid_t* dst, int* c){
  dst->tag=camlid_t_T3;
  dst->u.T3.c = *c;
  }
typedef camlid_t camlid_algdata;
#endif

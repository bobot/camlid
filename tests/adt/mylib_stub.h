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
typedef name_t * camlid_ref1;
typedef char * camlid_string_nt;
typedef struct {
  enum { camlid_name_Anonymous, camlid_name_Named, camlid_name_none} tag;
  union {  struct { camlid_string_nt s; } Named;} u; } camlid_name;
#define camlid_name_match(x) for(camlid_name *cid_m = (x); cid_m != NULL; cid_m = NULL) switch((int)cid_m->tag) case -1:
#define camlid_name_otherwise break; default:
#define camlid_name_of_Anonymous() break; case camlid_name_Anonymous:
#define camlid_name_of_Named(s) break; case camlid_name_Named: \
  for(int cid_once = 1; cid_once; cid_once = 0) \
  for(camlid_string_nt *s = &cid_m->u.Named.s; cid_once; cid_once = 0)
// @brief Fill [dst] with the constant case Anonymous of name
// @param dst structure to fill
static inline void camlid_mk_name_Anonymous(camlid_name* dst){
  dst->tag=camlid_name_Anonymous;
  }
// @brief Fill [dst] with the constructor Named of name
// @param dst structure to fill
// @param s reference is assigned to the constructor field

static inline void camlid_mk_name_Named(camlid_name* dst,
camlid_string_nt* s){
  dst->tag=camlid_name_Named;
  dst->u.Named.s = *s;
  }
typedef camlid_name camlid_algdata1;
#endif

// generated using generator.exe and camlid
#ifndef mylib
#define mylib
#include <stddef.h>
#include <inttypes.h>
#include "lib.h"
typedef intptr_t * camlid_ref;
typedef struct {
  enum { camlid_result_Data, camlid_result_Error} tag;
  union {
    struct { intptr_t data; } Data;
    struct { intptr_t error; } Error;} u; } camlid_result;
#define camlid_result_match(x) for(camlid_result *cid_m = (x); cid_m != NULL; cid_m = NULL) switch((int)cid_m->tag) case -1:
#define camlid_result_otherwise break; default:
#define camlid_result_of_Data(data) break; case camlid_result_Data: \
  for(int cid_once = 1; cid_once; cid_once = 0) \
  for(intptr_t *data = &cid_m->u.Data.data; cid_once; cid_once = 0)
#define camlid_result_of_Error(error) break; case camlid_result_Error: \
  for(int cid_once = 1; cid_once; cid_once = 0) \
  for(intptr_t *error = &cid_m->u.Error.error; cid_once; cid_once = 0)
// @brief Fill [dst] with the constructor Data of result
// @param dst structure to fill
// @param data reference is assigned to the constructor field

static inline void camlid_mk_result_Data(camlid_result* dst, intptr_t* data){
  dst->tag=camlid_result_Data;
  dst->u.Data.data = *data;
  }
// @brief Fill [dst] with the constructor Error of result
// @param dst structure to fill
// @param error reference is assigned to the constructor field

static inline void camlid_mk_result_Error(camlid_result* dst,
intptr_t* error){
  dst->tag=camlid_result_Error;
  dst->u.Error.error = *error;
  }
typedef camlid_result camlid_algdata;
#endif

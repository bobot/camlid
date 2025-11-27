// generated using generator.exe and camlid
#ifndef mylib
#define mylib
#include <stddef.h>
#include <inttypes.h>
#include "lib.h"
typedef intptr_t camlid_int;
typedef camlid_int * camlid_ref;
typedef struct {
  enum { camlid_result_Data, camlid_result_Error} tag;
  union {
    struct { camlid_int data; } Data;
    struct { camlid_int error; } Error;} u; } camlid_result;
// @brief Fill [dst] with the constructor Data of result
// @param dst structure to fill
// @param data reference is assigned to the constructor field

static void camlid_mk_result_Data(camlid_result* dst, camlid_int* data){
  dst->tag=camlid_result_Data;
  dst->u.Data.data = *data;
  }
// @brief Fill [dst] with the constructor Error of result
// @param dst structure to fill
// @param error reference is assigned to the constructor field

static void camlid_mk_result_Error(camlid_result* dst, camlid_int* error){
  dst->tag=camlid_result_Error;
  dst->u.Error.error = *error;
  }
typedef camlid_result camlid_algdata;
#endif
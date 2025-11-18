#include <stddef.h>
#include <inttypes.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include "lib.h"
typedef intptr_t camlid_int;
typedef int * camlid_custom;
static void camlid_ml2c(value * v, camlid_int * c){ *c = Long_val(*v); };
void initialize_ptr(camlid_custom *);
static void camlid_init(camlid_custom * c){ initialize_ptr(c); };
void finalize_ptr(camlid_custom *);
static void camlid_finalize_op(value v){
  finalize_ptr((camlid_custom *) Data_custom_val(v));
  };
int compare_ptr(camlid_custom *, camlid_custom *);
static int camlid_compare_op(value v1, value v2){
  return compare_ptr(
           (camlid_custom *) Data_custom_val(v1),
           (camlid_custom *) Data_custom_val(v2)
         );
  };
intptr_t hash_ptr(camlid_custom *);
static intptr_t camlid_hash_op(value v){
  return hash_ptr((camlid_custom *) Data_custom_val(v));
  };
struct custom_operations camlid_cops = {
NULL,
camlid_finalize_op,
camlid_compare_op,
camlid_hash_op,
custom_serialize_default,
custom_deserialize_default
};
static void camlid_c2ml(value * v, camlid_custom * c){
  *v = caml_alloc_custom(&camlid_cops,sizeof(camlid_custom), 0, 1);
  *((camlid_custom *) Data_custom_val(*v)) = *c;
  };
static void camlid_free(camlid_int * c){  };
static void camlid_free1(camlid_custom * c){  };
extern value camlid_stub_of_int(value p){
  camlid_int p1 = ((camlid_int) { });
  camlid_custom p2 = ((camlid_custom) { });
  value ret;
  camlid_ml2c(&p, &p1);
  camlid_init(&p2);
  of_int(p1, p2);
  camlid_c2ml(&ret, &p2);
  camlid_free(&p1);
  camlid_free1(&p2);
  return ret;
};
static void camlid_ml2c1(value * v, camlid_custom * c){
  *c = *((camlid_custom *) Data_custom_val(*v));
  };
static void camlid_c2ml1(value * v, camlid_int * c){ *v = Val_long(*c); };
extern value camlid_stub_to_int(value p){
  camlid_custom p1 = ((camlid_custom) { });
  camlid_int res;
  value ret;
  camlid_ml2c1(&p, &p1);
  res = to_int(p1);
  camlid_c2ml1(&ret, &res);
  camlid_free1(&p1);
  return ret;
};

  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > module Result = struct
  >    open Expert.AlgData
  >    let uc = start
  >    let data,uc = close_constr "Data" (("data",int) + const) uc
  >    let error,uc = close_constr "Error" (("error",int) + const) uc
  >    let _,ty = close "result" uc
  > end
  > 
  > let data, data_or_status =
  >   let data = ignored (ptr_ref int) "data" in
  >   let to_ml : Expert.to_ml =
  >       let dst = Expr.Var.mk "dst" (expr "%a *" pp_code Result.ty.cty) in
  >       let src = Expr.Var.mk "src" (expr "%a *" pp_code int.cty) in
  >        { dst; src; to_ml =
  >           Type.code "to_ml" "if(*%a){%a}{%a};"
  >       Expr.pp_var src (Expert.AlgData.make Result.data ~dst:(expr "%a" Expr.pp_var dst) (expr "%a" Expr.pp_var data.pc)).expr ()
  >        (Expert.AlgData.make Result.error ~dst:(expr "%a" Expr.pp_var dst)  (expr "%a" Expr.pp_var src)).expr ()
  > 
  >        }
  >   in
  >   let ty = Expert.convert ~to_ml ~c:int ~ml:Result.ty () in
  >   data, {
  >              Type.rty = ty;
  >              routput = true;
  >              rc = Expr.Var.mk "res" (expr "%a" pp_code ty.cty);
  >              binds = [];
  >            }
  >   
  > 
  > let () = Generate.to_file "basic" [
  >  let other_input = input int "other" in
  >  let fid = Expert.declare_existing
  >      ~result:(expr "%a" pp_code int.cty)
  >      "f" [other_input.pc;data.pc] in
  >  Expert.print_ml_fun {mlname="f";fid;params=[data;other_input;];
  >      result = Some data_or_status}
  > ]
  > EOF

  $ cat basic_stub.c
  #include <caml/mlvalues.h>
  #include <caml/memory.h>
  #include <caml/alloc.h>
  #include <caml/custom.h>
  typedef intnat camlid_int;
  typedef camlid_int * camlid_ref;
  static void camlid_ml2c(value * v, camlid_int * c){ *c = Long_val(*v); };
  static void camlid_init1(camlid_int * c){  };
  static void camlid_init(camlid_ref * c){ camlid_init1(*c); };
  camlid_int f(camlid_int, camlid_ref);
  typedef struct {
    enum { camlid_result_Data, camlid_result_Error, } tag;
    union {
      struct { camlid_int error;  } Error; struct { camlid_int data;  } Data; } u; 
    } camlid_result;
  static void camlid_Data(camlid_result* dst, camlid_int* data){
    dst->tag=camlid_result_Data;
    dst->u.Data.data = *data;
    
    };
  static void camlid_Error(camlid_result* dst, camlid_int* error){
    dst->tag=camlid_result_Error;
    dst->u.Error.error = *error;
    
    };
  static void camlid_to_ml(camlid_ref data, camlid_result * dst,
  camlid_int * src){
    if(*src){camlid_Data(dst, data);}{camlid_Error(dst, src);};
    };
  static void camlid_c2ml2(value * v, camlid_int * c){ *v = Val_long(*c); };
  static void camlid_c2ml1(value * v, camlid_result * c){
    CAMLparam0();
    CAMLlocal1(tmp);
    switch(c->tag){
    case camlid_result_Error: /* Error */
      *v=caml_alloc(1,1);
      camlid_c2ml2(&tmp, &c->u.Error.error);
      Store_field(*v,0,tmp);
      break;
    case camlid_result_Data: /* Data */
      *v=caml_alloc(1,0);
      camlid_c2ml2(&tmp, &c->u.Data.data);
      Store_field(*v,0,tmp);
      break;
    };
    CAMLreturn0;
    };
  static void camlid_c2ml(camlid_int * c, value * v, camlid_ref data){
    camlid_result tmp; camlid_to_ml(data, &tmp, c);
    camlid_c2ml1(v, &tmp);
    };
  static void camlid_free(camlid_ref * c){  };
  static void camlid_free1(camlid_int * c){  };
  extern value camlid_stub_f(value other){
    camlid_ref data = &(((struct { camlid_int a; }) { ((camlid_int) { }) }).a);
    camlid_int other1 = ((camlid_int) { });
    camlid_int res;
    value ret;
    camlid_ml2c(&other, &other1);
    camlid_init(&data);
    res = f(other1, data);
    camlid_c2ml(&res, &ret, data);
    camlid_free(&data);
    camlid_free1(&other1);
    return ret;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c


  $ cat basic.ml
  type camlid_int = int
  type camlid_result = | Data of camlid_int | Error of camlid_int
  external f: camlid_int -> camlid_result = "camlid_stub_f"

  $ ocamlc -c basic.ml

  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Helper
  > 
  > let () = Generate.to_file "basic" [
  >  func "f_nat" [ inout int "x"];
  >  func "f_int" [ inout int_trunc "x"];
  >  func "f_double" [ inout double "x"];
  >  func "f_int32" [ inout int32 "x"];
  >  func "f_int64" [ inout int64 "x"];
  >  func "f_nativeint" [ inout nativeint "x"];
  > ]
  > ~headers:["./basic.h"]
  > EOF

  $ cat > basic.h <<EOF
  > void f_nat(intnat);
  > void f_int(int);
  > void f_double(double);
  > void f_int32(int32_t);
  > void f_int64(int64_t);
  > void f_nativeint(intnat);
  > EOF

  $ cat basic_stub.c
  #include <caml/mlvalues.h>
  #include <caml/memory.h>
  #include <caml/alloc.h>
  #include <caml/custom.h>
  #include "./basic.h"
  typedef intnat camlid_int;
  static void camlid_ml2c(value * v, camlid_int * c){ *c = Long_val(*v); }
  void f_nat(camlid_int);
  static void camlid_c2ml(value * v, camlid_int * c){ *v = Val_long(*c); }
  static void camlid_free(camlid_int * c){  }
  extern value camlid_stub_f_nat(value x){
    camlid_int x1 = ((camlid_int) { });
    value ret;
    camlid_ml2c(&x, &x1);
    f_nat(x1);
    camlid_c2ml(&ret, &x1);
    camlid_free(&x1);
    return ret;
  };
  typedef int camlid_int1;
  static void camlid_ml2c1(value * v, camlid_int1 * c){ *c = Int_val(*v); }
  void f_int(camlid_int1);
  static void camlid_c2ml1(value * v, camlid_int1 * c){ *v = Val_int(*c); }
  static void camlid_free1(camlid_int1 * c){  }
  extern value camlid_stub_f_int(value x){
    camlid_int1 x1 = ((camlid_int1) { });
    value ret;
    camlid_ml2c1(&x, &x1);
    f_int(x1);
    camlid_c2ml1(&ret, &x1);
    camlid_free1(&x1);
    return ret;
  };
  typedef double camlid_float;
  static void camlid_ml2c2(value * v, camlid_float * c){ *c = Double_val(*v); }
  void f_double(camlid_float);
  static void camlid_c2ml2(value * v, camlid_float * c){
    *v = caml_copy_double(*c);
    }
  static void camlid_free2(camlid_float * c){  }
  extern value camlid_stub_f_double(value x){
    camlid_float x1 = ((camlid_float) { });
    value ret;
    camlid_ml2c2(&x, &x1);
    f_double(x1);
    camlid_c2ml2(&ret, &x1);
    camlid_free2(&x1);
    return ret;
  };
  typedef int32_t camlid_int32;
  static void camlid_ml2c3(value * v, camlid_int32 * c){ *c = Int32_val(*v); }
  void f_int32(camlid_int32);
  static void camlid_c2ml3(value * v, camlid_int32 * c){
    *v = caml_copy_int32(*c);
    }
  static void camlid_free3(camlid_int32 * c){  }
  extern value camlid_stub_f_int32(value x){
    camlid_int32 x1 = ((camlid_int32) { });
    value ret;
    camlid_ml2c3(&x, &x1);
    f_int32(x1);
    camlid_c2ml3(&ret, &x1);
    camlid_free3(&x1);
    return ret;
  };
  typedef int64_t camlid_int64;
  static void camlid_ml2c4(value * v, camlid_int64 * c){ *c = Int64_val(*v); }
  void f_int64(camlid_int64);
  static void camlid_c2ml4(value * v, camlid_int64 * c){
    *v = caml_copy_int64(*c);
    }
  static void camlid_free4(camlid_int64 * c){  }
  extern value camlid_stub_f_int64(value x){
    camlid_int64 x1 = ((camlid_int64) { });
    value ret;
    camlid_ml2c4(&x, &x1);
    f_int64(x1);
    camlid_c2ml4(&ret, &x1);
    camlid_free4(&x1);
    return ret;
  };
  typedef intnat camlid_nativeint;
  static void camlid_ml2c5(value * v, camlid_nativeint * c){
    *c = Nativeint_val(*v);
    }
  void f_nativeint(camlid_nativeint);
  static void camlid_c2ml5(value * v, camlid_nativeint * c){
    *v = caml_copy_nativeint(*c);
    }
  static void camlid_free5(camlid_nativeint * c){  }
  extern value camlid_stub_f_nativeint(value x){
    camlid_nativeint x1 = ((camlid_nativeint) { });
    value ret;
    camlid_ml2c5(&x, &x1);
    f_nativeint(x1);
    camlid_c2ml5(&ret, &x1);
    camlid_free5(&x1);
    return ret;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c

  $ cat basic.ml
  type camlid_int = int
  type camlid_int1 = int
  type camlid_float = float
  type camlid_int32 = int32
  type camlid_int64 = int64
  type camlid_nativeint = nativeint
  external f_nat: camlid_int -> camlid_int = "camlid_stub_f_nat"
  external f_int: camlid_int1 -> camlid_int1 = "camlid_stub_f_int"
  external f_double: camlid_float -> camlid_float = "camlid_stub_f_double"
  external f_int32: camlid_int32 -> camlid_int32 = "camlid_stub_f_int32"
  external f_int64: camlid_int64 -> camlid_int64 = "camlid_stub_f_int64"
  external f_nativeint:
    camlid_nativeint ->
    camlid_nativeint
    = "camlid_stub_f_nativeint"

  $ ocamlc -c basic.ml

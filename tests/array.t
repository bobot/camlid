  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Expr
  > open Helper
  > 
  > let (a_len,a,len) = array int
  > let io_a_len = inout ~used_in_call:false a_len "x"
  > let () = Generate.to_file "basic" [
  >  func "f" [ 
  >  io_a_len;
  >  ignored a "a" ~binds:[a_len.c,expr "&%a" pp_var io_a_len.pc];
  >  ignored len "l" ~binds:[a_len.c,expr "&%a" pp_var io_a_len.pc];
  > ]
  > ]
  > EOF

  $ cat basic_stub.c
  #include <caml/mlvalues.h>
  #include <caml/memory.h>
  #include <caml/alloc.h>
  #include <caml/custom.h>
  typedef int camlid_int15;
  struct camlid_array_s14 { camlid_int15* t; size_t len; };
  typedef struct camlid_array_s14 camlid_array3;
  typedef camlid_int15* camlid_array4;
  typedef size_t* camlid_length_array5;
  static void camlid_ml2c16(value * v7, camlid_int15 * c8){
    *c8 = Int_val(*v7);
    }
  static void camlid_ml2c6(value * v5, camlid_array3 * c6){
    CAMLparam0 ();
    CAMLlocal1(cid_temp);
    c6->len = Wosize_val(*v5);
    c6->t = malloc(sizeof(camlid_int15)*c6->len);
    for(size_t cid_i=0; cid_i < c6->len; cid_i++){
      cid_temp=Field(*v5,cid_i);
      camlid_ml2c16(&cid_temp, &c6->t[cid_i]);
      }
    CAMLreturn0;
    }
  static void camlid_init7(camlid_array3 * c6, camlid_array4 * c9){
    *c9 = c6->t;
    }
  static void camlid_init8(camlid_array3 * c6, camlid_length_array5 * c10){
    *c10 = &c6->len;
    }
  void f(camlid_array4, camlid_length_array5);
  static void camlid_c2ml17(value * v7, camlid_int15 * c8){
    *v7 = Val_int(*c8);
    }
  static void camlid_c2ml10(value * v5, camlid_array3 * c6){
    CAMLparam0 ();
    CAMLlocal1(cid_temp);
    *v5=caml_alloc(c6->len,0);
    for(size_t cid_i=0; cid_i < c6->len; cid_i++){
      camlid_c2ml17(&cid_temp, &c6->t[cid_i]);
      Store_field(*v5,cid_i,cid_temp);
      }
    CAMLreturn0;
    }
  static void camlid_free11(camlid_array3 * c6){ free(c6->t); }
  static void camlid_free12(camlid_array4 * c9){  }
  static void camlid_free13(camlid_length_array5 * c10){  }
  extern value camlid_stub_f1(value x0){
    camlid_array3 x1 = ((camlid_array3) { });
    camlid_array4 a2 = 0;
    camlid_length_array5 l3 = 0;
    value ret4;
    camlid_ml2c6(&x0, &x1);
    camlid_init7(&x1, &a2);
    camlid_init8(&x1, &l3);
    f(a2, l3);
    camlid_c2ml10(&ret4, &x1);
    camlid_free11(&x1);
    camlid_free12(&a2);
    camlid_free13(&l3);
    return ret4;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c

  $ cat basic.ml
  type camlid_int2 = int
  type camlid_array0 = camlid_int2 array
  external f: camlid_array0 -> camlid_array0 = "camlid_stub_f1"

  $ ocamlc -c basic.ml

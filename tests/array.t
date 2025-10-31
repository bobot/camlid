  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Expr
  > open Helper
  > 
  > let (a_len,a,len) = output_array ~input:true "a" int
  > let () = Generate.to_file "basic" [
  >  func "f" [ a_len; a; len ]
  > ]
  > EOF

  $ cat basic_stub.c
  #include <caml/mlvalues.h>
  #include <caml/memory.h>
  #include <caml/alloc.h>
  #include <caml/custom.h>
  typedef int camlid_int;
  struct camlid_array_s { camlid_int* t; size_t len; };
  typedef struct camlid_array_s camlid_array;
  typedef camlid_int* camlid_array1;
  typedef size_t* camlid_length_array;
  static void camlid_ml2c1(value * v, camlid_int * c){ *c = Int_val(*v); }
  static void camlid_ml2c(value * v, camlid_array * c){
    CAMLparam0 ();
    CAMLlocal1(cid_temp);
    c->len = Wosize_val(*v);
    c->t = malloc(sizeof(camlid_int)*c->len);
    for(size_t cid_i=0; cid_i < c->len; cid_i++){
      cid_temp=Field(*v,cid_i);
      camlid_ml2c1(&cid_temp, &c->t[cid_i]);
      }
    CAMLreturn0;
    }
  static void camlid_init(camlid_array * c, camlid_array1 * c1){ *c1 = c->t; }
  static void camlid_init1(camlid_array * c, camlid_length_array * c1){
    *c1 = &c->len;
    }
  void f(camlid_array1, camlid_length_array);
  static void camlid_c2ml1(value * v, camlid_int * c){ *v = Val_int(*c); }
  static void camlid_c2ml(value * v, camlid_array * c){
    CAMLparam0 ();
    CAMLlocal1(cid_temp);
    *v=caml_alloc(c->len,0);
    for(size_t cid_i=0; cid_i < c->len; cid_i++){
      camlid_c2ml1(&cid_temp, &c->t[cid_i]);
      Store_field(*v,cid_i,cid_temp);
      }
    CAMLreturn0;
    }
  static void camlid_free(camlid_array * c){ free(c->t); }
  static void camlid_free1(camlid_array1 * c){  }
  static void camlid_free2(camlid_length_array * c){  }
  extern value camlid_stub_f(value a){
    camlid_array a1 = ((camlid_array) { });
    camlid_array1 a_a = 0;
    camlid_length_array a_len = 0;
    value ret;
    camlid_ml2c(&a, &a1);
    camlid_init(&a1, &a_a);
    camlid_init1(&a1, &a_len);
    f(a_a, a_len);
    camlid_c2ml(&ret, &a1);
    camlid_free(&a1);
    camlid_free1(&a_a);
    camlid_free2(&a_len);
    return ret;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c

  $ cat basic.ml
  type camlid_int = int
  type camlid_array = camlid_int array
  external f: camlid_array -> camlid_array = "camlid_stub_f"

  $ ocamlc -c basic.ml

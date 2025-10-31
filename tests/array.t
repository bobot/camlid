  $ ./camlid_toplevel.exe -stdin > basic.c <<EOF
  > open Camlid
  > open Expr
  > open Helper
  > 
  > let () = Generate.to_file "basic" [
  >  (let (a_len,a,len) = output_array ~input:true  "a" int in
  >   func "f" [ a_len; a; len ]);
  >  (let (a_len,a,len) = output_array ~input:false "a" int in
  >   func "f1" [ a_len; a; len ]);
  >  (let (a_len,a,len) = input_array ~output:true  "a" int in
  >   func "f2" [ a_len; a; len ]);
  >  (let (a_len,a,len) = input_array ~output:false "a" int in
  >   func "f3" [ a_len; a; len ]);
  >  (let (a_len,len) = output_set_length_array "a" int in
  >   func "f4" [ len; a_len])
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
  struct camlid_array_s1 { camlid_int* t; size_t len; };
  typedef struct camlid_array_s1 camlid_array2;
  typedef camlid_int* camlid_array3;
  typedef size_t* camlid_length_array1;
  static void camlid_init2(camlid_array2 * c){  }
  static void camlid_init3(camlid_array2 * c, camlid_array3 * c1){
    *c1 = c->t;
    }
  static void camlid_init4(camlid_array2 * c, camlid_length_array1 * c1){
    *c1 = &c->len;
    }
  void f1(camlid_array3, camlid_length_array1);
  static void camlid_c2ml2(value * v, camlid_array2 * c){
    CAMLparam0 ();
    CAMLlocal1(cid_temp);
    *v=caml_alloc(c->len,0);
    for(size_t cid_i=0; cid_i < c->len; cid_i++){
      camlid_c2ml1(&cid_temp, &c->t[cid_i]);
      Store_field(*v,cid_i,cid_temp);
      }
    CAMLreturn0;
    }
  static void camlid_free3(camlid_array2 * c){ free(c->t); }
  static void camlid_free4(camlid_array3 * c){  }
  static void camlid_free5(camlid_length_array1 * c){  }
  extern value camlid_stub_f1(){
    camlid_array2 a = ((camlid_array2) { });
    camlid_array3 a_a = 0;
    camlid_length_array1 a_len = 0;
    value ret;
    camlid_init2(&a);
    camlid_init3(&a, &a_a);
    camlid_init4(&a, &a_len);
    f1(a_a, a_len);
    camlid_c2ml2(&ret, &a);
    camlid_free3(&a);
    camlid_free4(&a_a);
    camlid_free5(&a_len);
    return ret;
  };
  struct camlid_array_s2 { camlid_int* t; size_t len; };
  typedef struct camlid_array_s2 camlid_array4;
  typedef camlid_int* camlid_array5;
  typedef size_t camlid_length_array2;
  static void camlid_ml2c2(value * v, camlid_array4 * c){
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
  static void camlid_init5(camlid_array4 * c, camlid_array5 * c1){
    *c1 = c->t;
    }
  static void camlid_init6(camlid_array4 * c, camlid_length_array2 * c1){
    *c1 = c->len;
    }
  void f2(camlid_array5, camlid_length_array2);
  static void camlid_c2ml3(value * v, camlid_array4 * c){
    CAMLparam0 ();
    CAMLlocal1(cid_temp);
    *v=caml_alloc(c->len,0);
    for(size_t cid_i=0; cid_i < c->len; cid_i++){
      camlid_c2ml1(&cid_temp, &c->t[cid_i]);
      Store_field(*v,cid_i,cid_temp);
      }
    CAMLreturn0;
    }
  static void camlid_free6(camlid_array4 * c){ free(c->t); }
  static void camlid_free7(camlid_array5 * c){  }
  static void camlid_free8(camlid_length_array2 * c){  }
  extern value camlid_stub_f2(value a){
    camlid_array4 a1 = ((camlid_array4) { });
    camlid_array5 a_a = 0;
    camlid_length_array2 a_len = 0;
    value ret;
    camlid_ml2c2(&a, &a1);
    camlid_init5(&a1, &a_a);
    camlid_init6(&a1, &a_len);
    f2(a_a, a_len);
    camlid_c2ml3(&ret, &a1);
    camlid_free6(&a1);
    camlid_free7(&a_a);
    camlid_free8(&a_len);
    return ret;
  };
  struct camlid_array_s3 { camlid_int* t; size_t len; };
  typedef struct camlid_array_s3 camlid_array6;
  typedef camlid_int* camlid_array7;
  typedef size_t camlid_length_array3;
  static void camlid_ml2c3(value * v, camlid_array6 * c){
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
  static void camlid_init7(camlid_array6 * c, camlid_array7 * c1){
    *c1 = c->t;
    }
  static void camlid_init8(camlid_array6 * c, camlid_length_array3 * c1){
    *c1 = c->len;
    }
  void f3(camlid_array7, camlid_length_array3);
  static void camlid_free9(camlid_array6 * c){ free(c->t); }
  static void camlid_free10(camlid_array7 * c){  }
  static void camlid_free11(camlid_length_array3 * c){  }
  extern value camlid_stub_f3(value a){
    camlid_array6 a1 = ((camlid_array6) { });
    camlid_array7 a_a = 0;
    camlid_length_array3 a_len = 0;
    value ret;
    camlid_ml2c3(&a, &a1);
    camlid_init7(&a1, &a_a);
    camlid_init8(&a1, &a_len);
    f3(a_a, a_len);
    ret = Val_unit;
    camlid_free9(&a1);
    camlid_free10(&a_a);
    camlid_free11(&a_len);
    return ret;
  };
  typedef camlid_int* camlid_array8;
  static void camlid_init9(camlid_array8 * c){  }
  void f4(camlid_array8);
  static void camlid_c2ml4(camlid_int a_len, value * v, camlid_array8 * c){
    CAMLparam0 ();
    CAMLlocal1(cid_temp);
    *v=caml_alloc(a_len,0);
    for(size_t cid_i=0; cid_i < a_len; cid_i++){
      camlid_c2ml1(&cid_temp, &((*c)[cid_i]));
      Store_field(*v,cid_i,cid_temp);
      }
    CAMLreturn0;
    }
  static void camlid_free12(camlid_int * c){  }
  static void camlid_free13(camlid_array8 * c){ free(*c); }
  extern value camlid_stub_f4(value a_len){
    camlid_int a_len1 = 0;
    camlid_array8 a = ((camlid_array8) { });
    value ret;
    camlid_ml2c1(&a_len, &a_len1);
    camlid_init9(&a);
    f4(a);
    camlid_c2ml4(a_len1, &ret, &a);
    camlid_free12(&a_len1);
    camlid_free13(&a);
    return ret;
  };

  $ ocamlc -ccopt --warn-all -c basic_stub.c

  $ cat basic.ml
  type camlid_int = int
  type camlid_array = camlid_int array
  type camlid_array1 = camlid_int array
  type camlid_array2 = camlid_int array
  type camlid_array3 = camlid_int array
  type camlid_array4 = camlid_int array
  external f: camlid_array -> camlid_array = "camlid_stub_f"
  external f1: unit -> camlid_array1 = "camlid_stub_f1"
  external f2: camlid_array2 -> camlid_array2 = "camlid_stub_f2"
  external f3: camlid_array3 -> unit = "camlid_stub_f3"
  external f4: camlid_int -> camlid_array4 = "camlid_stub_f4"

  $ ocamlc -c basic.ml

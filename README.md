## camlid

 camlid is currently a POC of a C binding generator for OCaml.
 
 The description is similar to the extension of IDL found in CamlIDL, but it is a DSL in OCaml and it removes the interface language part of IDL. Moreover it tries to keep a core simple and generic and uses predefined helpers for common types and patterns.

 C11 is required.

 It is currently a POC without documentation, examples can be found in the [tests](./tests) directory.

 ### Example

Suppose the following header (`alib.h`):

```c
void f_input(int);
void f_output(int *);
int f_with_res();
void f_no_arg_no_result();
```

The following OCaml program produces a `basic_stub.c` and `basic.ml` file.

 ```ocaml
open Camlid
open Helper

 let () = Generate.to_file "basic"
  ~headers:["alib.h"]
  [
    func "f_input" [ input int "x"];
    func "f_output" [ output (ptr_ref int) "x"];
    func "f_with_res" [] ~result:int;
    func "f_no_arg_no_result" [];
  ]
 ```

 Then one just need to write `basic.mli` with the documentation:

 ```ocaml
 (** {2 Alib API} *)

 (** set important global information *)
 val f_input: int -> unit

 (** get important global information *)
 val f_output: unit -> int

 (** get another very important global information *)
 val f_with_res: unit -> int

 (** do something really important *)
 val f_no_arg_no_result: unit -> unit
 ```

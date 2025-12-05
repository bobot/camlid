## camlid

 Camlid is a C binding generator for OCaml. Its interface is not yet stable.
 
 The description is similar to the extension of IDL found in CamlIDL, but it is a DSL in OCaml and it removes the interface language part of IDL. Moreover it tries to keep a core simple and generic and uses predefined helpers for common types and patterns. It  supports automatic unboxing, untagging when possible. The ownership of the allocated memory after the call to a stubbed C function can be specified.

 C11 is required.

 [Documentation](https://bobot.github.io/camlid/camlid/index.html)

 ### Quick Example

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
    func "f_input" [ input int_trunc];
    func "f_output" [ output (ptr_ref int_trunc)];
    func "f_with_res" [] ~result:int_trunc;
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

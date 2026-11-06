(* Each scenario is a stub written as low-level operations: first the classic
   bug, then the correct stub. The model must refuse the first and accept the
   second. *)
open Camlid.Blocks

let scenarios =
  [
    (* A value kept in an unregistered variable across an allocation, while its
       block is still reachable from a root: the block is alive but it moved. *)
    ( "unregistered value, block still reachable",
      run ~slots:[ "x"; "y" ] ~roots:[ "y" ]
        ~init:[ ("x", [ "b" ]); ("y", [ "b" ]) ]
        ~blocks:[ ("b", Ocaml, []) ]
        [ Gc; Use [ "x" ]; Leave None ],
      run ~slots:[ "x"; "y" ] ~roots:[ "x"; "y" ]
        ~init:[ ("x", [ "b" ]); ("y", [ "b" ]) ]
        ~blocks:[ ("b", Ocaml, []) ]
        [ Gc; Use [ "x" ]; Leave None ] );
    (* Two custom outputs: the first one must survive the second allocation *)
    ( "two allocations, first output unregistered",
      run ~slots:[ "o1"; "o2" ]
        [
          New ("m1", Ocaml, []);
          Set ("o1", [ "m1" ]);
          Gc;
          New ("m2", Ocaml, []);
          Set ("o2", [ "m2" ]);
          Use [ "o1"; "o2" ];
          Leave None;
        ],
      run ~slots:[ "o1"; "o2" ] ~roots:[ "o1"; "o2" ]
        ~init:[ ("o1", []); ("o2", []) ]
        [
          New ("m1", Ocaml, []);
          Set ("o1", [ "m1" ]);
          Gc;
          New ("m2", Ocaml, []);
          Set ("o2", [ "m2" ]);
          Use [ "o1"; "o2" ];
          Leave None;
        ] );
    (* A value stored in a C array (the old Unrooted_heap) *)
    ( "value in C memory across an allocation",
      run ~slots:[ "in"; "arr" ] ~roots:[ "in" ]
        ~init:[ ("in", [ "ml" ]); ("arr", [ "cells" ]) ]
        ~blocks:[ ("ml", Ocaml, []); ("cells", Stub, [ "ml" ]) ]
        [ Gc; Use [ "arr" ]; Free "cells"; Leave None ],
      run ~slots:[ "in"; "arr" ] ~roots:[ "in" ]
        ~init:[ ("in", [ "ml" ]); ("arr", [ "cells" ]) ]
        ~blocks:[ ("ml", Ocaml, []); ("cells", Stub, [ "ml" ]) ]
        [ Use [ "arr" ]; Free "cells"; Gc; Leave None ] );
    (* A window: a custom block mentioning the data of its parent. Without the
       parent in the result, the parent's finalizer frees the data. *)
    ( "window without its parent",
      run ~slots:[ "p"; "w" ] ~roots:[ "p"; "w" ]
        ~init:[ ("p", [ "pm" ]); ("w", []) ]
        ~blocks:[ ("pm", Ocaml, [ "data" ]); ("data", Of [ "pm" ], []) ]
        [
          Gc;
          New ("wm", Ocaml, [ "data" ]);
          Set ("w", [ "wm" ]);
          Leave (Some "w");
        ],
      run ~slots:[ "p"; "w"; "t" ] ~roots:[ "p"; "w"; "t" ]
        ~init:[ ("p", [ "pm" ]); ("w", []); ("t", []) ]
        ~blocks:[ ("pm", Ocaml, [ "data" ]); ("data", Of [ "pm" ], []) ]
        [
          New ("wm", Ocaml, [ "data" ]);
          Set ("w", [ "wm" ]);
          Gc;
          New ("tuple", Ocaml, [ "wm"; "pm" ]);
          Set ("t", [ "tuple" ]);
          Leave (Some "t");
        ] );
    (* A child reference counts its parent: the count is shared *)
    ( "child freed by reference count, parent dropped",
      run ~slots:[ "p"; "c" ] ~roots:[ "p"; "c" ]
        ~init:[ ("p", [ "pm" ]); ("c", []) ]
        ~blocks:[ ("pm", Ocaml, [ "rc" ]); ("rc", Of [ "pm" ], []) ]
        [ New ("cm", Ocaml, [ "rc" ]); Set ("c", [ "cm" ]); Leave (Some "c") ],
      run ~slots:[ "p"; "c" ] ~roots:[ "p"; "c" ]
        ~init:[ ("p", [ "pm" ]); ("c", []) ]
        ~blocks:[ ("pm", Ocaml, [ "rc" ]); ("rc", Stub, []) ]
        [
          New ("cm", Ocaml, [ "rc" ]);
          Give ("rc", Of [ "pm"; "cm" ]);
          Set ("c", [ "cm" ]);
          Leave (Some "c");
        ] );
    (* caml_alloc_small: the fields are not initialized *)
    ( "allocation before filling a new block",
      run ~slots:[ "t" ] ~roots:[ "t" ]
        ~init:[ ("t", []) ]
        [
          New_uninit ("tuple", Ocaml);
          Set ("t", [ "tuple" ]);
          Gc;
          Fill ("tuple", []);
          Leave None;
        ],
      run ~slots:[ "t" ] ~roots:[ "t" ]
        ~init:[ ("t", []) ]
        [
          New_uninit ("tuple", Ocaml);
          Set ("t", [ "tuple" ]);
          Fill ("tuple", []);
          Gc;
          Leave None;
        ] );
    (* A conversion that raises after a malloc *)
    ( "raising while owning a copy",
      run ~slots:[ "c" ]
        [
          New ("copy", Stub, []);
          Set ("c", [ "copy" ]);
          May_raise;
          Free "copy";
          Leave None;
        ],
      run ~slots:[ "c" ]
        [
          May_raise;
          New ("copy", Stub, []);
          Set ("c", [ "copy" ]);
          Free "copy";
          Leave None;
        ] );
    ( "leak",
      run ~slots:[] [ New ("copy", Stub, []); Leave None ],
      run ~slots:[] [ New ("copy", Stub, []); Free "copy"; Leave None ] );
    ( "double free",
      run ~slots:[]
        [ New ("copy", Stub, []); Free "copy"; Free "copy"; Leave None ],
      run ~slots:[] [ New ("copy", Stub, []); Free "copy"; Leave None ] );
    ( "moved twice into OCaml",
      run ~slots:[]
        [
          New ("data", Stub, []);
          New ("m1", Ocaml, [ "data" ]);
          Give ("data", Of [ "m1" ]);
          New ("m2", Ocaml, [ "data" ]);
          Give ("data", Of [ "m2" ]);
          Leave None;
        ],
      run ~slots:[]
        [
          New ("data", Stub, []);
          New ("m1", Ocaml, [ "data" ]);
          Give ("data", Of [ "m1" ]);
          Leave None;
        ] );
    (* An interior pointer (arb_midref) read after its parent's copy is freed *)
    ( "interior pointer after free",
      run ~slots:[ "x"; "mid" ]
        ~init:[ ("x", [ "arb" ]) ]
        ~blocks:[ ("arb", Stub, []) ]
        [ Set ("mid", [ "arb" ]); Free "arb"; Use [ "mid" ]; Leave None ],
      run ~slots:[ "x"; "mid" ]
        ~init:[ ("x", [ "arb" ]) ]
        ~blocks:[ ("arb", Stub, []) ]
        [ Set ("mid", [ "arb" ]); Use [ "mid" ]; Free "arb"; Leave None ] );
    (* The lock has no rule of its own: the C function reads a copy of its
       argument while a GC runs. A pointer to the OCaml heap fails, a borrowed C pointer passes. * *)
    ( "lock released on the OCaml heap",
      run ~slots:[ "v"; "arg" ] ~roots:[ "v" ]
        ~init:[ ("v", [ "ml" ]) ]
        ~blocks:[ ("ml", Ocaml, []) ]
        [ Copy ("arg", "v"); Gc; Use [ "arg" ]; Leave None ],
      run ~slots:[ "v"; "c"; "arg" ] ~roots:[ "v" ]
        ~init:[ ("v", [ "ml" ]); ("c", [ "data" ]) ]
        ~blocks:[ ("ml", Ocaml, [ "data" ]); ("data", Of [ "ml" ], []) ]
        [ Copy ("arg", "c"); Gc; Use [ "arg" ]; Leave None ] );
    (* The same call when v is the only root that keeps the data alive *)
    ( "lock released, data's owner not registered",
      run ~slots:[ "v"; "c"; "arg" ]
        ~init:[ ("v", [ "ml" ]); ("c", [ "data" ]) ]
        ~blocks:[ ("ml", Ocaml, [ "data" ]); ("data", Of [ "ml" ], []) ]
        [ Copy ("arg", "c"); Gc; Use [ "arg" ]; Leave None ],
      run ~slots:[ "v"; "c"; "arg" ] ~roots:[ "v" ]
        ~init:[ ("v", [ "ml" ]); ("c", [ "data" ]) ]
        ~blocks:[ ("ml", Ocaml, [ "data" ]); ("data", Of [ "ml" ], []) ]
        [ Copy ("arg", "c"); Gc; Use [ "arg" ]; Leave None ] );
    ( "noalloc stub that allocates",
      run ~noalloc:true ~slots:[] [ Gc; Leave None ],
      run ~noalloc:true ~slots:[] [ Leave None ] );
    ( "uninitialized variable",
      run ~slots:[ "c" ] [ Use [ "c" ]; Leave None ],
      run ~slots:[ "c" ] [ Set ("c", []); Use [ "c" ]; Leave None ] );
  ]

(* The C function allocates an output_value: the stub can't be noalloc. It was
   declared noalloc, which corrupted the heap (FMPZ.to_z in ocaml-flint). *)
let () =
  match
    Camlid.Generate.to_file "noalloc_output_value"
      [ Camlid.Helper.(func ~noalloc:true "f" [ output_value "t" ]) ]
  with
  | () ->
      print_endline "output_value accepted as noalloc";
      exit 1
  | exception Invalid_argument _ -> ()

let () =
  let failed = ref false in
  let verbose = Array.length Sys.argv > 1 in
  List.iter
    (fun (name, bad, good) ->
      (match (bad, good) with Error _, Ok () -> () | _ -> failed := true);
      if verbose || !failed then
        Printf.printf "%-48s %s / %s\n" name
          (match bad with Error e -> "refused: " ^ e | Ok () -> "NOT REFUSED")
          (match good with
          | Ok () -> "fixed: ok"
          | Error e -> "FIXED REFUSED: " ^ e))
    scenarios;
  if !failed then exit 1

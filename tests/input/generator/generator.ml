open Camlid
open Helper

let () =
  Generate.to_file "mylib"
    [
      func "f" [ input int_trunc ];
      func "f7"
        [
          input int_trunc;
          input int_trunc;
          input int_trunc;
          input int_trunc;
          input int_trunc;
          input int_trunc;
          input int_trunc;
        ];
    ]
    ~headers:[ "./lib.h" ]

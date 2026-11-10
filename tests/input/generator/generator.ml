open Camlid
open Helper

let () =
  Generate.to_file "mylib"
    [
      func "f" [ input c_int ];
      func "f7"
        [
          input c_int;
          input c_int;
          input c_int;
          input c_int;
          input c_int;
          input c_int;
          input c_int;
        ];
    ]
    ~headers:[ "./lib.h" ]

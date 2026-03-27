
let () =
  let module Cminus = Cminus.Core in
  let module Tests = 
    Tests_base.Make
      (struct include Cminus let tests_dir = "suites/core" end)
  in

  Tests.run_tests ()


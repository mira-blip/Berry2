
let () =
  let module Cminus = Cminus.Challenge in
  let module Tests = 
    Tests_base.Make
      (struct include Cminus let tests_dir = "suites/challenge" end)
  in

  Tests.run_tests ()


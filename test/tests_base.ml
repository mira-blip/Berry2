(* Testing framework for C-.
 *
 * N. Danner
 *
 * Tests are defined by test specifications in C- source code files.  Each
 * source code file must have a block comment of the form
 *
 *   /*!tests!
 *    *  spec 1
 *    *  spec 2
 *    *  ...
 *    */
 * I.e., the opening comment line must be exactly as above, then followed by
 * a sequence of test specifications, followed by the close comment.  After
 * the first line, the leading characters are defined to be any combination
 * of whitespace and "*" characters, and the closing line must have nothing
 * but whitespace before "*/".  When the leading characters are stripped
 * from the specification lines, the result must be a valid sequence of JSON
 * objects.
 *
 * A test specification is a JSON object with the following attributes:
 *
 *   - "input":  a string list of values to provide as input.
 *   - "output": a string list of expected output values.
 *   - "exception": an exception that is expected to be raised.
 *
 * There must be exactly one `input` attribute and either an `output` or
 * `exception` attribute.
 *
 * The test is conducted as follows.  The source code file is executed by the
 * interpreter.  Each `get_*` function is given the next item in the input
 * list.  
 *
 * If there is an `output` attribute, then the sequence of `print_*` outputs
 * is compared to it.  If they are the same, the test passes; otherwise the
 * test fails.  So, for example, consider
 *
 *   {
 *       "input":    [ "84", "60" ],
 *       "output":   [ "12" ]
 *   }
 *
 * This test will the program.  The first call to `get_*` will get "84"
 * and the second call will get "60".  The test passes if the program outputs a
 * single line that consists of "12", fails otherwise.
 *
 * If there is an `exception` attribute, then the test passes if the program
 * execution raises the given exception, and fails otherwise.  The output of
 * calls to `print_*` is ignored.
 *
 * If there is an `output` and an `exception` attribute, the test is as if
 * there were just an `exception` attribute (i.e., output is ignored).
 * Later versions of this testing framework could require that the program
 * output match the `output` attribute, then raise the indicated exception.
 *
 * Individual tests within a suite are named 0, 1, 2,... according to their
 * index in the specification file.
 *
 * Tests are grouped by teams.  Each team has a directory under
 * `interp_tests_dir` where they should locate their test files (not in
 * subdirectories).
 *
 *)

module Make (Cminus : sig include Cminus.S val tests_dir : string end) = struct

  module YJ = Yojson.Basic
  module YJU = YJ.Util

  (* Raised when there is an error extracting a specification from a test
   * file.
   *)
  exception BadSpec of string

  let split (c : char) (s : string) : string list =
    match String.split_on_char c s with
    | [""] -> []
    | ss -> ss

  (* iotest test_code input expected = a test that executes `test_code` with
   * input provided by `input`.  The test passes if the output matches
   * `expected`, and fails otherwise.
   *)
  let iotest test_file test_code input expected () : Alcotest.return =
    Sys.set_signal Sys.sigalrm (
      Sys.Signal_handle (
        fun _ -> Alcotest.fail "Timeout!"
      )
    ) ;
    ignore (Unix.alarm 5) ;
    Alcotest.(check (list string))
      test_file
      expected
      (
        let () = 
          Cminus.Interp.Io.in_channel := 
            Scanf.Scanning.from_string (String.concat " " input) in
        let outbuf = Buffer.create 80 in
        let () = Cminus.Interp.Io.output := Buffer.add_string outbuf in
        let () = Cminus.Interp.exec test_code in
        Buffer.contents outbuf |> String.trim |> split '\n'
      )

  (* extest test_code input expected = a test that succeeds when executing
   * `test_code` with `input` raises an exception e such that
   * Printexc_to_string e = `expected`.
   *
   * We don't use assert_raise here, because that expects an exception value,
   * which requires us to know the arguments to the constructor when the
   * exception is raised, which is not something we can rely upon.  So instead
   * we compare to the string representation of the exception, which we expect
   * to be fixed by an appropriate call to `Printexc.register_printer`.
   *)
  let extest test_file test_code input expected () : Alcotest.return =
    Sys.set_signal Sys.sigalrm (
      Sys.Signal_handle (
        fun _ -> Alcotest.fail "Timeout!"
      )
    ) ;
    ignore (Unix.alarm 5) ;
    Alcotest.(check (option string))
      test_file
      (Some expected)
      (
        let () = 
          Cminus.Interp.Io.in_channel := 
            Scanf.Scanning.from_string (String.concat " " input) in
        let () = Cminus.Interp.Io.output := fun _ -> () in

        try
          let _ = Cminus.Interp.exec test_code in
          None
        with
        | e -> Some (Printexc.to_string e)
      )

  (* make_test_from_spec fname spec = tf, where `tf` is a test function
   * corresponding to the test defined by `spec` in the file `fname`.
   *)
  let make_test_from_spec (test_file : string) (spec : YJ.t) : unit -> Alcotest.return =

    (* test_code = the program parsed from `fname`.
     *)
    let test_code = In_channel.with_open_text test_file (
      fun ic ->
        let lexbuf = Lexing.from_channel ic in
        try
          Cminus.Parser.terminated_pgm Cminus.Lexer.read_token lexbuf
        with
        | Cminus.Parser.Error ->
          let pos = Lexing.lexeme_start_p lexbuf in
          failwith @@ Printf.sprintf
            ("Parser error in %s near line %d, character %d.\n")
            test_file
            pos.pos_lnum
            (pos.pos_cnum - pos.pos_bol)

    ) in

    (* Set the API input channel to read from the input in the test
     * specification.
     *)
    let input : string list = 
      spec |> YJU.member "input" |> YJU.to_list |> YJU.filter_string in

    (* Are we testing against expected output or an exception?
     *)
    let keys : string list = YJU.keys spec in

    if List.exists (fun k -> k = "output") keys then
      (* Get the expected output.
       *)
      let expected : string list =
        spec |> YJU.member "output" |> YJU.to_list |> YJU.filter_string in

      iotest test_file test_code input expected
    else if List.exists (fun k -> k = "exception") keys then
      let ex : string =
        spec |> YJU.member "exception" |> YJU.to_string in
      extest test_file test_code input ex
    else
      raise @@ BadSpec "No output or exception attribute"


  (*  is_dir f = true,  f is the name of a directory
   *             false, o/w.
   *)
  let is_dir (f : string) : bool =
    match Unix.stat f with
    | {st_kind = S_DIR; _} -> true
    | _ -> false

  (* tests_from_file f = ts, where ts is a test suite with name `f`, where the
   * tests are specified in a /*!tests! */ comment at the beginning of `f`.
   *)
  let tests_from_file (test_file : string) : unit Alcotest.test_case list =

    (* read_test_specs = ts, where ts is a list of JSON test specs read from
     * `test_file`.
     *)
    let read_test_specs () : YJ.t list =
      let inch : In_channel.t = In_channel.open_text test_file in

      let spec_start : string = "/*!tests!" in
      let spec_leader_regexp : Str.regexp = Str.regexp {|^\([ \t]\|\*\)*|} in

      (* Read from `inch` until we find the start of the specifications.
       * The start is indicated by a line that begins with `spec_start` and
       * `inch` will be positioned at the line following the first line that
       * starts with `spec_start`.
       *)
      let rec find_spec_start () =
        match In_channel.input_line inch with
        | None ->
          raise @@ BadSpec "No specs found"
        | Some s ->
          if String.starts_with ~prefix:spec_start s then ()
          else find_spec_start ()
      in

      (* read_specs () = the list of lines that are test specifications.
       *)
      let rec read_specs () : string list =
        match In_channel.input_line inch with
        | None -> raise @@ BadSpec "Unterminated spec comment"
        | Some s ->
          if String.trim s = "*/" then []
          else if not (Str.string_match spec_leader_regexp s 0)
               then raise @@ BadSpec ("Bad spec line: " ^ s)
          else Str.replace_first spec_leader_regexp "" s :: read_specs ()
      in

      try
        find_spec_start() ;
        read_specs () |> String.concat "\n" |> YJ.seq_from_string |> List.of_seq 
      with
      | BadSpec msg -> 
        Printf.eprintf "Bad test spec in %s: %s\n" test_file msg ; []

    in

      try
        (* specs = the test specifications.
         *)
        let specs = read_test_specs() in

        List.mapi
          (
            fun n s ->
              try
                Alcotest.test_case
                  (Int.to_string n)
                  `Quick
                  (make_test_from_spec test_file s)
              with
              | BadSpec msg ->
                raise @@ BadSpec (
                  Printf.sprintf "%s(%d): %s" test_file n msg
                )
          )
          specs

      with
      | Yojson.Json_error s ->
        raise @@ BadSpec (
          Printf.sprintf "%s: JSON: %s" test_file s
        )
      | Yojson.Basic.Util.Type_error (s, _) -> 
        raise @@ BadSpec (
          Printf.sprintf "%s: JSON type error: %s" test_file s
        )

  let run_tests () =
    try

      (* Define the strings by which to identify exceptions that are raised.
       *)
      Printexc.register_printer (
        function
        | Cminus.Interp.NoReturn _ -> Some "NoReturn"
        | Cminus.Interp.MultipleDeclaration _ -> Some "MultipleDeclaration"
        | Cminus.Interp.UnboundVariable _ -> Some "UnboundVariable"
        | Cminus.Interp.UndefinedFunction _ -> Some "UndefinedFunction"
        | Cminus.Interp.TypeError _ -> Some "TypeError"
        | Cminus.Interp.OutOfMemoryError -> Some "OutOfMemoryError"
        | Cminus.Interp.SegmentationError _ -> Some "SegmentationError"
        | _ -> None
      ) ;

      (* test_file suite_dir = the list of files with suffix `.c`.
       *
       * We sort in reverse order because the tests seem to be run in the
       * reverse order from this list, so this way the tests are executed in
       * alphabetical order.
       *)
      let test_files (suite_dir : string) : string list =
        List.filter
          (fun f -> Filename.check_suffix f "c")
          (Sys.readdir suite_dir |> Array.to_list)
        |> List.sort Stdlib.compare
      in

      (* suite_dirs = directories that contain test files.
       *)
      let suite_dirs : string Array.t =
        Sys.readdir Cminus.tests_dir 
        |> Array.to_list 
        |> List.map (Filename.concat Cminus.tests_dir)
        |> List.filter is_dir 
        |> List.sort Stdlib.compare
        |> List.to_seq
        |> Array.of_seq
      in

      for i = 0 to Array.length suite_dirs - 1 do
        let suite_dir = suite_dirs.(i) in
        print_endline "========================================" ;
        begin
          try
            Alcotest.run
              ~and_exit:false
              ~show_errors:true
              suite_dir
              (
                List.map (
                  fun test_file ->
                    (
                      test_file,
                      tests_from_file (Filename.concat suite_dir test_file)
                    )
                ) (test_files suite_dir)
              )
          with
          | Alcotest.Test_error -> ()
        end ;
        print_endline ""
      done

    with
    | BadSpec msg ->
      Printf.eprintf "%s" msg

end

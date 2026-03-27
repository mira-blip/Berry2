(** COMP 324 Project 2:  implementation of an imperative language.
 *
 * `cminus` driver program.
 *
 * @author N. Danner
 *)

(* ****************************************
 * Usage
 * ****************************************
 *)

let usage = {|
  cminus impl parseexp e:  parse expresion e
  cminus impl parsestm s:  parse statement s
  cminus impl parsepgm f:  parse program in file f
  cminus impl exec f:  execute program f (path to file)

The `impl` argument must be either "core" or "challenge", to indicate which
language implementation to use.
|}

exception Usage_error of string

module Cli (Impl : Cminus.S) = struct

  open Impl

  exception Parse_error of string

  type parse_cmd_t = Expr | Stm | Pgm

  let parse_and_show (cmd : parse_cmd_t) (s : string) : unit =

      let parse_and_show' parser shower s =
        let lexbuf = Lexing.from_string s in
        try
          let exp = parser Lexer.read_token lexbuf in
          print_endline (shower exp)
        with
        | Parser.Error ->
          let pos = Lexing.lexeme_start_p lexbuf in
          raise @@ Parse_error (
            Printf.sprintf
              "Parser error near line %d, character %d.\n"
              pos.pos_lnum
              (pos.pos_cnum - pos.pos_bol)
          )
      in

      match cmd with
      | Expr -> parse_and_show' Parser.terminated_exp Ast.Expr.show s
      | Stm -> parse_and_show' Parser.terminated_stm Ast.Stm.show s
      | Pgm -> 
        In_channel.with_open_text s (fun inch ->
          parse_and_show' Parser.terminated_pgm Ast.Prog.show
          (In_channel.input_all inch)
        )

  let exec (s : string) : unit =
    In_channel.with_open_text s (fun inch ->
      let lexbuf = Lexing.from_channel inch in
      try
        let p = Parser.terminated_pgm Lexer.read_token lexbuf in
        Interp.exec p
      with
      | Parser.Error ->
        let pos = Lexing.lexeme_start_p lexbuf in
        raise @@ Parse_error (
          Printf.sprintf
            "Parser error near line %d, character %d.\n"
            pos.pos_lnum
            (pos.pos_cnum - pos.pos_bol)
        )
    )
end

let () =
  try
    let args = ref [] in
    Arg.parse [] (fun a -> args := a :: !args) usage ;

    let (mode, command, arg) =
      match List.rev !args with
      | [mode; command; arg] -> (mode, command, arg)
      | _ -> raise @@ Usage_error "wrong number of command line arguments"
    in

    let module Impl =
      (val
        try Cminus.choose_impl mode
        with
        | Invalid_argument _ ->
          raise @@ Usage_error (
            Printf.sprintf "bad implementation '%s'" mode
          )
      )
    in
    let module Cli = Cli(Impl) in

    let cmd =
      match command with
      | "parseexp" -> Cli.parse_and_show Expr
      | "parsestm" -> Cli.parse_and_show Stm
      | "parsepgm" -> Cli.parse_and_show Pgm
      | "exec" -> Cli.exec
      | _ -> raise @@ Usage_error "bad command"
    in

    try
      cmd arg
    with
    | Cli.Parse_error msg -> print_endline ("Parse error: " ^ msg)
    | Impl.Interp.MultipleDeclaration x ->
      Printf.printf
        "Error: variable '%s' multiply declared.\n"
        (Impl.Ast.Id.show x)
    | Impl.Interp.UnboundVariable x ->
      Printf.printf
        "Error: variable '%s' used by not declared.\n"
        (Impl.Ast.Id.show x)
    | Impl.Interp.UndefinedFunction f ->
      Printf.printf
        "Error: function '%s' called but not defined.\n"
        (Impl.Ast.Id.show f)
    | Impl.Interp.NoReturn f ->
      Printf.printf
        "Error: function '%s' terminated without executing return.\n"
        (Impl.Ast.Id.show f)

  with
  | Usage_error msg ->
    Printf.printf
      "Bad command line usage:  %s"
      msg ;
    print_endline "" ;
    print_endline usage


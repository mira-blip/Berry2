(* Abstract syntax for C-.
 *
 * N. Danner

 
 *)

(* A module for identifiers/variables.
 *)
module Id = struct
    type t = string
    [@@deriving show]

    let compare = String.compare
end

(* C- expressions.
 *)
module Expr= struct

  type unop =
    | Neg
    | Not
  [@@deriving show]

  type binop =
    | Plus
    | Minus
    | Times
    | Div
    | Mod
    | And
    | Or
    | Eq
    | Ne
    | Lt
    | Le
    | Gt
    | Ge
    [@@deriving show]

  type t =
      (* `x` parses to Var "x".
       *)
    | Var of Id.t
      (* `xs[e]` parses to Index(xs, e)
       *)
    | Index of Id.t*t
      (* `n` parses to Num n for numbers n.
       *)
    | Num of int
      (* `true` parses to Bool true, `false` parses to Bool false.
       *)
    | Bool of bool
      (* `s` parses to String s for strings s.
       *)
    | Str of string
      (* -e parses to Unop(Neg, e), !e parses to Unop(Not, e).
       *)
    | Unop of unop*t
      (* e <op> e' parses to Binop(<op>, e, e').
       *)
    | Binop of binop*t*t
      (* f(e_0,...,e_{n-1}) parses to Call(f, e_0,..., e_{n-1}).
       *)
    | Call of Id.t * t list
    [@@deriving show]
end

module Stm = struct
  type t =
      (* typ ..., x,... y = e,... parses to
       *   VarDec [...; (x, None);... (y, Some e);...]
       * for typ = int, bool, or char*.
       *)
    | VarDec of (Id.t * Expr.t option) list
      (* typ ..., xs[e],... parses to ArrayDec [...; (xs, e);... ].
       * for typ = int, bool, or char*.
       *)
    | ArrayDec of (Id.t * Expr.t) list
    | Fscanf of Id.t*string*Id.t
      (* `x = e;` parses to Assign(x, e).
       *)
    | Assign of Id.t*Expr.t
      (* `xs[e] = e'` parses to IndexAssign(xs, e, e').
       *)
    | IndexAssign of Id.t*Expr.t*Expr.t
      (* `e;` parses to Expr(e).
       *)
    | Expr of Expr.t
      (* {
       *     s_0
       *     s_1
       *     ...
       *     s_{n-1}
       * }
       * parses to Block [s_0;...; s_{n-1}].
       *)
    | Block of t list
      (* `if (e) s else s'` parses to If(e, s, s').
       * `if (e) s` parses to If(e, s, Block []).
       *)
    | IfElse of Expr.t*t*t
      (* `while e s` parses to While(e, s).
       *)
    | While of Expr.t*t
      (* `return` parses to Return None.
       * `return e` parses to Return (Some e).
       *)
    | Return of Expr.t option
    [@@deriving show]
end

module Prog = struct
  (* A function definition of the form
   *   typ f(typ_0 x_0,..., typ_{n-1} x_{n-1}) { ss }
   * parses to
   *   FunDef(f, [x_0;...; x_{n-1}], ss)
   *)
  type fundef = Id.t*Id.t list*Stm.t list
  [@@deriving show]

  (* A program that consists of the sequence of function definitions
   *   fdef_0
   *   fdef_1
   *   ...
   *   fdef_{n-1}
   * parses to
   *   Pgm [fdef_0;...; fdef_{n-1}]
   *)
  type t = Pgm of fundef list
  [@@deriving show]
end


(* C- interpreter.
 *
 * N. Danner

  * Team Berry: Ramon Ruiz, Dalton Soper, Tamiraa Sanjaajav 
 *)

module Ast = Core_ast

(* Raised when a function body terminates without executing `return`.
 *)
exception NoReturn of Ast.Id.t

(* MultipleDeclaration x is raised when x is declared more than once in a
 * block.
 *)
exception MultipleDeclaration of Ast.Id.t

(* UnboundVariable x is raised when x is used but not declared.
 *)
exception UnboundVariable of Ast.Id.t

(* UndefinedFunction f is raised when f is called but has not been defined.
 *)
exception UndefinedFunction of Ast.Id.t

(* TypeError s is raised when an operator or function is applied to operands
 * of the incorrect type.  s is any (hopefuly useful) message.
 *)
exception TypeError of string

(* OutOfMemoryError is raised when the an attempt is made to allocate more
 * space in the store than is available.
 *)
exception OutOfMemoryError

(* Raised when an attempt is made to access a store location that is
 * negative or larger than the store capacity.
 *)
exception SegmentationError of int


(* Values.
 *)
module Value = struct
  type t = 
    | V_Undefined
    | V_None
    | V_Int of int
    | V_Bool of bool
    | V_Str of string
    [@@deriving show]

  (* to_string v = a string representation of v (more human-readable than
   * `show`.
   *)
  let to_string (v : t) : string =
    match v with
    | V_Undefined -> "?"
    | V_None -> "None"
    | V_Int n -> Int.to_string n
    | V_Bool b -> Bool.to_string b
    | V_Str s -> s
end

(* Module for input/output built-in functions.
 *
 * Alas, this module needs to be defined as part of the Interp module, because
 * the I/O functions rely on Value.t.  I guess the right way to do this is for
 * [fprintf] to take a list of metalanguage type values, and for [fscanf] to
 * return a disjoint sum of metalanguage type values, and let the caller
 * unpack/pack from/to [Value.t] values.
 *)
module Io = struct

  (* The input source and output destination is abstracted, because there
   * are two use cases that are rather different.  The interactive
   * interpreter uses standard input and standard output for input and
   * output.  But the automated tests need the input source and output
   * destination to be programmatic values (the former is read from a test
   * specification, and the latter has to be compared to the test
   * specification).  The "right" way to do this is to make the interpreter
   * itself a functor that takes an IO module as an argument, but that is a
   * little much for this project, so instead we define this Io module with
   * the input source (`in_channel`) and output destination (`output`)
   * references that can be changed by the client that is using the
   * interpreter.
   *)

  (* The input channel.  get_* and prompt_* read from this channel.  Default
   * is standard input.
   *)
  let in_channel : Scanf.Scanning.in_channel ref =
    ref Scanf.Scanning.stdin

  (* The output function.  printf calls this function for output.  Default is
   * to print the string to standard output and flush.
   *)
  let output : (string -> unit) ref = 
    ref (
      fun s ->
        Out_channel.output_string Out_channel.stdout s ;
        Out_channel.flush Out_channel.stdout
    )

  (* tail s = s[1..].
   *)
  let tail (s : string) : string =
    String.sub s 1 (String.length s - 1)

  (* tailtail s = tail(tail s).
   *)
  let tailtail (s : string) : string =
    tail (tail s)

  (* scons c s = String.make 1 c ^ s.
   *)
  let scons (c : char) (s : string) : string =
    String.make 1 c ^ s

  (* do_fprintf fmt vs:  print [vs] to stdout according to [fmt].
   *)
  let do_fprintf (fmt : string) (vs : Value.t list) : unit =
    let rec build_result (fmt : string) (vs : Value.t list) : string =
      if fmt = "" 
      then
        match vs with
        | [] -> ""
        | _ -> raise @@ TypeError "Too many values to print for format string"
      else if fmt.[0] != '%' then scons fmt.[0] (build_result (tail fmt) vs)
      else if String.length fmt = 1
      then raise @@ TypeError "Malformed format string (incomplete %)"
      else
        match (String.sub fmt 0 2, vs) with
        | ("%d", V_Int n :: vs) -> 
          Printf.sprintf
            "%d%s"
            n
            (build_result (tailtail fmt) vs)
        | ("%b", V_Bool b :: vs) -> 
          Printf.sprintf
            "%b%s"
            b
            (build_result (tailtail fmt) vs)
        | ("%s", V_Str s :: vs) -> 
          Printf.sprintf
            "%s%s"
            s
            (build_result (tailtail fmt) vs)
        | _ ->
          raise @@ TypeError "Bad % specifier or incorrect value type"
    in

    !output (build_result fmt vs)

  (* do_fscanf fmt = v, where v is the value read from stdin according to fmt.
   *)
  let do_fscanf (fmt : string) : Value.t =
    let fmt' : string = String.trim fmt in
    match fmt' with
    | "%d" -> Value.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
    | "%b" -> Value.V_Bool (Scanf.bscanf !in_channel " %b" (fun b -> b))
    | "%s" -> Value.V_Str (Scanf.bscanf !in_channel " %s" (fun s -> s))
    | _ ->
      raise @@ TypeError (
        Printf.sprintf
        "Bad scanf format string: %s"
        fmt
      )

end

let binop (operation : Ast.Expr.binop) (v1 : Value.t) (v2 : Value.t) : Value.t = 
  match (operation, v1, v2) with 

  | (Ast.Expr.Plus, Value.V_Int v, Value.V_Int v') -> Value.V_Int (v + v')

  | (Ast.Expr.Minus, Value.V_Int v, Value.V_Int v') -> Value.V_Int (v - v')

  | (Ast.Expr.Times, Value.V_Int v, Value.V_Int v') -> Value.V_Int (v * v')

  | (Ast.Expr.Div, Value.V_Int v, Value.V_Int v') -> if v' = 0 then raise Division_by_zero else Value.V_Int (v / v')

  | (Ast.Expr.Mod, Value.V_Int v, Value.V_Int v') -> if v' = 0 then raise Division_by_zero else Value.V_Int (v mod v')

  | (Ast.Expr.And, Value.V_Bool v, Value.V_Bool v') -> Value.V_Bool (v && v')

  | (Ast.Expr.Or,Value.V_Bool v, Value.V_Bool v') -> Value.V_Bool (v || v')

  | (Ast.Expr.Eq, Value.V_Int v, Value.V_Int v') -> if v = v' then Value.V_Bool (true) else Value.V_Bool (false)

  | (Ast.Expr.Eq, Value.V_Bool v, Value.V_Bool v') -> if v = v' then Value.V_Bool (true) else Value.V_Bool (false)

  | (Ast.Expr.Ne, Value.V_Int v, Value.V_Int v') -> if v = v' then Value.V_Bool (false) else Value.V_Bool (true)

  | (Ast.Expr.Ne, Value.V_Bool v, Value.V_Bool v') -> if v = v' then Value.V_Bool (false) else Value.V_Bool (true)

  | (Ast.Expr.Lt, Value.V_Int v, Value.V_Int v') -> if v < v' then Value.V_Bool (true) else Value.V_Bool (false)

  | (Ast.Expr.Le, Value.V_Int v, Value.V_Int v') -> if v <= v' then Value.V_Bool (true) else Value.V_Bool (false)

  | (Ast.Expr.Gt, Value.V_Int v, Value.V_Int v') -> if v > v' then Value.V_Bool (true) else Value.V_Bool (false)
  
  | (Ast.Expr.Ge, Value.V_Int v, Value.V_Int v') -> if v >= v' then Value.V_Bool (true) else Value.V_Bool (false)

  | _ -> raise (TypeError "invalid operation")

let unop (operation : Ast.Expr.unop) (v : Value.t) : Value.t = 
  match (operation, v) with 

  | (Ast.Expr.Neg, Value.V_Int v') -> Value.V_Int (-v')

  | (Ast.Expr.Not, Value. V_Bool v') -> Value.V_Bool (not v')

  | _ -> raise (TypeError "invalid operation")
  (* Module for environments.
 *)
module Env = struct
  type t = (Ast.Id.t * Value.t) list
  [@@deriving show]

  (*  empty = ρ, where dom ρ = ∅.
   *)
  let empty : t = []


  (** lookup env x = v, where v = env(x)
  * returns the value bound to x
  * raises unbound variable if not_found is returned; meaning the binding does not exist
  *)
  let lookup (env : t) (x : Ast.Id.t) : Value.t = 
    try List.assoc x env 
    with Not_found -> raise (UnboundVariable x)

  (** add env x v = env p{x->v}
  * adds new binding in the environment
  *)
  let add (env : t) (x : Ast.Id.t) (v : Value.t) : t =
  if List.mem_assoc x env then raise (MultipleDeclaration x)
  else (x, v) :: env

  (** update env x v = env p{x->v}
  * creates a new binding in the envrionment and removes the old one
  *)
  let update (env : t) (x : Ast.Id.t) (v : Value.t) : t =
  if List.mem_assoc x env then
    (x, v) :: List.remove_assoc x env
  else
    raise (UnboundVariable x)

end
module EnvBlock = struct
  type t = Env.t list

  let empty : t = [Env.empty]
  (** enter a new block
  * we add a new empty environment to the head of the list
  *)
  let enter_block (eb : t) : t =
    Env.empty :: eb
  
  (** exit a block
  *)
  let exit_block (eb : t) : t =
    match eb with
    | [] -> failwith "cannot exit empty block"
    | _ :: rest -> rest

  (** we look for the variable in the environments
  * starting at the head and working backwards
  *)
  let rec lookup (eb : t) (x : Ast.Id.t) : Value.t =
    match eb with
  | [] -> raise (UnboundVariable x)
  | env :: rest ->
      try Env.lookup env x
      with UnboundVariable _ -> lookup rest x
  
  (** we add the declared variable to the environment
  * at the head of the list
  *)
  let add (eb : t) (x : Ast.Id.t) (v : Value.t) : t =
  match eb with
  | [] -> failwith "no active scope"
  | env :: rest -> Env.add env x v :: rest
  
  (** update variable in the nearest scope
  * and raise exception if variable doesn't exist in any
  *)
  let rec update (eb : t) (x : Ast.Id.t) (v : Value.t) : t =
  match eb with
  | [] -> raise (UnboundVariable x)
  | env :: rest ->
      if List.mem_assoc x env then
        Env.update env x v :: rest
      else
        env :: update rest x v
end

module Frame = struct
  type t = 
  | EnvBlockFrame of EnvBlock.t 
  | ReturnFrame of Value.t

  let empty : t = EnvBlockFrame EnvBlock.empty
end

let exec (p : Ast.Prog.t) : unit =
  let funs =
    match p with
    | Ast.Prog.Pgm fs -> fs
  in

  let find_function (name : Ast.Id.t) : Ast.Prog.fundef =
    try List.find (fun (f, _, _) -> f = name) funs
    with Not_found -> raise (UndefinedFunction name)
  in

  let rec eval (eb : EnvBlock.t) (e : Ast.Expr.t) : Value.t =
    match e with
    | Ast.Expr.Var x -> EnvBlock.lookup eb x
    | Ast.Expr.Num n -> Value.V_Int n
    | Ast.Expr.Bool b -> Value.V_Bool b
    | Ast.Expr.Str s -> Value.V_Str s
    | Ast.Expr.Unop (op, e1) -> unop op (eval eb e1)
    | Ast.Expr.Binop (op, e1, e2) -> binop op (eval eb e1) (eval eb e2)
    | Ast.Expr.Call (fname, args) ->
        begin
          match fname, args with

          (* fprintf case *)
          | "fprintf", _stream :: fmt_exp :: rest ->
              let fmt =
                match eval eb fmt_exp with
                | Value.V_Str s -> s
                | _ -> raise (TypeError "format must be string")
              in
              let vs = List.map (fun a -> eval eb a) rest in
              Io.do_fprintf fmt vs;
              Value.V_None

          (* normal function *)
          | _ ->
              let arg_vals = List.map (fun a -> eval eb a) args in
              let (_, params, ss) = find_function fname in
              let local_env =
                List.fold_left2
                  (fun env param arg -> Env.add env param arg)
                  Env.empty
                  params
                  arg_vals
              in
              let local_eb = [local_env] in
              match exec_stms local_eb ss with
              | Frame.ReturnFrame v -> v
              | Frame.EnvBlockFrame _ -> raise (NoReturn fname)
        end

  and exec_stm (eb : EnvBlock.t) (s : Ast.Stm.t) : Frame.t =
    match s with
    | Ast.Stm.Assign (x, e) ->
        let v = eval eb e in
        Frame.EnvBlockFrame (EnvBlock.update eb x v)

    | Ast.Stm.Expr e ->
        let _ = eval eb e in
        Frame.EnvBlockFrame eb

    | Ast.Stm.Fscanf (_filevar, fmt, target) ->
        let v =
          try Io.do_fscanf fmt
          with _ -> raise (TypeError "bad scanf input")
        in
        Frame.EnvBlockFrame (EnvBlock.update eb target v)

    | Ast.Stm.VarDec decls ->
        let eb' =
          List.fold_left
            (fun acc (x, init) ->
              match init with
              | None -> EnvBlock.add acc x Value.V_Undefined
              | Some e ->
                  let v = eval acc e in
                  EnvBlock.add acc x v)
            eb decls
        in
        Frame.EnvBlockFrame eb'

    | Ast.Stm.Block ss ->
        let eb' = EnvBlock.enter_block eb in
        begin
          match exec_stms eb' ss with
          | Frame.EnvBlockFrame eb_after ->
              Frame.EnvBlockFrame (EnvBlock.exit_block eb_after)
          | Frame.ReturnFrame v ->
              Frame.ReturnFrame v
        end

    | Ast.Stm.IfElse (e, s1, s2) ->
        begin
          match eval eb e with
          | Value.V_Bool true -> exec_stm eb s1
          | Value.V_Bool false -> exec_stm eb s2
          | _ -> raise (TypeError "if condition must be bool")
        end

    | Ast.Stm.While (e, s) ->
        begin
          match eval eb e with
          | Value.V_Bool true ->
              begin
                match exec_stm eb s with
                | Frame.ReturnFrame v -> Frame.ReturnFrame v
                | Frame.EnvBlockFrame eb' ->
                    exec_stm eb' (Ast.Stm.While (e, s))
              end
          | Value.V_Bool false -> Frame.EnvBlockFrame eb
          | _ -> raise (TypeError "while condition must be bool")
        end

    | Ast.Stm.Return None ->
        Frame.ReturnFrame Value.V_None

    | Ast.Stm.Return (Some e) ->
        Frame.ReturnFrame (eval eb e)

  and exec_stms (eb : EnvBlock.t) (ss : Ast.Stm.t list) : Frame.t =
    match ss with
    | [] -> Frame.EnvBlockFrame eb
    | s :: rest ->
        begin
          match exec_stm eb s with
          | Frame.EnvBlockFrame eb' -> exec_stms eb' rest
          | Frame.ReturnFrame v -> Frame.ReturnFrame v
        end
  in

  let _ = eval EnvBlock.empty (Ast.Expr.Call ("main", [])) in
  ()



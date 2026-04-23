(* COMP 324 Project 3:  C- interpreter with dynamic security enforcement.
 *
 * N. Danner

 * Team Berry: Ramon Ruiz, Dalton Soper, Tamiraa Sanjaajav 
 *)



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

(* Raised when public output depends on private input.
 *)
exception SecurityError

(* Raised when the no sensitive upgrade condition is violated.
 *
 * Code that fails the NSU check must raise this exception, even if
 * [SecurityError] could be raised for other reasons.
 *)
exception NSU_Error



(* Security labels.
 *
 * This module defines the two-point security lattice Low <= High.  However,
 * clients must treat the security labels abstractly in the sense that they
 * may only use the values specified in the given signature.  Notice that
 * those values:
 * - Define the type of security labels, but not the definition of that type;
 * - Define bottom, leq, and join, which taken together should make the type
 *   of security labels into a join semi-lattice.
 * - Define some convenience functions for printing and equality checking.
 * - Define [of_channel], which returns the security level associated to a
 *   given input or output channel by name.
 *
 * For this particular security lattice, `bottom` = `Low` and security labels
 * are associated to I/O channels as follows:
 *
 * stdout_lo, stdin_lo --> Low
 * stdout_hi, stdin_hi --> High
 *)
module SecLab : sig
  (** The type of a security label.
   *)
  type t

  (** The bottom security label.  It must be that [leq bottom x] = [true] for
   *  all x.
   *)
  val bottom : t

  (** A partial order on security labels.
   *)
  val leq : t -> t -> bool

  (** Equality predicate for security labels.  It must be that
   * [eq x y] = [leq x y && leq y x].
   *)
  val eq : t -> t -> bool

  (** [join x y] is the least upper bound of [x] and [y] with respect to
   * [leq].
   *)
  val join : t -> t -> t

  (** [to_string x] = a string representation of [x].
   *)
  val to_string : t -> string

  (** [pp] is a formatter for [t].
   *)
  val pp : Format.formatter -> t -> unit

  (** [of_channel ch] = the security level associated to the channel [ch].
   *)
  val of_channel : Ast.Id.t -> t
end = struct

  type t = Low | High
  [@@deriving show]

  let bottom = Low

  (* to_string x = a string representation of x.
   *)
  let to_string (x : t) : string =
    match x with
    | Low -> "L"
    | High -> "H"

  (* eq x y = true,  if x and y are the same security label
   *          false, otherwise.
   *)
  let eq (x : t) (y : t) : bool =
    x = y


  (* leq x y = true,  eq x y or x = Low and y = High
   *           false, o/w.
   *)
  let leq (x : t) (y : t) : bool =
    match (x, y) with 
    | (High, High) -> true 
    | (High, Low) -> false 
    | (Low, High) -> true 
    | (Low, Low) -> true 


  (* join x y = the maximum of x and y with respect to `leq`.
   *)
  let join (x: t) (y : t) : t =
    match (x, y) with 
    | (High, _) | (_, High) -> High
    | _ -> Low
  

  (* [of_channel ch] = the security label associated to the channel [ch].
   *)
  let of_channel (ch : Ast.Id.t) : t =
    match ch with
    | ("stdout" | "stdin") -> Low
    | ("stdout_lo" | "stdin_lo") -> Low
    | ("stdout_hi" | "stdin_hi") -> High
    | _ -> invalid_arg ch

end

(* Values.
 *)
 module PrimValue = struct
  type t = 
    | V_Undefined of SecLab.t
    | V_None of SecLab.t
    | V_Int of int * SecLab.t
    | V_Bool of bool * SecLab.t
    | V_Str of string * SecLab.t 
    [@@deriving show]

  (* to_string v = a string representation of v (more human-readable than
   * `show`.
   *)
  let to_string (v : t) : string =
    match v with
    | V_Undefined _ -> "?"
    | V_None _ -> "None"
    | V_Int (n,_) -> Int.to_string n
    | V_Bool (b, _) -> Bool.to_string b
    | V_Str (s, _) -> s
end


(* Module for input/output built-in functions.
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

  (* The input channel.  Default is standard input.
   *)
  let in_channel : Scanf.Scanning.in_channel ref =
    ref Scanf.Scanning.stdin

  (* The output function.  Default is to print the string to standard output
   * and flush.
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
  let do_fprintf (fmt : string) (vs : PrimValue.t list) : unit =
    let rec build_result (fmt : string) (vs : PrimValue.t list) : string =
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
        | (_, []) ->
          raise @@ TypeError "Too few values to print for format string"
        | ("%d", V_Int (n, _) :: vs') -> 
          Printf.sprintf
            "%d%s"
            n
            (build_result (tailtail fmt) vs')
        | ("%b", V_Bool (b, _) :: vs') -> 
          Printf.sprintf
            "%b%s"
            b
            (build_result (tailtail fmt) vs')
        | ("%s", V_Str (s, _) :: vs') -> 
          Printf.sprintf
            "%s%s"
            s
            (build_result (tailtail fmt) vs')
        | _ ->
          raise @@ TypeError "Bad % specifier or incorrect value type"
    in

    !output (build_result fmt vs)

  (* do_fscanf fmt = v, where v is the value read from stdin according to fmt.
   *)
  let do_fscanf (fmt : string) (slab : SecLab.t ): PrimValue.t =
    let fmt' : string = String.trim fmt in
    match fmt' with
    | "%d" -> V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n), slab)
    | "%b" -> V_Bool (Scanf.bscanf !in_channel " %b" (fun b -> b),slab)
    | "%s" -> V_Str (Scanf.bscanf !in_channel " %s" (fun s -> s), slab)
    | _ ->
      raise @@ TypeError (
        Printf.sprintf
        "Bad scanf format string: %s"
        fmt
      )

end


let binop (operation : Ast.Expr.binop) (v1 : PrimValue.t) (v2 : PrimValue.t) : PrimValue.t = 
  match (operation, v1, v2) with 

  | (Ast.Expr.Plus, PrimValue.V_Int (v, seclab1), PrimValue.V_Int (v', seclab2)) -> PrimValue.V_Int (v + v', SecLab.join seclab1 seclab2)

  | (Ast.Expr.Minus, PrimValue.V_Int (v, seclab1), PrimValue.V_Int (v', seclab2)) -> PrimValue.V_Int (v - v', SecLab.join seclab1 seclab2)

  | (Ast.Expr.Times, PrimValue.V_Int (v, seclab1), PrimValue.V_Int (v', seclab2)) -> PrimValue.V_Int (v * v', SecLab.join seclab1 seclab2)

  | (Ast.Expr.Div, PrimValue.V_Int (v, seclab1), PrimValue.V_Int (v', seclab2)) -> if v' = 0 then raise Division_by_zero else PrimValue.V_Int (v / v', SecLab.join seclab1 seclab2)

  | (Ast.Expr.Mod, PrimValue.V_Int (v, seclab1), PrimValue.V_Int (v', seclab2)) -> if v' = 0 then raise Division_by_zero else PrimValue.V_Int (v mod v', SecLab.join seclab1 seclab2)

  | (Ast.Expr.And, PrimValue.V_Bool (v, seclab1), PrimValue.V_Bool (v', seclab2)) -> PrimValue.V_Bool (v && v', SecLab.join seclab1 seclab2)

  | (Ast.Expr.Or, PrimValue.V_Bool (v, seclab1), PrimValue.V_Bool (v', seclab2)) -> PrimValue.V_Bool (v || v', SecLab.join seclab1 seclab2)

  | (Ast.Expr.Eq, PrimValue.V_Int (v, seclab1), PrimValue.V_Int (v', seclab2)) -> if v = v' then PrimValue.V_Bool (true, SecLab.join seclab1 seclab2) else PrimValue.V_Bool (false, SecLab.join seclab1 seclab2)

  | (Ast.Expr.Eq, PrimValue.V_Bool (v, seclab1), PrimValue.V_Bool (v', seclab2)) -> if v = v' then PrimValue.V_Bool (true, SecLab.join seclab1 seclab2) else PrimValue.V_Bool (false, SecLab.join seclab1 seclab2)

  | (Ast.Expr.Ne, PrimValue.V_Int (v, seclab1), PrimValue.V_Int (v', seclab2)) -> if v = v' then PrimValue.V_Bool (false, SecLab.join seclab1 seclab2) else PrimValue.V_Bool (true,  SecLab.join seclab1 seclab2)

  | (Ast.Expr.Ne, PrimValue.V_Bool (v, seclab1), PrimValue.V_Bool (v', seclab2)) -> if v = v' then PrimValue.V_Bool (false,  SecLab.join seclab1 seclab2) else PrimValue.V_Bool (true,  SecLab.join seclab1 seclab2)

  | (Ast.Expr.Lt, PrimValue.V_Int (v, seclab1), PrimValue.V_Int (v', seclab2)) -> if v < v' then PrimValue.V_Bool (true,  SecLab.join seclab1 seclab2) else PrimValue.V_Bool (false,  SecLab.join seclab1 seclab2)

  | (Ast.Expr.Le, PrimValue.V_Int (v, seclab1), PrimValue.V_Int (v', seclab2)) -> if v <= v' then PrimValue.V_Bool (true,  SecLab.join seclab1 seclab2) else PrimValue.V_Bool (false,  SecLab.join seclab1 seclab2)

  | (Ast.Expr.Gt, PrimValue.V_Int (v, seclab1), PrimValue.V_Int (v', seclab2)) -> if v > v' then PrimValue.V_Bool (true,  SecLab.join seclab1 seclab2) else PrimValue.V_Bool (false,  SecLab.join seclab1 seclab2)
  
  | (Ast.Expr.Ge, PrimValue.V_Int (v, seclab1), PrimValue.V_Int (v', seclab2)) -> if v >= v' then PrimValue.V_Bool (true,  SecLab.join seclab1 seclab2) else PrimValue.V_Bool (false,  SecLab.join seclab1 seclab2)

  | _ -> raise (TypeError "invalid operation")

let unop (operation : Ast.Expr.unop) (v : PrimValue.t) : PrimValue.t = 
  match (operation, v) with 

  | (Ast.Expr.Neg, PrimValue.V_Int (v', secval)) -> PrimValue.V_Int (-v', secval)

  | (Ast.Expr.Not, PrimValue.V_Bool (v', secval)) -> PrimValue.V_Bool (not v', secval)

  | _ -> raise (TypeError "invalid operation")


  (* Module for environments.
 *)
module Env = struct
  type t = (Ast.Id.t * PrimValue.t) list
  [@@deriving show]

  (*  empty = ρ, where dom ρ = ∅.
   *)
  let empty : t = []


  (** lookup env x = v, where v = env(x)
  * returns the value bound to x
  * raises unbound variable if not_found is returned; meaning the binding does not exist
  *)
  let lookup (env : t) (x : Ast.Id.t) : PrimValue.t = 
    try List.assoc x env 
    with Not_found -> raise (UnboundVariable x)

  (** add env x v = env p{x->v}
  * adds new binding in the environment
  *)
  let add (env : t) (x : Ast.Id.t) (v : PrimValue.t) : t =
  if List.mem_assoc x env then raise (MultipleDeclaration x)
  else (x, v) :: env

  (** update env x v = env p{x->v}
  * creates a new binding in the envrionment and removes the old one
  *)
  let update (env : t) (x : Ast.Id.t) (v : PrimValue.t) : t =
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
  let rec lookup (eb : t) (x : Ast.Id.t) : PrimValue.t =
    match eb with
  | [] -> raise (UnboundVariable x)
  | env :: rest ->
      try Env.lookup env x
      with UnboundVariable _ -> lookup rest x
  
  (** we add the declared variable to the environment
  * at the head of the list
  *)
  let add (eb : t) (x : Ast.Id.t) (v : PrimValue.t) : t =
  match eb with
  | [] -> failwith "no active scope"
  | env :: rest -> Env.add env x v :: rest
  
  (** update variable in the nearest scope
  * and raise exception if variable doesn't exist in any
  *)
  let rec update (eb : t) (x : Ast.Id.t) (v : PrimValue.t) : t =
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
  | ReturnFrame of PrimValue.t 

  let empty : t = EnvBlockFrame EnvBlock.empty
end

let security_level (value : PrimValue.t) (sec : SecLab.t) : PrimValue.t = 
  match value with 
    | PrimValue.V_Int (y, sl) -> PrimValue.V_Int (y, SecLab.join sl sec)
    | PrimValue.V_Bool (y, sl) -> PrimValue.V_Bool (y, SecLab.join sl sec)
    | PrimValue.V_Str (y, sl) -> PrimValue.V_Str (y, SecLab.join sl sec)
    | PrimValue.V_None sl -> PrimValue.V_None (SecLab.join sl sec)
    | PrimValue.V_Undefined sl -> PrimValue.V_Undefined (SecLab.join sl sec)


let exec (p : Ast.Prog.t) : unit =
  let funs =
    match p with
    | Ast.Prog.Pgm fs -> fs
  in

  let find_function (name : Ast.Id.t) : Ast.Prog.fundef =
    try List.find (fun (f, _, _) -> f = name) funs
    with Not_found -> raise (UndefinedFunction name)
  in

  let rec eval (eb : EnvBlock.t) (e : Ast.Expr.t) (sec : SecLab.t) : PrimValue.t =
    let calculated = 
      match e with
      | Ast.Expr.Var x -> EnvBlock.lookup eb x

      | Ast.Expr.Num n -> PrimValue.V_Int (n, SecLab.bottom)

      | Ast.Expr.Bool b -> PrimValue.V_Bool (b, SecLab.bottom)

      | Ast.Expr.Str s -> PrimValue.V_Str (s, SecLab.bottom)

      | Ast.Expr.Unop (op, e1) -> unop op (eval eb e1 sec) 

      | Ast.Expr.Binop (op, e1, e2) -> binop op (eval eb e1 sec) (eval eb e2 sec) 
        
      | Ast.Expr.Call (fname, args) ->
          begin
            match fname, args with

            (* fprintf case *)
            | "fprintf", _stream :: fmt_exp :: rest ->
              let seclabel = 
                match _stream with 
                | Ast.Expr.Var x -> SecLab.of_channel x
                | _ -> raise (TypeError "format must be string")
              in

              let vs = List.map (fun a -> eval eb a sec) rest in

              List.iter (fun v -> 
               let new_label = 
                  match v with 
                  | PrimValue.V_Int (_, sl) -> sl
                  | PrimValue.V_Bool (_, sl) -> sl 
                  | PrimValue.V_Str (_, sl) -> sl
                  | PrimValue.V_None sl -> sl
                  | PrimValue.V_Undefined sl -> sl
              in 
              if not (SecLab.leq (SecLab.join new_label sec) seclabel) then raise SecurityError) vs ;

              let fmt =
                match eval eb fmt_exp sec with
                | PrimValue.V_Str (s, _) -> s
                | _ -> raise (TypeError "format must be string")
              in

              Io.do_fprintf fmt vs;

              PrimValue.V_None SecLab.bottom



            (* normal function *)
            | _ ->
              let arg_vals = List.map (fun a -> eval eb a sec) args in
              let (_, params, ss) = find_function fname in
              let local_env =
                List.fold_left2
                  (fun env param arg -> (param, arg) :: env)
                  Env.empty
                  params
                  arg_vals
              in
              let local_eb = [local_env]  in
              match exec_stms local_eb ss SecLab.bottom with
              | Frame.ReturnFrame v -> v
              | Frame.EnvBlockFrame _ -> raise (NoReturn fname)
              
    
        end
      in
        calculated 


  and exec_stm (eb : EnvBlock.t) (s : Ast.Stm.t) (sec : SecLab.t) : Frame.t =
    let old_label (v : PrimValue.t) : SecLab.t = 
      match v with 
      | PrimValue.V_Int (_, sl) -> sl
      | PrimValue.V_Bool (_, sl) -> sl 
      | PrimValue.V_Str (_, sl) -> sl
      | PrimValue.V_None sl -> sl
      | PrimValue.V_Undefined sl ->sl
    in 


    match s with
    | Ast.Stm.Assign (x, e) ->
        let new_v = eval eb e sec in
        let old_v = EnvBlock.lookup eb x in 
        let old_s = old_label old_v in 


        if not (SecLab.leq sec old_s) then raise NSU_Error
        else if not (SecLab.leq (old_label new_v) old_s) then raise SecurityError 
         else Frame.EnvBlockFrame (EnvBlock.update eb x new_v)
      

    | Ast.Stm.Expr e ->
        let _ = eval eb e sec in
        Frame.EnvBlockFrame eb


    | Ast.Stm.Fscanf (ch_name, fmt, target) ->
          let label' = SecLab.of_channel ch_name in
      
          let old_val = EnvBlock.lookup eb target in
          if not (SecLab.leq sec (old_label old_val)) then raise NSU_Error;

          let v =
            try Io.do_fscanf fmt label'
            with _ -> raise (TypeError "bad scanf input")
          in
      
          let new_v = security_level v sec in
          Frame.EnvBlockFrame (EnvBlock.update eb target new_v)
      
      


    | Ast.Stm.VarDec decls ->
        let eb' =
          List.fold_left
            (fun acc (x, init) ->
              match init with
              | None -> EnvBlock.add acc x (PrimValue.V_Undefined SecLab.bottom)
              | Some e ->
                  let v = eval acc e sec in
                  EnvBlock.add acc x v)
            eb decls
        in
        Frame.EnvBlockFrame eb'


    | Ast.Stm.Block ss ->
        let eb' = EnvBlock.enter_block eb in
        begin
          match exec_stms eb' ss sec with
          | Frame.EnvBlockFrame eb_after ->
              Frame.EnvBlockFrame (EnvBlock.exit_block eb_after)
          | Frame.ReturnFrame v ->
              Frame.ReturnFrame v
        end


    | Ast.Stm.IfElse (e, s1, s2) ->
        begin
          let val' = eval eb e sec in 
          let sec' = SecLab.join sec (old_label val') in 

          (match val' with
          | PrimValue.V_Bool (true, _) -> exec_stm eb s1 sec'
          | PrimValue.V_Bool (false, _) -> exec_stm eb s2 sec'
          | _ -> raise (TypeError "if condition must be bool")
          )
        end


    | Ast.Stm.While (e, s) ->
      let val' = eval eb e sec in 
      let sec' = SecLab.join sec (old_label val') in 

        (match val' with
          | PrimValue.V_Bool (true, _) -> 
              begin
                match exec_stm eb s sec' with
                | Frame.ReturnFrame v -> Frame.ReturnFrame v
                | Frame.EnvBlockFrame eb' -> exec_stm eb' (Ast.Stm.While (e, s)) sec'
              end

          | PrimValue.V_Bool (false, _) -> Frame.EnvBlockFrame eb
          | _ -> raise (TypeError "while condition must be bool")
        )

    | Ast.Stm.Return None ->
        Frame.ReturnFrame (PrimValue.V_None SecLab.bottom)

    | Ast.Stm.Return (Some e) ->
      let v = eval eb e sec in 
      let v_label = 
        match v with 
        | PrimValue.V_Int (_, sl) -> sl
        | PrimValue.V_Bool (_, sl) -> sl 
        | PrimValue.V_Str (_, sl) -> sl
        | PrimValue.V_None sl -> sl
        | PrimValue.V_Undefined sl ->sl
      in 

      if not (SecLab.leq sec v_label) then raise NSU_Error; 
      Frame.ReturnFrame v 

    
    

  and exec_stms (eb : EnvBlock.t) (ss : Ast.Stm.t list) (sec : SecLab.t) : Frame.t =
    match ss with
    | [] -> Frame.EnvBlockFrame eb
    | s :: rest ->
        begin
          match exec_stm eb s sec with
          | Frame.EnvBlockFrame eb' -> exec_stms eb' rest sec
          | Frame.ReturnFrame v -> Frame.ReturnFrame v
        end
  in

  let _ = eval EnvBlock.empty (Ast.Expr.Call ("main", [])) SecLab.bottom in
  ()


(**
*
* dune build @runtest-tests    
*)
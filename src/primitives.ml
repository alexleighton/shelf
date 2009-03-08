(*
 * Shelf is a functional, dynamic, stack-based language.
 * Copyright (C) 2009  Alex Leighton
 * This file is part of Shelf.
 *
 * Shelf is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Shelf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Shelf.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Module containing the primitive words of the language. *)

open Types
open Num

(** Module containing all of the special words used by the interpreter. *)
module Interpreter = struct

  (** Quit function. Raises the "End_of_file" exception, which should be
      gracefully handled by the interpreter. *)
  let quit ((s:svalue Stackp.t),(exps: program),(n:svalue Nm.namespace)) =
    raise End_of_file

  (** Removes all values from the stack. *)
  let clear (s,exps,n) = let s = Stackp.clear s in (s,exps,n)

  (** Reads and evaluates all code from the given file. *)
  let load (s,exps,n) =
    match Stackp.pop s with
        SString(str),s -> (
          try
            let lex = Lexing.from_channel (open_in str) in
            let cmds = Parser.toplevel Lexer.token lex in
              (s,(cmds@exps),n)
          with
            | Failure("lexing: empty token") ->
                error "lexing: empty token"
            | Parsing.Parse_error ->
                error "Parser: bad syntax."
            | Sys_error(msg) -> error ("Could not load file: "^msg)
        )
      | _ -> raise (invalid_arg ":load: expects a string")

  let build w = SBuiltin w

  (** List consisting of the words and their corresponding functions. *)
  let primitives = [
    ":quit",(build ":quit"),quit ;
    ":q",(build ":quit"),quit ;
    ":exit",(build ":exit"),quit ;
    ":e",(build ":e"),quit ;
    ":clear",(build ":clear"),clear ;
    ":c",(build ":c"),clear ;
    ":load",(build ":load"),load ;
    ":l",(build ":l"),load ;
  ]

  (** Creates two namespaces, one containing SBuiltin values for all of
      the words this module exports, and the other containing the actual
      OCaml functions. *)
  let create () =
    let vs = Nm.create () in
    let ws = Nm.create () in
    let bind (s,t,w) (vspc,wspc) = (Nm.bind vspc s t),(Nm.bind wspc s w) in
      List.fold_right bind primitives (vs,ws)

end

(** Module containing words pertaining to Lists. The lists in question are
    heterogeneous. *)
module Lists = struct

  let build w = SBuiltin w

  (** Pops a value off the stack and appends it onto the front of the list
      below it. *)
  let cons ((s : svalue Stackp.t),(exps: program),(n:svalue Nm.namespace)) =
    let top,s = Stackp.pop s in
      match Stackp.pop s with
          SList(vs),s ->
            let s = Stackp.push s (SList (top::vs)) in (s,exps,n)
        | _ -> invalid_arg "invalid argument: cons: (list 'a -> list)"

  (** Takes a quotation off the top of the stack and turns it into a
      heterogenous list. *)
  let list (s,exps,n) =
    match Stackp.pop s with
        SQuote(vs),s ->
          let s = Stackp.push s (SList vs) in (s,exps,n)
      | _ -> invalid_arg "invalid argument: requires a quotation on top of the stack."

  (** Takes two lists off the top of a stack, appending the bottom list onto
      the back-end of the top list, and pushing the resulting list back onto
      the stack. *)
  let append (s,exps,n) =
    match Stackp.get_two s with
        SList(xs),SList(ys),s ->
          let s = Stackp.push s (SList (xs @ ys)) in (s,exps,n)
      | _ -> invalid_arg "append: requires two lists."

  (** Copies the head of a list on the top of a stack and pushes it on. *)
  let head (s,exps,n) =
    match Stackp.pop s with
        SList(x::xs),s ->
          let s = Stackp.push s (SList xs) in
          let s = Stackp.push s x in
            (s,exps,n)
      | SList([]),s -> invalid_arg "head: requires a non-empty list."
      | _ -> invalid_arg "head: requires a list."

  (** Pops off a list and pushes on the tail of the List. *)
  let tail (s,exps,n) =
    match Stackp.pop s with
        SList(x::xs),s -> let s = Stackp.push s (SList xs) in (s,exps,n)
      | SList([]),s -> let s = Stackp.push s (SList []) in (s,exps,n)
      | _ -> invalid_arg "tail: requires a list."

  (** Pushes on true if the top element of the stack is an empty list. *)
  let isnil (s,exps,n) =
    match Stackp.pop s with
        SList([]),s ->
          let s = Stackp.push s (SList []) in
          let s = Stackp.push s (SBool true) in (s,exps,n)
      | SList(xs),s ->
          let s = Stackp.push s (SList xs) in
          let s = Stackp.push s (SBool false) in (s,exps,n)
      | _ -> invalid_arg "nil?: requires a list."

          (*
  let foldr (s,exps,n) =
    match Stackp.get_two s with
      | SList(x::xs),SQuote(SWord(name)::[]),s ->

  let len (s,exps,n) =
    match Stackp.pop s with
        *)

  (** List consisting of the words and their corresponding functions. *)
  let primitives = [
    "cons",(build "cons"),cons ;
    "list",(build "list"),list ;
    "append",(build "append"),append ;
    "head",(build "head"),head ;
    "tail",(build "tail"),tail ;
    "nil?",(build "nil?"),isnil ;
  ]

  (** Creates two namespaces, one containing SBuiltin values for all of
      the words this module exports, and the other containing the actual
      OCaml functions. *)
  let create () =
    let vs = Nm.create () in
    let ws = Nm.create () in
    let bind (s,t,w) (vspc,wspc) = (Nm.bind vspc s t),(Nm.bind wspc s w) in
      List.fold_right bind primitives (vs,ws)

end

(** Module containing all of the most primitive words. *)
module Kernel = struct

  (** Pops off the top value of the stack. *)
  let pop ((s : svalue Stackp.t),(exps: program),(n:svalue Nm.namespace)) =
    let _,s = Stackp.pop s in (s,exps,n)

  (** Duplicates the top value of the stack, pushing it back on. *)
  let dup (s,exps,n) = let s = Stackp.push s (Stackp.top s) in (s,exps,n)

  (** Swaps the top two values of the stack. *)
  let swap (s,exps,n) =
    let bottom,top,s = Stackp.get_two s in
    let s = Stackp.push s top in
    let s = Stackp.push s bottom in
      (s,exps,n)

  (** Pops a quotation off the stack, temporarily storing the second value.
      It then evaluates the quotation, and places the second value on the
      top of the stack. *)
  let dip (s,exps,n) =
    let v,s = Stackp.pop s in
      match v with
          SQuote(vs) ->
            let ign,s = Stackp.pop s in (s,(vs @ (ign::exps)),n)
        | _ -> invalid_arg "dip: expects a quotation."

  (** Type: 'A bool 'a 'b -> 'B. If the bottom value is true, evaluates
      the middle value, if false, evaluates the top value. *)
  let ifexp (s,exps,n) =
    let test,thenexp,elseexp,s = Stackp.get_three s in
      match test with
          SBool(true)  -> (s,((Types.unpack thenexp) @ exps),n)
        | SBool(false) -> (s,((Types.unpack elseexp) @ exps),n)
        | _ -> invalid_arg "if: expects a boolean below two values."

  (** Pops a quotation off the top of the stack and evaluates it.. *)
  let apply (s,exps,n) =
    let q,s = Stackp.pop s in
      match q with
          SQuote(vs) -> (s,(vs@exps),n)
        | _ -> invalid_arg "apply: requires a quotation."

  (** Consumes two quotations and pushes on a single quotation where the
      contents of the bottom quotation is evaluated first and the top,
      second. *)
  let compose (s,exps,n) =
    match Stackp.get_two s with
        SQuote(xs),SQuote(ys),s ->
          let s = Stackp.push s (SQuote (xs@ys)) in (s,exps,n)
      | _ -> invalid_arg "compose: requires two quotations."

  (** Pops the top value off the stack and prints it to standard out. *)
  let print (s,exps,n) =
    let sval,s = Stackp.pop s in
      print_endline (string_of_svalue sval) ;
      (s,exps,n)

  (** Non-destructive. Prints the contents of the entire stack. *)
  let print_stack (s,exps,n) =
    let str = ref "" in
    let aux stref v =
      stref := (string_of_svalue v) ^ " " ^ !stref
    in Stackp.iter (aux str) s;
       print_endline ("Stack: "^(!str));
       (s,exps,n)

  (** Consumes two values from the stack and pushes a boolean that indicates
      whether they are equal or not. *)
  let eq (s,exps,n) =
    match Stackp.get_two s with
        SNum(a),SNum(b),s ->
          let s = Stackp.push s (SBool (a=/b)) in (s,exps,n)
      | SBool(a),SBool(b),s ->
          let s = Stackp.push s (SBool (a=b)) in (s,exps,n)
      | SString(a),SString(b),s ->
          let s = Stackp.push s (SBool(a=b))in(s,exps,n)
      | SWord(a),SWord(b),s ->
          let s = Stackp.push s (SBool (a=b)) in (s,exps,n)
      | SList(xs),SList(ys),s ->
          let s = Stackp.push s (SBool (xs=ys)) in (s,exps,n)
      | _ ->
          invalid_arg "=: expects two words, lists, strings, booleans, or numbers."

  let build w = SBuiltin w

  (** List consisting of the words and their corresponding functions. *)
  let primitives = [
    "pop",(build "pop"),pop ;
    "drop",(build "drop"),pop ;
    "dup",(build "dup"),dup ;
    "swap",(build "swap"),swap ;
    "dip",(build "dip"),dip ;
    "print",(build "print"),print ;
    "print_stack",(build "print_stack"),print_stack ;
    "if",(build "if"),ifexp ;
    "apply",(build "apply"),apply ;
    "compose",(build "compose"),compose ;
    "=",(build "="),eq ;
  ]

  (** Creates two namespaces, one containing SBuiltin values for all of
      the words this module exports, and the other containing the actual
      OCaml functions. *)
  let create () =
    let vs = Nm.create () in
    let ws = Nm.create () in
    let bind (s,t,w) (vspc,wspc) = (Nm.bind vspc s t),(Nm.bind wspc s w) in
      List.fold_right bind primitives (vs,ws)

end

(** Module containing the words related to arithmetic on numbers. *)
module Arithmetic = struct

  (** Adds the top two numbers on the stack and pushes the result
      onto the top. *)
  let plus (s,exps,n) =
    match Stackp.get_two s with
        SNum(a),SNum(b),s ->
          let s = Stackp.push s (SNum (a+/b)) in (s,exps,n)
      | _ -> invalid_arg "+: expects two numbers."

  (** Subtracts the top two numbers on the stack and pushes the result
      onto the top. Subtracts the top value from the second value. *)
  let minus (s,exps,n) =
    match Stackp.get_two s with
        SNum(a),SNum(b),s ->
          let s = Stackp.push s (SNum (a-/b)) in (s,exps,n)
      | _ -> invalid_arg "-: expects two integers."

  (** Multiplies the top two numbers on the stack and pushes the result
      onto the top. Multiplies the second value by the top value. *)
  let mult (s,exps,n) =
    match Stackp.get_two s with
        SNum(a),SNum(b),s ->
          let s = Stackp.push s (SNum (a*/b)) in (s,exps,n)
      | _ -> invalid_arg "*: expects two integers."

  (** Divides the top two numbers on the stack and pushes the result
      onto the top. Divides the top value by the bottom value. *)
  let div (s,exps,n) =
    try
      match Stackp.get_two s with
          SNum(a),SNum(b),s ->
            let s = Stackp.push s (SNum (a//b)) in (s,exps,n)
        | _ -> invalid_arg "/: expects two integers."
    with Division_by_zero -> error "/: division by zero."

  (** Consumes two values from the stack and pushes a boolean that indicates
      whether the second is less than the top, or not. *)
  let lt (s,exps,n) =
    match Stackp.get_two s with
        SNum(a),SNum(b),s -> let s = Stackp.push s (SBool (a</b)) in
          (s,exps,n)
      | _ -> invalid_arg "<: expects two integers."

  (** Consumes two values from the stack and pushes a boolean that indicates
      whether the second is greater than the top, or not. *)
  let gt (s,exps,n) =
    match Stackp.get_two s with
        SNum(a),SNum(b),s ->
          let s = Stackp.push s (SBool (a>/b)) in (s,exps,n)
      | _ -> invalid_arg ">: expects two integers."

  let build w = SBuiltin w

  (** List consisting of the words and their corresponding functions. *)
  let primitives = [
    "+",(build "+"),plus ;
    "-",(build "-"),minus ;
    "*",(build "*"),mult ;
    "/",(build "/"),div ;
    "<",(build "<"),lt ;
    ">",(build ">"),gt ;
  ]

  (** Creates two namespaces, one containing SBuiltin values for all of
      the words this module exports, and the other containing the actual
      OCaml functions. *)
  let create () =
    let vs = Nm.create () in
    let ws = Nm.create () in
    let bind (s,t,w) (vspc,wspc) = (Nm.bind vspc s t),(Nm.bind wspc s w) in
      List.fold_right bind primitives (vs,ws)

end

(** Creates the runtime data system. The system consists of two namespaces
    which contain bound words and bound primitive functions, and a data
    stack which holds the values pushed on it at runtime. *)
let create_runtime () =
  let kern_vspace,kern_wspace = Kernel.create () in
  let terp_vspace,terp_wspace = Interpreter.create () in
  let arith_vspace,arith_wspace = Arithmetic.create () in
  let list_vspace,list_wspace = Lists.create () in
  let vspace = Nm.extend kern_vspace terp_vspace in
  let wspace = Nm.extend kern_wspace terp_wspace in
  let vspace = Nm.extend vspace      arith_vspace in
  let wspace = Nm.extend wspace      arith_wspace in
  let vspace = Nm.extend vspace      list_vspace in
  let wspace = Nm.extend wspace      list_wspace in
    (Stackp.create ()),vspace,wspace

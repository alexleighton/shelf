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

(** Module containing the runtime of the shelf programming language. *)

open Types
open Evaluate

let version = "0.1.5"

(** Creates the runtime data system. The system consists of two namespaces
    which contain bound words and bound primitive functions, and a data
    stack which holds the values pushed on it at runtime. *)
let create_runtime () = Primitives.create_runtime ()

(** Uses the parsergenerator-created parser to parse a given string into
    a list of values. *)
let parse str =
  let lex = Lexing.from_string str in
  let cmds =
    try
      Parser.toplevel Lexer.token lex
    with
      | Failure("lexing: empty token")
      | Parsing.Parse_error -> raise Parsing.Parse_error
  in
    cmds

(** A runtime error. *)
exception Error of string

(** Raise a runtime error. *)
let runtime_error msg = raise (Error msg)

(** Whether or not we should print the stack. After each evaluation. *)
let print_stack = true

(** Runs the toplevel. *)
let toplevel () =
  print_string ("Shelf Interpreter v"^version^".\nPress ") ;
  print_string (match Sys.os_type with
                    "Unix" | "Cygwin" -> "Ctrl-D"
                  | "Win32" -> "Ctrl-Z"
                  | _ -> "EOF") ;
  print_endline " to exit." ;
  let stack,vspace,wspace = create_runtime () in
  let stack = ref stack in
  let vspace = ref vspace in
  let wspace = ref wspace in
    try
      while true do
        try
          print_string ">>> " ;
          let str = read_line () in
          let lex = Lexing.from_string str in
          let cmds =
            try Parser.toplevel Lexer.token lex
            with
              | Failure msg -> runtime_error "Lexing: empty token."
              | Parsing.Parse_error -> runtime_error "Parser: bad syntax."
          in
          let (newstack,newvspace,newwspace) =
            eval (!stack,cmds,!vspace,!wspace) in
            stack := newstack ;
            vspace := newvspace ;
            wspace := newwspace ;
            if print_stack
            then (ignore (Primitives.Kernel.print_stack (!stack,"thunk","thunk"));())
            else ()
        with
          | Failure msg -> print_endline msg
          | Types.Invalid_argument(msg) -> print_endline msg
          | Types.Error(msg) -> print_endline msg
          | Error msg -> print_endline msg
          | Stackp.Empty -> print_endline "Stack is empty."
          | Nm.Unbound(w) -> print_endline ("Cannot find word '"^w^"'.")
          | Sys.Break -> (print_endline "Interrupted." ; exit 0)
      done
    with
        End_of_file -> print_endline "\nGood bye."


let _ =
  toplevel ()

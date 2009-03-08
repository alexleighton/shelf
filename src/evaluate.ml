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

(** Contains the main eval function. Along with some helper functions. *)

open Types

(** Unpacks a quote and returns its constituent values. If the value passed
    is not a quote, turns it into a list. *)
let unpack = function SQuote(vs) -> vs | v -> v::[]

(** Evaluates the head of the expression list, passing the modified stack,
    namespaces, and expression list to another call to eval. Basically, it
    evaluates all of the expressions in the list, return the stack and both
    namespaces. All of these should be implemented using immutable data
    structures so that if necessary, a history can be kept and
    continuations aren't wasteful to implement. *)
let rec eval (stack,exps,vspace,(wspace: word Nm.namespace)) =
  match exps with
      SQuote(vs)::SWord("list")::rest ->
        let s,_,_ = eval ((Stackp.create ()),vs,vspace,wspace) in
        let stack =
          Stackp.push stack (SList (List.rev (Stackp.list_of_stack s)))
        in eval (stack,rest,vspace,wspace)
    | SWord(s)::rest     ->
        let exps = (unpack (Nm.find vspace s))@rest in
          eval (stack,exps,vspace,wspace)
    | SBuiltin(s)::rest  ->
        let builtin = Nm.find wspace s in
        let stack,exps,vspace  = (builtin (stack,rest,vspace)) in
          eval (stack,exps,vspace,wspace)
    | SDefine(s,v)::rest ->
        let vspace = Nm.bind vspace s v in
          eval (stack,rest,vspace,wspace)
    | value::rest        ->
        let stack = Stackp.push stack value in
          eval (stack,rest,vspace,wspace)
    | []                -> (stack,vspace,wspace)

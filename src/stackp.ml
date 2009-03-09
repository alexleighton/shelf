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

(** Last-in first-out stacks.
    This module implements immutable stacks (LIFOs). *)

(** The type of stacks containing elements of type ['a]. *)
type 'a t = { data : 'a list }

(** Raised when {!Stackp.pop} or {!Stackp.top} is applied to an empty stack. *)
exception Empty

(** Return a new stack, initially empty. *)
let create () = { data = [] }

(** Discard all elements from a stack. *)
let clear s = { data = [] }

(** Returns an exact copy of the given stack. *)
let copy s = { data = s.data }

(** [push x s] adds the element [x] at the top of stack [s]. *)
let push s x = { data = x :: s.data }

(** [pop s] removes and returns the topmost element in stack [s],
    or raises [Empty] if the stack is empty. *)
let pop s =
  match s.data with
      hd::tl -> hd,{ data = tl }
    | [] -> raise Empty

(** [top s] returns the topmost element in stack [s],
    or raises [Empty] if the stack is empty. *)
let top s =
  match s.data with
      hd::_ -> hd
    | [] -> raise Empty

(** Return [true] if the given stack is empty, [false] otherwise. *)
let is_empty s = s.data = []

(** Return the number of elements in a stack. *)
let length s = List.length s.data

(** Returns a new stack whose values are the results of the given function
    applied to the values of the given stack. *)
let map f s = { data = List.map f s.data }

(** [iter f s] applies [f] in turn to all elements of [s],
    from the element at the top of the stack to the element at the
    bottom of the stack. The stack itself is unchanged. *)
let iter f s = List.iter f s.data

(**************************************************)
(*  Helper functions.                             *)

(** Return a list of all values in the stack. *)
let list_of_stack s = List.rev s.data

(** Return the values (bottom,top,stack). *)
let get_two s =
  let second,s = pop s in
  let first,s  = pop s in
    first,second,s

(** Return the values (bottom,middle,top,stack).*)
let get_three s =
  let third,s = pop s in
  let first,second,s = get_two s in
    first,second,third,s

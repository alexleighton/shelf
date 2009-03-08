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

(** Module containing all of the types used in the shelf language. *)

(** The values of the shelf language. *)
type svalue =
    SNum of Num.num
  | SBool of bool
  | SString of string
  | SQuote of svalue list
  | SList of svalue list
  | SWord of string
  | SBuiltin of string
  | SDefine of string * svalue

(** The type of a program. *)
type program = svalue list

(** The type of primitive words, under the covers. *)
type word = svalue Stackp.t * program * svalue Nm.namespace ->
            svalue Stackp.t * program * svalue Nm.namespace

(** Returns a string representation of a value. *)
let rec string_of_svalue v = match v with
    SNum(i)       -> (Num.string_of_num i)
  | SBool(b)      -> if b then "true" else "false"
  | SString(s)    -> "\"" ^ s ^ "\""
  | SWord(s)      -> s
  | SBuiltin(s)   -> s
  | SQuote(vs) -> "["^(String.concat " " (List.map string_of_svalue vs))^"]"
  | SList(vs) -> "("^(String.concat "," (List.map string_of_svalue vs))^")"
  | SDefine(s,v) -> "(def: "^s^", "^(string_of_svalue v)^")"

(** Returns all the values contained in a quote. If not a quote, returns
    the given value in a list. *)
let unpack = function SQuote(vs) -> vs | v -> [v]

exception Invalid_argument of string
exception Error of string

let invalid_arg msg = raise (Invalid_argument msg)
let error msg = raise (Error msg)

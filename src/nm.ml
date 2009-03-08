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

(** A namespace module. Contained in it are the functions for manipulating
    data in a namespace. *)

(** A namespace record. Contains a list of key,value pairs. *)
type 'a namespace = { dict : (string * 'a) list }

(** Unbound exception. Raised when a value is not found
    for a specific key. *)
exception Unbound of string

(** Creates a new namespace with no bindings. *)
let create () = { dict = [] }

(** Returns true if the given namespace is empty. *)
let is_empty n = n.dict = []

(** Returns a new namespace with the indicated binding. *)
let bind n k v = { dict = (k,v) :: n.dict }

(** Returns a new namespace with the indicate binding removed. *)
let remove n k = { dict = (List.remove_assoc k n.dict) }

(** Returns true if a binding contains the given key. False otherwise. *)
let bound n k = List.mem_assoc k n.dict

(** Returns the key,value pair in the namespace given the key.
    It will raise the Not_found exception if there is no binding. *)
let find n k =
  try List.assoc k n.dict
  with Not_found -> raise (Unbound k)

(** Extends namespace a with all of the key,value pairs in b. *)
let extend a b = { dict = (b.dict @ a.dict) }

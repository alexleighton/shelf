{

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

  (** Auto-generated module for lexing a given string into tokens suitable
      for the auto-generated parser. *)
  open Parser
  open Lexing

  let incr_linenum lexbuf =
    let pos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <- { pos with
        pos_lnum = pos.pos_lnum + 1;
        pos_bol = pos.pos_cnum;
                           }
}

let word =
  [':''=''<''>''_''+''-''*''/''a'-'z' 'A'-'Z']
  [':''=''<''>''_''+''-''*''/''a'-'z' 'A'-'Z' '0'-'9' '?']*

let float_number = '-'?['0'-'9']+('.'['0'-'9']*)? | '-'?'.'['0'-'9']+

rule token = parse
    "#" [^'\n']* '\n'   { incr_linenum lexbuf; token lexbuf }
  | '\n'                { incr_linenum lexbuf; token lexbuf }
  | [' ' '\t']          { token lexbuf }
  | '-'?['0'-'9']+      { NUM (Num.num_of_string(lexeme lexbuf)) }
  | float_number        { NUM (Num.num_of_string(lexeme lexbuf)) }
  | "true"              { TRUE }
  | "false"             { FALSE }
  | "def"               { DEFINE }
  | '\"' [^'\"']* '\"'  { let str = lexeme lexbuf in
                         STRING (String.sub str 1 (String.length str - 2)) }
  | '\'' [^'\'']* '\''  { let str = lexeme lexbuf in
                         STRING (String.sub str 1 (String.length str - 2)) }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | '{'                 { LCBRACE }
  | '}'                 { RCBRACE }
  | '['                 { LSBRACE }
  | ']'                 { RSBRACE }
  | word as id          { WORD (id) }
  | eof                 { EOF }

{
}

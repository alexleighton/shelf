%{

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

  (** Auto-generated module for parsing the shelf language. *)
  open Types
  open Num

%}

%token LCBRACE RCBRACE LSBRACE RSBRACE LPAREN RPAREN
%token <string> WORD
%token <string> COMMAND
%token <Num.num> NUM
%token TRUE FALSE
%token DEFINE
%token <string> STRING
%token EOF

%start toplevel
%type <Types.svalue list > toplevel

%%

toplevel:
  | exprs EOF             { $1 }

exprs:
  |                       { [] }              /* Thanks kaustuv. */
  | expr exprs            { $1 :: $2 }

expr:
  | vals                  { $1 }

vals:
  | DEFINE WORD expr      { SDefine ($2,$3) }
  | NUM                   { SNum $1 }
  | STRING                { SString $1 }
  | TRUE                  { SBool true }
  | FALSE                 { SBool false }
  | WORD                  { SWord $1 }
  | LSBRACE exprs RSBRACE { SQuote $2 }
  | LCBRACE exprs RCBRACE { SQuote $2 }
  | LPAREN exprs RPAREN   { SList $2 }

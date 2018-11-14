(*
  sast.mli
  Author: Matthew Chan
*)

open Ast

type sexpr =
  SNoexpr
| SBinop of sexpr * operator * sexpr * typ
| SUop of uoperator * sexpr * typ
| SIntLit of int
| SStringLit of string
| SArrLit of sexpr list
| SId of string
| SFuncCall of string * sexpr list * typ
| SArrAccess of string * sexpr  * typ
| SField of string * string * typ

type sstmt = 
  SNostmt
| SBlock of sstmt list
| SExpr of sexpr * typ
| SVarDecl of typ * string
| SVarDeclAsn of typ * string * sexpr
| SAsn of string * sexpr
| SReturn of sexpr * typ
| SPipeStmt of sexpr
| SFor of  sstmt * sexpr * sstmt * sstmt
| SForeach of sexpr * sexpr * sstmt
| SIf of sexpr * sstmt * sstmt
| SElif of sexpr list * sstmt list

(* Functions have a return type, name, argument list, and body of statements *)
type sfdecl = {
	styp: typ;
	sfname: string;
	sparams: param list;
	sbody: sstmt list;
}

(* Program is composed of functions, statements and imports *)
type sprogram = {
	sfuncs: fdecl list;
	sstmts: sstmt list;
	simports: import list;
}

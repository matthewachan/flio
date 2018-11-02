(*
  ast.mli
  Author: Matthew Chan
*)

type operator = Add | Sub | Mul | Div | Gt | Lt | Eq | Neq | And | Or | Pipe

type uoperator = Neg | Not

type typ = Int | String | File | Dir | Array of typ * int | Void

(* Statements can be expressions or local var declarations *)
type param = Parameter of typ * string

(* Expressions are assignment and basic operations *)
type expr =
  Noexpr
| Binop of expr * operator * expr
| Uop of uoperator * expr
| IntLit of int
| StringLit of string
| ArrLit of expr list
| Id of string
| FuncCall of string * expr list
| ArrAccess of string * expr
| Field of string * string

type stmt = 
  Nostmt
| Block of stmt list
| Expr of expr
| VarDecl of typ * string
| VarDeclAsn of typ * string * expr
| Asn of string * expr
| Return of expr
| PipeStmt of expr
| For of  stmt * expr * stmt * stmt
| Foreach of expr * expr * stmt
| If of expr * stmt * stmt
| Elif of expr list * stmt list

type import =
  Import of string

(* Functions have a return type, name, argument list, and body of statements *)
type fdecl = {
	typ: typ;
	fname: string;
	params: param list;
	body: stmt list;
}

(* Program is composed of functions, statements and imports *)
type program = {
	funcs: fdecl list;
	stmts: stmt list;
	imports: import list;
}

type operator = Add | Sub | Mul | Div | Gt | Lt | Eq

type typ = Int | String

(* type vdecl = VarDecl of typ * string *)

(* Expressions are assignment and basic operations *)
type expr =
  Noexpr
| Binop of expr * operator * expr
| Lit of int
| Asn of string * expr
| Id of string

(* Statements can be expressions or local var declarations 
type formal = Formal of typ * string
*)
type stmt = 
  Block of stmt list
| Expr of expr
| VarDecl of typ * string
| VarDeclAsn of typ * string * expr
| Return of expr
| For of  expr * expr * expr * stmt
| If of expr * stmt * stmt

(* Funcs have a type, name, argument list, and body of statements 
type fdecl = {
	typ: typ;
	fname: string;
	formals: formal list;
	body: stmt list;
}
*)
(* Program is composed of functions and statements *)
type program = {
	stmts: stmt list;
}

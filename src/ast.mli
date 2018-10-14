type operator = Add | Sub | Mul | Div | Gt | Lt | Eq | Neq | And | Or

type uoperator = Neg | Not


type typ = Int | String | File | Dir | Array of typ * int | Void



(* type vdecl = VarDecl of typ * string *)

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
| Asn of string * expr
| Id of string
| FuncCall of string * expr list
| ArrAccess of string * expr

type stmt = 
  Block of stmt list
| Expr of expr
| VarDecl of typ * string
| VarDeclAsn of typ * string * expr
| Return of expr
| For of  expr * expr * expr * stmt
| Foreach of expr * expr * stmt
| If of expr * stmt * stmt

(* Funcs have a type, name, argument list, and body of statements *)
type fdecl = {
	typ: typ;
	fname: string;
	params: param list;
	body: stmt list;
}

(* Program is composed of functions and statements *)
type program = {
	funcs: fdecl list;
	stmts: stmt list;
}

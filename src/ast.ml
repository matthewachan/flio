(*
  ast.mli
  Author: Matthew Chan
*)

type operator = Add | Sub | Mul | Div | Gt | Lt | Eq | Neq | And | Or

type uoperator = Neg | Not

type typ = Int | String | File | Dir | Void

(* Statements can be expressions or local var declarations *)
type param = typ * string

(* Expressions are assignment and basic operations *)
type expr =
  Noexpr
| Binop of expr * operator * expr
| Uop of uoperator * expr
| IntLit of int
| StringLit of string
| Id of string
| FuncCall of string * expr list

type stmt = 
  Nostmt
| Block of stmt list
| Expr of expr
| VarDecl of typ * string
| VarDeclAsn of typ * string * expr
| Asn of string * expr
| Return of expr
| For of  stmt * expr * stmt * stmt
| If of expr * stmt * stmt


(* Functions have a return type, name, argument list, and body of statements *)
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

(* Pretty printing functions *)
let string_of_op = function
  Add -> "+"
| Sub -> "-"
| Mul -> "*"
| Div -> "/"
| Eq -> "=="
| Neq -> "!="
| Lt -> "<"
| Gt -> ">"
| And -> "and"
| Or -> "or"

let string_of_uop = function
  Neg -> "-"
| Not -> "!"

let string_of_typ = function
  Int -> "int"
| Void -> "void"
| String -> "string"
| File -> "file"
| Dir -> "dir"

let rec string_of_expr = function
  IntLit(l) -> string_of_int l
| StringLit(l) -> l
| Id(s) -> s
| Binop(e1, op, e2) -> let lhs = string_of_expr e1 and rhs = string_of_expr e2 in
	(lhs ^ " " ^ string_of_op op ^ " " ^ rhs)
| Uop(op, e) -> string_of_uop op ^ string_of_expr e
| FuncCall(f, args) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
| Noexpr -> ""

let rec string_of_stmt = function
  Block(stmts) ->
	"{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
| Expr(expr) -> string_of_expr expr ^ ";\n";
| VarDecl(t, id) -> (string_of_typ t) ^ " " ^ id ^ ";\n"
| VarDeclAsn(t, id, e) -> (string_of_typ t) ^ " " ^ id ^ " = " ^ (string_of_expr e) ^ ";\n"
| Asn(id, e) -> id ^ " = " ^ (string_of_expr e) ^ ";\n"
| Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
| For(s1, e, s2, s3) ->
      "for (" ^ string_of_stmt s1  ^ " ; " ^ string_of_expr e ^ " ; " ^
      string_of_stmt s2  ^ ";) " ^ string_of_stmt s3
| If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
| If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
        string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
| Nostmt -> ""

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.params) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"


let string_of_program program =
  String.concat "\n" (List.map string_of_fdecl program.funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_stmt program.stmts)

(*
  ast.ml
  Author: Matthew Chan
  Author: Justin Gross
*)

type operator = Add | Sub | Mul | Div | Gt | Lt | Eq | Neq | And | Or

type uoperator = Neg | Not

type typ = Int | String | File | Dir | Array of typ * int | Void

(* Statements can be expressions or local var declarations *)
type param = typ * string

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
| PipeStmt of expr list
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
        locals: param list; 
	params: param list;
	body: stmt list;
}

(* Program is composed of functions, statements and imports *)
type program = {
	funcs: fdecl list;
	stmts: stmt list;
	imports: import list;
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
(* | Pipe -> "|>" *)

 let string_of_uop = function 
   Neg -> "-" 
 | Not -> "!" 

 let rec string_of_typ = function 
   Int -> "int" 
 | Void -> "void" 
 | String -> "string" 
 | File -> "file" 
 | Dir -> "dir" 
 | Array(t, size) -> let t1 = string_of_typ t 
 	in t1 ^ "[" ^ string_of_int size ^ "]" 

 let rec string_of_expr = function 
   IntLit(l) -> string_of_int l 
 | StringLit(l) -> l 
 | Id(s) -> s 
 | Binop(e1, op, e2) -> let lhs = string_of_expr e1 and rhs = string_of_expr e2 in 
 	(lhs ^ " " ^ string_of_op op ^ " " ^ rhs) 
 | Uop(op, e) -> string_of_uop op ^ string_of_expr e 
 | ArrLit(e) -> "{" ^ String.concat ", " (List.map string_of_expr e) ^ "}" 
 | ArrAccess(id, e) -> let idx = string_of_expr e in 
 	(id ^ "[" ^ idx ^ "]") 
 | Field(id, field) -> id ^ "." ^ field 
 | FuncCall(f, args) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")" 
 | Noexpr -> "" 

(* Everything complete except for Elif statement with Else  I believe the code below that is commented out works, but I'm not sure*)

 let rec string_of_stmt = function 
   Block(stmts) -> 
 	"{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n" 
 | Expr(expr) -> string_of_expr expr ^ ";\n"; 
 | VarDecl(t, id) -> (string_of_typ t) ^ " " ^ id ^ ";\n" 
 | VarDeclAsn(t, id, e) -> (string_of_typ t) ^ " " ^ id ^ " = " ^ (string_of_expr e) ^ ";\n" 
 | Asn(id, e) -> id ^ " = " ^ (string_of_expr e) ^ ";\n" 
 | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n" 
 | PipeStmt(exprs) -> String.concat " |> " (List.map string_of_expr exprs) ^ ";\n" 
 | Foreach(elem, lst, stmt) -> "foreach " ^ string_of_expr elem ^ " in " ^ string_of_expr lst ^ string_of_stmt stmt
 | For(s1, e, s2, s3) -> "for (" ^ string_of_stmt s1 ^ " " ^ string_of_expr e ^ " ; " ^
         string_of_stmt s2 ^ ") " ^ string_of_stmt s3


| If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
| If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
	string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
| Elif(exprs, stmts) ->  "if (" ^ string_of_expr (List.hd exprs) ^ ")\n" ^
	string_of_stmt (List.hd stmts)
	^ String.concat "" (List.map2 (fun e s -> "elif (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s) (List.tl exprs) (List.tl (List.rev 		(List.tl (List.rev stmts))))) 
	^ "else\n" ^ string_of_stmt (List.hd (List.rev stmts))
(*| Elif(exprs, stmts) ->  "if (" ^ string_of_expr (List.hd exprs) ^ ")\n" ^
	string_of_stmt (List.hd stmts)
	^ String.concat "" (List.map2 (fun e s -> "elif (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s) (List.tl exprs) (List.tl stmts))
*)
| Nostmt -> ""

 let string_of_import = function
    Import(util)-> "import " ^ util 


let string_of_fdecl fdecl =
  "def " ^ string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.params) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (imports, funcs, stmts) =
  String.concat "\n" (List.map string_of_import imports) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_stmt stmts) 
 

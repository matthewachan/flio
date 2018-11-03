open Ast
open Sast

module StringMap = Map.Make (String)

let check ast = 
 List.iter (fun n -> print_endline n) ast.stmts


(*
type env = {
  env_fmap: fdecl StringMap.t;
  env_fname: string;
  env_return_type: datatype;
  env_globals: datatype StringMap.t;
  env_flocals: datatype StringMap.t;
  env_in_loop: bool;
  env_set_return: bool;
  env_sfmap: sfdecl StringMap.t;
}

let rec expr_to_sexpr expr env =
  match expr with
    IntLit(d)			-> (SIntLit(d, Datatype(Int)), env)
  | StringLit(s)		-> (SStringLit(s, Datatype(String)), env)
  | Id(s)			-> (check_scope s env, env)
  | Noexpr			-> (SNoexpr, env)
  | Binop(e1, op, e2)		-> (check_binop e1 op e2 env)
  (* built-in functions *)
  | Call(s, e_l)              -> (check_call s e_l env)
  (* list functionality *)
  | List(e_l)                 -> (check_list e_l env)
  | ListAccess(s, e)          -> (check_access s e env)
  | ListSlice(s, e1, e2)      -> (check_slice s e1 e2 env) (* returns func call *)

and sexpr_to_type = function
    SIntLit(_, typ)           -> typ
  | SFloatLit(_, typ)         -> typ
  | SStrLit(_, typ)           -> typ
  | SBoolLit(_, typ)          -> typ
  | SId(_, typ)               -> typ
  | SBinop(_, _, _, typ)      -> typ
  | SUnop(_, _, typ)          -> typ
  | SCall(_, _, typ)          -> typ
  | SNoexpr                   -> Datatype(Void)
  (* list functionality *)
  | SList(_, typ)             -> typ
  | SListAccess(_, _, typ)    -> typ
  *)

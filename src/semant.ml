(* @TODO:
        * Check imports
        * Uop, Nostmt, Foreach, Elif, Pipestmt*)
open Ast
open Sast

module StringMap = Map.Make (String)

let check ast = 

        (* Raise an exception if the given list has a duplicate *)
        let report_duplicate exceptf list =
                let rec helper = function
                        n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
                in helper (List.sort compare list)
                in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
          (Void, n) -> raise (Failure (exceptf n))
      | _ -> ()
        in

  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
          if lvaluet == rvaluet then lvaluet else raise err
  in
  (**** Checking Functions ****)

  (* Print function cannot be redefined *)
  if List.mem "print" (List.map (fun fd -> fd.fname) ast.funcs)
  then raise (Failure ("function print may not be defined")) else ();


  (* Duplicate function names not permitted *)
  report_duplicate (fun n -> "duplicate function " ^ n)
  (List.map (fun fd -> fd.fname) ast.funcs);

  (* Keep track of function declarations *)

  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
  StringMap.empty ast.funcs 
  in

  let function_decl s = try StringMap.find s function_decls
  with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in


    let type_of_identifier s map =
            try
                    StringMap.find s map 
  with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

    let rec expr map = function
            IntLit _ -> Int
      | StringLit _ -> String
      | ArrLit a -> 
                      Array(expr map (List.hd a), List.length a)
      | Id s -> type_of_identifier s map
      | Noexpr -> Void
      | Uop(op, e) as ex -> let t = expr map e in
	 (match op with
	   Neg when t = Int -> Int
	 | Not when t = Int -> Int
         | _ -> raise (Failure ("illegal unary operator ")))
      | ArrAccess(n, idx) -> type_of_identifier n map
      | FuncCall(f, args) -> let fd = function_decl f in 
      if List.length args != List.length fd.params then
              raise (Failure ("Mismatched number of arguments"))
  else
          List.iter2 (fun (ft, _) e -> let et = expr map e in
          ignore (check_assign ft et (Failure ("Illegal argument")))
          ) fd.params args;
           fd.typ
      | Binop(e1, op, e2) as e -> let t1 = expr map e1 and t2 = expr map e2 in
      (match op with
          Add | Sub | Mul | Div when t1 = Int && t2 = Int -> Int
      | Eq | Neq when t1 = t2 -> Int
        | Lt | Gt when t1 = Int && t2 = Int -> Int
        | And | Or when t1 = Int && t2 = Int -> Int
        | Pipe when t1 = File && t2 = File -> File 
        | _ -> raise (Failure ("illegal binary operator " ))
        (* ^ *)
      (*       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^ *)
      (*       string_of_typ t2 ^ " in " ^ string_of_expr e)) *)
      )

    in 
    let check_bool_expr map e = if expr map e != Int
      then raise (Failure ("expected Boolean expression in " ))
      (* ^ string_of_expr e)) *)
     else ()
    in
  let check_stmt s =

          (* Type of each variable (global, formal, or local *)
          let symbols = StringMap.empty

    in

    (* Verify a statement or throw an exception *)
    let rec stmt map = function
            Block sl -> let rec check_block m = function
                    [Return _ as s] -> stmt m s
        | Return _ :: _ -> raise (Failure "nothing may follow a return")
        | Block sl :: ss -> check_block m (sl @ ss)
        | s :: ss -> check_block (stmt m s) ss
        | [] -> m
          in check_block map sl
        | VarDecl(t, n) as var -> (StringMap.add n t map)
        | VarDeclAsn(t, n ,e) -> (StringMap.add n t map)
        | Asn(n, e) -> let lt = type_of_identifier n map 
            and rt = expr map e in 
            ignore(check_assign lt rt (Failure ("illegal assignment"))); map
      | Expr e -> ignore(expr map e) ; map
      | Return e -> raise (Failure ("Returns not allowed outside of function scope"))
      | For(s1, e, s2, s3) -> ignore(stmt (stmt (stmt map s1) s2) s3) ; map
      | If(e, s1, s2) -> check_bool_expr map e; ignore(stmt map s1); ignore(stmt map s2); map 
      | Nostmt -> map
      | Foreach(e1, e2, s) -> ignore(stmt map s); map
      | Elif(exprs, stmts) -> let check_e e = ignore(expr map e) and check_s s = ignore(stmt map s) in
        List.iter check_e exprs; List.iter check_s stmts; map



        in
    ignore(stmt symbols (Block s))
    in

  let check_function func =

          (* Params cannot have void type *)
          List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.params;

    (* Params cannot have duplicate names *)
    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
    (List.map snd func.params);

    (* Type of each variable (global, formal, or local *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
    StringMap.empty (func.params)
  in

    (* Verify a statement or throw an exception *)
    let rec stmt map = function
            Block sl -> let rec check_block m = function
                    [Return _ as s] -> stmt m s
      | Return _ :: _ -> raise (Failure "nothing may follow a return")
      | Block sl :: ss -> check_block m (sl @ ss)
      | s :: ss -> check_block (stmt m s) ss
        | [] -> m
    in check_block map sl
        | VarDecl(t, n) as var -> (StringMap.add n t map)
        | VarDeclAsn(t, n ,e) -> (StringMap.add n t map)
        | Asn(n, e) -> let lt = type_of_identifier n map and rt = expr map e
            in ignore(check_assign lt rt (Failure ("illegal assignment"))); map
      | Expr e -> ignore(expr map e) ; map
      | Return e -> let t = expr map e in
      if 
              t = func.typ then map
    else
            raise (Failure ("Return type mismatch"))
      | For(s1, e, s2, s3) -> ignore(stmt (stmt (stmt map s1) s2) s3) ; map
      | If(e, s1, s2) -> check_bool_expr map e; ignore(stmt map s1); ignore(stmt map s2); map 
      | Nostmt -> map
      | Foreach(e1, e2, s) -> ignore(stmt map s); map
      | Elif(exprs, stmts) -> let check_e e = ignore(expr map e) and check_s s = ignore(stmt map s) in
        List.iter check_e exprs; List.iter check_s stmts; map



      in
    ignore(stmt symbols (Block func.body))

    in List.iter check_function ast.funcs ; check_stmt ast.stmts

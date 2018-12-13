open Ast

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


        (* Print function cannot be redefined *)
        if List.mem "print" (List.map (fun fd -> fd.fname) ast.funcs)
        then raise (Failure ("function print may not be defined")) else ();


        (* Duplicate function names not permitted *)
        report_duplicate (fun n -> "duplicate function " ^ n)
        (List.map (fun fd -> fd.fname) ast.funcs);

        let built_in_decls =  StringMap.add "print"
                { typ = Void; fname = "print"; params = [(Int, "x")];
                body = [] } (StringMap.add "prints" { typ = Void; fname = "prints"; params = [(String, "x")];
                body = []} (StringMap.add "fopen" { typ = File; fname = "fopen"; params = [(String, "f")];
                body = []} (StringMap.add "delete" { typ = Int; fname = "delete"; params = [(String, "x")];
                body = []} (StringMap.add "copy" { typ = Int; fname = "copy"; params = [(String, "src") ; (String, "dest")];
                body = []} (StringMap.add "move" { typ = Int; fname = "move"; params = [(String, "src") ; (String, "dest")];
                body = []} (StringMap.add "write" { typ = Int; fname = "write"; params = [(File, "f") ; (String, "buf")];
                body = []} (StringMap.add "read" { typ = String; fname = "read"; params = [(File, "f") ; (Int, "length")];
                body = []} (StringMap.add "readLine" { typ = String; fname = "readLine"; params = [(File, "f")];
                body = []} (StringMap.add "appendString" { typ = Int; fname = "readLine"; params = [(String, "f") ; (String, "buf")];
                body = []} (StringMap.add "dopen" { typ = Dir; fname = "dopen"; params = [(String, "d")];
                body = []} (StringMap.add "rmdir" { typ = Int; fname = "rmdir"; params = [(String, "d")];
                body = []} StringMap.empty)))))))))))
        in
     
        (* Keep track of function declarations *)
        let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
        built_in_decls ast.funcs
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
                | ArrLit a -> Array(expr map (List.hd a), List.length a)
                | Id s -> type_of_identifier s map
                | Noexpr -> Void
                | Uop(op, e) as ex -> let t = expr map e in
                        (match op with
                          Neg when t = Int -> Int
                        | Not when t = Int -> Int
                        | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex))
                        )
                | ArrAccess(n, _) -> type_of_identifier n map
                | FuncCall(f, args) as call -> let fd = function_decl f in 
                        if List.length args != List.length fd.params then
                                raise (Failure ("expecting " ^ string_of_int
                                (List.length fd.params) ^ " arguments in " ^ string_of_expr call))
                        else
                                List.iter2 (fun (ft, _) e -> let et = expr map e in
                                ignore (check_assign ft et (Failure ("illegal actual argument found " ^
                                string_of_typ et ^ " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
                                fd.params args ; fd.typ
                | Binop(e1, op, e2) as e -> let t1 = expr map e1 and t2 = expr map e2 in
                        (match op with
                          Add | Sub | Mul | Div when t1 = Int && t2 = Int -> Int
                        | Eq | Neq when t1 = t2 -> Int
                        | Lt | Gt when t1 = Int && t2 = Int -> Int
                        | And | Or when t1 = Int && t2 = Int -> Int
                        | Pipe when t1 = File && t2 = File -> File 
                        | _ -> raise (Failure ("illegal binary operator " ^ 
                                string_of_typ t1 ^ " " ^ string_of_op op ^
                                " " ^ string_of_typ t2 ^ " in " ^ string_of_expr e))
                        )

        in 

        let check_bool_expr map e = if expr map e != Int
                then raise (Failure ("expected Boolean expression in "
                        ^ string_of_expr e))
                else ()
        in

        (**** Check Statements ****)
        let check_stmt s =

                (* Type of each variable (global, formal, or local *)
                let symbols = StringMap.empty

                in

                let rec stmt map = function
                          Block sl -> let rec check_block m = function
                                  [Return _ as s] -> stmt m s
                                | Return _ :: _ -> raise (Failure "nothing may follow a return")
                                | Block sl :: ss -> check_block m (sl @ ss)
                                | s :: ss -> check_block (stmt m s) ss
                                | [] -> m
                                in check_block map sl
                        | VarDecl(t, n) -> (StringMap.add n t map)
                        | VarDeclAsn(t, n ,e) -> ignore(expr map e); (StringMap.add n t map)
                        | Asn(n, e) as ex -> let lt = type_of_identifier n map and rt = expr map e in 
                                ignore(check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
                                " = " ^ string_of_typ rt ^ " in " ^ string_of_stmt ex))) ; map
                        | Expr e -> ignore(expr map e) ; map
                        | Return e -> ignore(expr map e) ; raise (Failure ("returns not allowed outside of function scope"))
                        | For(s1, e, s2, s3) -> let m = stmt map s1 in
                                ignore(expr m e); ignore(stmt (stmt m s2) s3) ; map
                        | If(e, s1, s2) -> check_bool_expr map e; ignore(stmt map s1); ignore(stmt map s2); map 
                        | Nostmt -> map
                        | Foreach(_, e, s) -> ignore(expr map e); ignore(stmt map s); map
                        | Elif(exprs, stmts) -> let check_e e = ignore(expr map e) and check_s s = ignore(stmt map s) in
                                List.iter check_e exprs; List.iter check_s stmts; map
                in ignore(stmt symbols (Block s))

        in
        
        (**** Check Functions ****)
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
                        | VarDecl(t, n) -> (StringMap.add n t map)
                        | VarDeclAsn(t, n , _) -> (StringMap.add n t map)
                        | Asn(n, e) as ex -> let lt = type_of_identifier n map and rt = expr map e in 
                                ignore(check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
                                " = " ^ string_of_typ rt ^ " in " ^ string_of_stmt ex))) ; map
                        | Expr e -> ignore(expr map e) ; map
                        | Return e -> let t = expr map e in
                                if t = func.typ then map
                                else raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                                        string_of_typ func.typ ^ " in " ^ string_of_expr e))
                        | For(s1, e, s2, s3) -> ignore(expr map e); ignore(stmt (stmt (stmt map s1) s2) s3) ; map
                        | If(e, s1, s2) -> check_bool_expr map e; ignore(stmt map s1); ignore(stmt map s2); map 
                        | Nostmt -> map
                        | Foreach(e1, e2, s) -> ignore(expr map e1); ignore(expr map e2); ignore(stmt map s); map
                        | Elif(exprs, stmts) -> let check_e e = ignore(expr map e) and check_s s = ignore(stmt map s) in
                List.iter check_e exprs; List.iter check_s stmts; map

                in ignore(stmt symbols (Block func.body))

        in List.iter check_function ast.funcs ; check_stmt ast.stmts

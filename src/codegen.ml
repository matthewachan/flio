module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate program =
  
  let context = L.global_context () in
    let the_module = L.create_module context "flio"
    and i32_t = L.i32_type context
    and i8_t = L.i8_type context
    and str_ptr_t = L.pointer_type (L.i8_type context) 
    and str_array = L.struct_type context [|L.array_type (L.pointer_type (L.i8_type context)) 100; L.i32_type context|]
    and void_t = L.void_type context 
  in       

  (* Pattern match AST types to LLVM types *)
   let rec ltype_of_typ = function 
         A.Int -> i32_t 
       | A.String -> str_ptr_t 
       | A.Void -> void_t 
       | A.File -> L.struct_type context [|str_ptr_t; i32_t; str_array |]  
       | A.Dir -> L.struct_type context [|str_ptr_t; str_array; str_array; str_array|]
       | A.Array (typ, size) -> L.struct_type context [|L.pointer_type (ltype_of_typ typ); i32_t|]

   in 

   let statements = 
           let statement_items = List.filter (fun x-> match x with 
           A.Stmt (x) -> true
          | _-> false) program
           in List.map (fun x -> match x with
                A.Stmt (x) -> x
              | _ -> raise( Failure("Conversion failed"))) statement_items

    in

   (* extract all the global variables from the statements *)
   let globals = 
           let global_list = List.filter (fun x -> match x with
           A.VarDecl(_, _) -> true
         | A.VarDeclAsn(_, _, _) -> true
         | _ -> false) statements
           in List.map (fun x-> match x with
           A.VarDecl(x, name) -> (x, name)
         | A.VarDeclAsn(x, name, _) -> (x, name)
         | _ -> raise(Failure("Invalid variable type"))) global_list

   in

   let not_globals = List.filter (fun x -> match x with
        A.VarDecl(x, _) -> false
       | _ -> true) statements in

   let functions = 
           let fdecl_main = A.Function({
                   typ = A.Int;
                   fname = "main";
                   params = [];
                   body = List.rev(A.Return(A.IntLit(0))::List.rev(not_globals))
                 })
           in let other_functions = List.filter (fun x -> match x with
                A.Function(x) -> true
              | _ -> false) program
           in 
           let all_functions = fdecl_main:: other_functions in List.map
           (fun x -> match x with 
                A.Function(x) -> x
               | _ -> raise(Failure("Invalid function passed in"))) all_functions
   in 

   (*Initialize all the global variables and add them to a string map*)
   let global_vars =
            let global_var m (t, n) =
                    let init = match t with
                    _ -> L.const_int (ltype_of_typ t) 0
             in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals 
    in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can call it even before we've created its body *)

 let function_decls =
        let function_decl m fdecl =
                let name = fdecl.A.fname 
                and formal_types = Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.A.params)
                in let ftype = 
                        L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
          StringMap.add name (L.define_function name ftype the_module, fdecl) m in
  List.fold_left function_decl StringMap.empty functions in      

 let build_function_body fdecl =
          let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
          let builder = L.builder_at_end context (L.entry_block the_function)  in
          

(* Declare main function *)
(* let main_t = L.function_type i32_t [| |] in
  let main_func = L.define_function "main" main_t the_module in

  let builder = L.builder_at_end context (L.entry_block main_func) in*)
  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

  let lookup n = try StringMap.find n global_vars 
                 with Not_found -> raise (Failure("The variable does not exist")) 
  in

  let rec int_range = function
          0 -> []
         | 1 -> [ 0]
         | n -> int_range (n-1) @ [n-1] in 

  (* Build statments inside of the main function *) 
(*  let build_stmts s =*)
    (* Construct code for an expression; return its value *)

    let add_terminal builder f =
            match L.block_terminator (L.insertion_block builder) with 
            Some _ -> ()
           | None -> ignore (f builder) in 

    let rec expr builder = function
        A.IntLit i -> L.const_int i32_t i
      | A.Noexpr -> L.const_int i32_t 0
      | A.FuncCall ("print", [e]) -> 
                      L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
      | A.Binop (e1, op, e2) -> let e1' = expr builder e1
                                and e2' = expr builder e2 in
                                (match op with
                                A.Add -> L.build_add
                              | A.Sub -> L.build_sub
                              | A.Mul -> L.build_mul
                              | A.Div -> L.build_sdiv
                              | A.Gt -> L.build_icmp L.Icmp.Sgt
                              | A.Lt -> L.build_icmp L.Icmp.Slt
                              | A.Eq -> L.build_icmp L.Icmp.Eq
                              | A.Neq -> L.build_icmp L.Icmp.Ne
                              | A.And -> L.build_and
                              | A.Or -> L.build_or
                             ) e1' e2' "tmp" builder
      | A.Uop (op, e) -> let e' = expr builder e in
                        (match op with 
                                A.Neg -> L.build_neg
                              | A.Not -> L.build_not) e' "tmp" builder
      | A.StringLit s -> L.build_global_stringptr s "string" builder
      | A.FuncCall(f, args) -> 
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (expr builder) (List.rev args)) in
        let result = (match fdecl.A.typ with
                A.Void -> ""
               | _ -> f  ^"_result") in
        L.build_call fdef (Array.of_list llargs) result builder
        (*raise (Failure ("not implemented yet"))*)
      | A.ArrAccess (id, exp) -> (*L.build_load (L.build_gep (lookup id) [|(expr builder exp); (L.const_int i32_t 0)|] s builder) s builder*) raise(Failure("not implemented"))
      | A.ArrLit (l) -> 
          let size = L.const_int i32_t (List.length l) in
          let all = List.map (fun e -> expr builder e) l in 
          let new_array = L.build_array_malloc (L.type_of (List.hd all)) size "tmp" builder in
          List.iter (fun x -> let more = (L.build_gep new_array [|L.const_int i32_t x|] "tmp2" builder) in
          let inter = List.nth all x in ignore (L.build_store inter more builder)) (int_range (List.length l));

          let type_of_new_lit = L.struct_type context [|L.pointer_type (L.type_of (List.hd all)); i32_t|] in
          let new_lit = L.build_malloc type_of_new_lit "arr_literal" builder in
          let first = L.build_struct_gep new_lit 0 "first" builder in
          let second = L.build_struct_gep new_lit 1 "second" builder in
          ignore(L.build_store size first builder);
          ignore(L.build_store new_array second builder);
          let actual_literal = L.build_load new_lit "actual_arr_literal" builder 
          in actual_literal;
       | A.Id s -> L.build_load (lookup s) s builder 
 
    in  
  (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
        A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Nostmt -> builder
      | A.For (e1, e2, e3, body) -> 
      (*  let loop predicate body =
                let pred_bb = L.append_block context "while" the_function in
                ignore (L.build_br pred_bb builder);

                let body_bb = L.append_block context "while_body" the_function in
                add_terminal (stmt (L.builder_at_end context body_bb) body) (L.build_br pred_bb);
                
                let pred_builder = L.builder_at_end context pred_bb in
                let bool_val = expr pred_builder predicate in
                
                let merge_bb = L.append_block context "merge" the_function in
                ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
                L.builder_at_end context merge_bb

    in stmt builder
                (A.Block [(stmt builder e1);
                        loop (e2, A.Block [body ;
                                                (stmt builder e3)]) ] )*)
                
       raise (Failure ("not implemented yet")) 
      | A.Foreach (_, _, _) -> raise (Failure ("not implemented yet"))
      | A.If (predicate, then_stmt, else_stmt) -> 
        let bool_val = expr builder predicate in
        let merge_bb = L.append_block context "merge" the_function in
        let b_br_merge = L.build_br merge_bb in
        let then_bb = L.append_block context "then" the_function in
        add_terminal(stmt (L.builder_at_end context then_bb) then_stmt)
        b_br_merge;

        let else_bb = L.append_block context "else" the_function in
        add_terminal (stmt (L.builder_at_end context else_bb) else_stmt) b_br_merge;
        ignore(L.build_cond_br bool_val then_bb else_bb builder);

        L.builder_at_end context merge_bb
       (*raise(Failure ("not implemented yet"))*)
      | A.Elif (_, _) -> raise (Failure ("not implemented yet"))
      | A.Return e  -> ignore(match fdecl.A.typ with
                A.Void -> L.build_ret_void builder
               | _ -> L.build_ret (expr builder e) builder);
               builder
      (*raise (Failure ("not implemented yet"))*)
      | A.VarDecl (_, _) -> builder
      | A.VarDeclAsn (_, name, expr) -> ignore (stmt builder (A.Asn(name, expr))); builder
      | A.Asn (s, e) ->  let e' = expr builder e in ignore(L.build_store e' (lookup s) builder); builder

    in

    (* Build the code for each statement in the function *)
   let builder =  stmt builder (A.Block fdecl.A.body) in

(*  ignore(build_stmts program.A.stmts) in*)
  (* Add terminal for main function *)
  (*add_terminal builder (L.build_ret (L.const_int i32_t 0));*)

   add_terminal builder (match fdecl.A.typ with 
   A.Void -> L.build_ret_void
  | t -> L.build_ret (L.const_int (ltype_of_typ t)  0)) in 

  List.iter build_function_body functions;  

  the_module

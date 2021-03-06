(* 
 * codegen.ml
 * Author: Matthew Chan
 *)
module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate program =
  
  (* Setup LLVM environment vars *)
  let context = L.global_context () in
    let the_module = L.create_module context "flio"
    and i32_t = L.i32_type context
    and i8_t = L.i8_type context
    and str_ptr_t = L.pointer_type (L.i8_type context)
    and void_t = L.void_type context 
  in       

  (* Pattern match AST types to LLVM types *)
  let ltype_of_typ = function
        A.Int -> i32_t
      | A.String -> str_ptr_t
      | A.Void -> void_t
      | A.File -> str_ptr_t 
      | A.Dir -> str_ptr_t 
  in

  (* Utility function for getting a val from a map, given a key *)
  let lookup n m = StringMap.find n m
  in


  (* Utility function to build a return block *)
  let add_terminal builder f =
    match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder) in

  (* Declare built-in functions *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let concat_t = L.function_type (L.pointer_type i8_t) [| L.pointer_type i8_t ; L.pointer_type i8_t |] in
  let concat_func = L.declare_function "concat" concat_t the_module in

  let intToStr_t = L.function_type (L.pointer_type i8_t) [| i32_t |] in
  let intToStr_func = L.declare_function "intToStr" intToStr_t the_module in

  let strcmp_t = L.function_type i32_t [| L.pointer_type i8_t ; L.pointer_type i8_t |] in
  let strcmp_func = L.declare_function "strcmp" strcmp_t the_module in
  
  let create_t = L.function_type i32_t [| L.pointer_type i8_t |] in
  let create_func = L.declare_function "create" create_t the_module in

  let fopen_t = L.var_arg_function_type (L.pointer_type i8_t) [| L.pointer_type i8_t |] in
  let fopen_func = L.declare_function "fopen" fopen_t the_module in

  let dopen_t = L.function_type (L.pointer_type i8_t) [| L.pointer_type i8_t |] in
  let dopen_func = L.declare_function "opendir" dopen_t the_module in

  let delete_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let delete_func = L.declare_function "remove" delete_t the_module in

  let rmdir_t = L.function_type i32_t [| L.pointer_type i8_t |] in
  let rmdir_func = L.declare_function "rmdir" rmdir_t the_module in

  let copy_t = L.function_type i32_t [| L.pointer_type i8_t ; L.pointer_type i8_t |] in
  let copy_func = L.declare_function "copy" copy_t the_module in

  let move_t = L.function_type i32_t [| L.pointer_type i8_t ; L.pointer_type i8_t |] in
  let move_func = L.declare_function "move" move_t the_module in

  let write_t = L.function_type i32_t [| L.pointer_type i8_t ; L.pointer_type i8_t |] in
  let write_func = L.declare_function "bwrite" write_t the_module in

  let appendstr_t = L.function_type i32_t [| L.pointer_type i8_t ; L.pointer_type i8_t |] in
  let appendstr_func = L.declare_function "appendString" appendstr_t the_module in

  let read_t = L.function_type (L.pointer_type i8_t) [| L.pointer_type i8_t ; i32_t |] in
  let read_func = L.declare_function "bread" read_t the_module in

  let readline_t = L.function_type (L.pointer_type i8_t) [| L.pointer_type i8_t |] in
  let readline_func = L.declare_function "readLine" readline_t the_module in

  (* Build a map of function declarations *)
  let function_decls =
    let function_decl m fdecl = 
      let name = fdecl.A.fname
      and formal_types =
        Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.A.params)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty program.A.funcs in

  (* Default format strings *)
  let int_format_str builder = L.build_global_stringptr "%d\n" "fmt" builder in
  let str_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder in
  let fopen_mode builder = L.build_global_stringptr "r+" "mode" builder in

  (* Construct code for an expression; return its value *)
  let rec expr map builder = function
        A.IntLit i -> L.const_int i32_t i
      | A.Noexpr -> L.const_int i32_t 0
      | A.FuncCall ("print", [e]) -> 
              L.build_call printf_func [| (int_format_str builder) ; 
              (expr map builder e) |] "printf" builder
      | A.FuncCall ("concat", [s1 ; s2]) -> 
              L.build_call concat_func [| (expr map builder s1) ; 
              (expr map builder s2) |] "concat" builder
      | A.FuncCall ("create", [f]) -> 
              L.build_call create_func [| (expr map builder f) |]
              "create" builder
      | A.FuncCall ("intToStr", [e]) -> 
              L.build_call intToStr_func [| (expr map builder e) |] 
              "intToStr" builder
      | A.FuncCall ("strcmp", [s1 ; s2]) -> 
              L.build_call strcmp_func [| (expr map builder s1) ; 
              (expr map builder s2) |] "strcmp" builder
      | A.FuncCall ("fopen", [e]) ->
                     L.build_call fopen_func [| (expr map builder e) ; (fopen_mode builder)|] "fopen" builder
      | A.FuncCall ("dopen", [e]) ->
                      L.build_call dopen_func [| (expr map builder e) |] "dopen" builder
      | A.FuncCall ("delete", [e]) ->
	  L.build_call delete_func [| (expr map builder e) |] "delete" builder
      | A.FuncCall ("rmdir", [e]) ->
	  L.build_call rmdir_func [| (expr map builder e) |] "rmdir" builder
      | A.FuncCall ("copy", [e1 ; e2]) ->
                      L.build_call copy_func [| (expr map builder e1) ; (expr map builder e2)|] "copy" builder
      | A.FuncCall ("write", [e1 ; e2]) ->
                      L.build_call write_func [| (expr map builder e1) ; (expr map builder e2)|] "write" builder
      | A.FuncCall ("appendString", [e1 ; e2]) ->
                      L.build_call appendstr_func [| (expr map builder e1) ; (expr map builder e2)|] "appendString" builder
      | A.FuncCall ("read", [e1 ; e2]) ->
                      L.build_call read_func [| (expr map builder e1) ; (expr map builder e2)|] "read" builder
      | A.FuncCall ("readLine", [e1]) ->
                      L.build_call readline_func [| (expr map builder e1) |] "readLine" builder
      | A.FuncCall ("move", [e1 ; e2]) ->
                      L.build_call move_func [| (expr map builder e1) ; (expr map builder e2)|] "move" builder
      | A.FuncCall ("prints", [e]) -> 
              L.build_call printf_func [| (str_format_str builder); 
              (expr map builder e) |] "printf" builder
      | A.Binop (e1, op, e2) -> 
	  let e1' = expr map builder e1
	  and e2' = expr map builder e2 in
	  (match op with
	    A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mul    -> L.build_mul
          | A.Div     -> L.build_sdiv
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Eq   -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Lt    -> L.build_icmp L.Icmp.Slt
	  | A.Gt -> L.build_icmp L.Icmp.Sgt
	  ) e1' e2' "tmp" builder
      | A.Uop (_, _) -> raise (Failure ("not implemented yet"))
      | A.StringLit s -> 
              L.build_global_stringptr s "strptr" builder
      | A.Id s -> L.build_load (lookup s map) s builder 
      | A.FuncCall(f, args) -> 
                let (fdef, fdecl) = StringMap.find f function_decls in
                let actuals = List.rev (List.map (expr map builder) (List.rev args)) in
                let result = (match fdecl.A.typ with
                  A.Void -> ""
                  | _ -> f ^ "_result") in
                L.build_call fdef (Array.of_list actuals) result builder
    in

  (* Build function bodies *)
  let build_function_body global_map fdecl =
    (* Find the function in our map *)
    let (fdef, _) = StringMap.find fdecl.A.fname function_decls in
    (* Move the builder to the entry point of that function *)
    let builder = L.builder_at_end context (L.entry_block fdef) in


    (* The function's local variables are initially the function args *)
    let add_formal m (t, n) p = L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store p local builder);
      StringMap.add n local m in
    let local_vars = List.fold_left2 add_formal global_map 
      fdecl.A.params (Array.to_list (L.params fdef)) in

    let rec fstmt mb = function
        A.Block sl -> (List.fold_left fstmt mb sl)
      | A.Expr e -> ignore (expr (fst mb) (snd mb) e); mb
      | A.Nostmt -> mb 
      | A.For (s1, e, s2, body) -> 
                (* Construct for basic block *)
                let init_bb = L.append_block context "init" fdef in
                ignore (L.build_br init_bb (snd mb)); 
                
                let pred_bb = L.append_block context "for" fdef in
                ignore(pred_bb);

                let init = fstmt (fst mb, L.builder_at_end context init_bb) s1 in
                add_terminal (snd init) (L.build_br pred_bb);

                (* Construct body basic block, and add s2 at the tail *)
                let body_bb = L.append_block context "for_body" fdef in
                let b = (fstmt (fst mb, L.builder_at_end context body_bb) body) in
                add_terminal (snd (fstmt b s2)) (L.build_br pred_bb);

                (* Do initialization before checking the predicate e *)
                let pred_builder = L.builder_at_end context pred_bb in
                let bool_val = expr (fst init) pred_builder e in

                (* Construct merge basic block *)
                let merge_bb = L.append_block context "merge" fdef in
                ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
                (fst mb, L.builder_at_end context merge_bb)
      | A.If (p, then_stmt, else_stmt) -> 
         let bool_val = expr (fst mb) (snd mb) p in
	 let merge_bb = L.append_block context "merge" fdef in

	 let then_bb = L.append_block context "then" fdef in
	 add_terminal (snd (fstmt (fst mb, L.builder_at_end context then_bb) then_stmt))
	   (L.build_br merge_bb);

	 let else_bb = L.append_block context "else" fdef in
	 add_terminal (snd (fstmt (fst mb, L.builder_at_end context else_bb) else_stmt))
	   (L.build_br merge_bb);

	 ignore (L.build_cond_br bool_val then_bb else_bb (snd mb));
	  (fst mb, L.builder_at_end context merge_bb)
      | A.Return e -> ignore(match fdecl.A.typ with
                  A.Void -> L.build_ret_void (snd mb)
                | _ -> L.build_ret (expr (fst mb) (snd mb) e) (snd mb)); mb
      | A.VarDecl (t, n) -> let init = 
              (match t with
                  A.Int -> L.build_alloca i32_t n (snd mb) 
                | A.String -> L.build_alloca str_ptr_t n (snd mb)
                | A.File -> L.build_alloca str_ptr_t n (snd mb)
                | A.Dir -> L.build_alloca str_ptr_t n (snd mb)
                | A.Void -> L.build_ret_void (snd mb)
              ) in
              ((StringMap.add n init (fst mb)), snd mb)
      | A.VarDeclAsn (t, n, e) -> let init = 
              (match t with
                  A.Int -> L.build_alloca i32_t n (snd mb) 
                | A.String -> L.build_alloca str_ptr_t n (snd mb)
                | A.File -> L.build_alloca str_ptr_t n (snd mb)
                | A.Dir -> raise (Failure ("not implemented yet"))
                | A.Void -> L.build_ret_void (snd mb)
              ) in
              let m = (StringMap.add n init (fst mb)) in
              let e' = expr (m) (snd mb) e in
              ignore(L.build_store e' (lookup n (m)) (snd mb)) ; (m, (snd mb))
      | A.Asn (s, e) -> let e' = expr (fst mb) (snd mb) e in
        ignore(L.build_store e' (lookup s (fst mb)) (snd mb)) ; mb
    in
    
    let builder = (snd (fstmt (local_vars, builder) (A.Block fdecl.A.body))) 
    in
    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  (* Declare main function *)
  let main_t = L.function_type i32_t [| |] in
  let main_func = L.define_function "main" main_t the_module in

  (* Variables in main are the empty set initially *)
  let main_vars = StringMap.empty in 

  (* Init builder inside of main *)
  let builder = L.builder_at_end context (L.entry_block main_func) in

  (* Build statments inside of the main function *) 
  let build_stmts s =

    (* Build the code for the given statement; return the StringMap and builder 
       for the statement's successor *)
    let rec stmt mb = function
        A.Block sl -> (List.fold_left stmt mb sl)
      | A.Expr e -> ignore (expr (fst mb) (snd mb) e); mb
      | A.Nostmt -> mb 
      | A.For (s1, e, s2, body) -> 
                (* Construct for basic block *)
                let init_bb = L.append_block context "init" main_func in
                ignore (L.build_br init_bb (snd mb)); 
                
                let pred_bb = L.append_block context "for" main_func in
                ignore(pred_bb);

                let init = stmt (fst mb, L.builder_at_end context init_bb) s1 in
                add_terminal (snd init) (L.build_br pred_bb);

                (* Construct body basic block, and add s2 at the tail *)
                let body_bb = L.append_block context "for_body" main_func in
                let b = (stmt (fst init, L.builder_at_end context body_bb) body) in
                add_terminal (snd (stmt b s2)) (L.build_br pred_bb);

                (* Do initialization before checking the predicate e *)
                let pred_builder = L.builder_at_end context pred_bb in
                let bool_val = (expr (fst init) pred_builder e) in

                (* Construct merge basic block *)
                let merge_bb = L.append_block context "merge" main_func in
                ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
                (fst mb, L.builder_at_end context merge_bb)
      | A.If (p, then_stmt, else_stmt) -> 
         let bool_val = (expr (fst mb) (snd mb) p) in
	 let merge_bb = L.append_block context "merge" main_func in

	 let then_bb = L.append_block context "then" main_func in
	 add_terminal (snd (stmt (fst mb, L.builder_at_end context then_bb) then_stmt))
	   (L.build_br merge_bb);

	 let else_bb = L.append_block context "else" main_func in
	 add_terminal (snd (stmt (fst mb, L.builder_at_end context else_bb) else_stmt))
	   (L.build_br merge_bb);

	 ignore (L.build_cond_br bool_val then_bb else_bb (snd mb));
	  (fst mb, L.builder_at_end context merge_bb)
      | A.Return _ -> raise (Failure ("returns are not allowed in main"))
      | A.VarDecl (t, n) -> let init = 
              (match t with
                  A.Int -> L.build_alloca i32_t n (snd mb)
                | A.String -> L.build_alloca str_ptr_t n (snd mb)
                | A.File -> L.build_alloca str_ptr_t n (snd mb)
                | A.Dir -> L.build_alloca str_ptr_t n (snd mb)
                | A.Void -> L.build_ret_void (snd mb)
              ) in
              ((StringMap.add n init (fst mb)), snd mb)
      | A.VarDeclAsn (t, n, e) -> let init = 
              (match t with
                  A.Int -> L.build_alloca i32_t n (snd mb)
                | A.String -> L.build_alloca str_ptr_t n (snd mb)
                | A.File -> L.build_alloca str_ptr_t n (snd mb)
                | A.Dir -> L.build_alloca str_ptr_t n (snd mb)
                | A.Void -> L.build_ret_void (snd mb)
              ) in
              let m = (StringMap.add n init (fst mb)) in
              let e' = (expr (m) (snd mb) e) in
              ignore(L.build_store e' (lookup n (m)) (snd mb)) ; (m, (snd mb))
      | A.Asn (s, e) -> let e' = (expr (fst mb) (snd mb) e) in
        ignore(L.build_store e' (lookup s (fst mb)) (snd mb)) ; mb
    in

    (* Build the code for each statement in the function *)
    let retval = stmt (main_vars, builder) (A.Block s) in
    ignore(add_terminal (snd retval) (L.build_ret (L.const_int i32_t 0))) ; fst retval
  in


  ignore(List.iter (build_function_body (build_stmts program.A.stmts)) program.A.funcs);

  (* Add terminal for main function *)
  the_module

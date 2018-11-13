module L = Llvm
module S = Sast
module A = Ast

module StringMap = Map.Make(String)

let translate(globals, functions) =
        let context = L.global_context () in
        let the_module = L.create_module context "Flio"
        and i32_t = L.i32_type context
        and i8_t = L.i8_type context
        and str_ptr_t = L.pointer_type (L.i8_type context)
        and void_t = L.void_type context in
        let file_type = L.struct_type context [|str_ptr_t; i32_t; array_ptr_t|] in
        let dir_type = L.struct_type context [|str_ptr_t; array_ptr_t; array_ptr_t|] in       

        let rec ltype_of_type = function
                S.Int -> i32_t
              | S.String -> str_ptr_t
              | S.Void -> void_t
              | S.File -> file_type
              | S.Dir -> dir_type
              | S.Array (t, _) -> L.struct_type context [| i32_t; L.pointer_type (ltype_of_type t)|]

        in
        
        let global_vars : L.llvalue StringMap.t =
                let global_var m (t, n) =
                        let init = L.const_int (ltype_of_typ t) 0
                in StringMap.add n (L.define_global n init the_module) m in
            List.fold_left global_var StringMap.empty globals in
        
        (* BUILT IN FUNCTION - PRINTF ADDED FOR NOW REST WILL BE ADDED LATER *)
        let printf_t : L.lltype = L.var_arg_function_type i32_t [| L.pointer_type i8_t|] in
        let printf_func : L.llvalue = L.declare_function "printf" printf_t the_module in

        let function_decls: (L.llvalue * sfunc_decl) StringMap.t =
                let function_decl m fdecl = 
                        let name = fdecl.sfname
                        and formal_types = Array.of_list
                                (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals)
                        in let ftype = 
                                L.function_type (ltype_of_typ fdecl.styp) formal_types in 
                        StringMap.add name (L.define_function name ftype the_module, fdecl) m in
                List.fold_left function_decl StringMap.empty functions in

        let build_function_body fdecl =
                let (the_function, _) =
                        StringMap.find fdecl.sfname function_decls in
                let builder =
                        L.builder_at_end context (L.entry_block the_function) in

         let int_format_str =
                 L.build_global_stringptr "%d" "fmt" builder
         and str_format_str =
                 L.build_global_stringptr "%s" "fmt" builder in

         let local_vars =
                 let add_formal m (t, n) p =
                         L.set_value_name n p;
                         let local = L.build_alloca (ltype_of_typ t) n builder in
                         ignore(L.build_store p local builder);
                         StringMap.add n local m
                  and add_local m (t, n) = 
                          let local_var = L.build_alloca (ltype_of_typ t) n builder
                          in StringMap.add n local_var m in

          let formals = List.fold_left2 add_formal StringMap.empty
                fdecls.sformals (Array.to_list (L.params the_function)) in 
          List.fold_left add_local formals fdecl.slocals in  

          let lookup n = try StringMap.find n local_vars
                         with Not_found -> StringMap.find n globla_vars
          in

          let rec expr builder ((_, e) : sexpr) = match e with
                S.SNoexpr -> L.const_int i32_t 0
              | S.SIntLit i -> L.const_int i32_t i
              | S.SStringLit -> L.build_global_stringptr s "string" builder 
              | S.SArrLit e -> (match e with
                        S.Int -> L.const_int i32_t e
                | S.String -> L.build_global_stringptr s "string" builder)
              | S.SId s -> L.build_load (lookup s) s builder
              | S.SArrAccess (id, e) -> 0
              | S.SField (id, field) -> 0
              | S.FuncCall ("printf", [e]) -> L.build_call printf_func [| float_format_str ; (expr builder e) |] "printf" builder
              | S.FuncCall(f, args) -> 
                let (fdef, fdecl) = StringMap.find f function_decls in
                let llargs = List.rev (List.map (expr builder) (List.rev args)) in
                let result = (match fdecl.styp with
                        A.Void -> ""
                      | _ -> f ^ "_result") in
                L.build_call fdef (Array.of_list llargs) result builder
              | S.SBinop (e1, op, e2) -> 
                              let e1' = expr builder e1
                              and e2' = expr builder e2 in
                              (match op with
                              A.Add -> L.build_add
                            | A.Sub -> L.build_sub
                            | A.Mult -> L.build_mul
                            | A.Div -> L.build_sdiv
                            | A.Gt -> L.build_icmp L.Icmp.Sgt
                            | A.Lt -> L.build_icmp L.Icmp.Slt
                            | A.Equal -> L.build_icmp L.Icmp.Eq
                            | A.Neq -> L.build_icmp L.Icmp.Ne
                            | A.And -> L.build_and
                            | A.Or -> L.build_or
                            | A.Pipe -> 0
              | S.SUop (op, e) ->
                let e' = expr builder e in
                (match op with 
                        A.Neg -> L.build_neg
                      | A.Not -> L.build_not) e' "tmp" builder) in
       L.build_call fdef (Array.of_list llargs) result builder in

      let add_terminal builder f =
          match L.block_terminator (L.insertion_block builder) with
              Some _ -> ()
              | None -> ignore (f builder) in
            
      let rec stmt builder = function
         S.SNostmt -> 0_
      |  S.SBlock sl -> List.fold_left stmt builder sl
      |  S.SExpr expr -> ignore(expr builder e); builder
      |  S.SVarDecl (t, id) -> builder
      |  S.SVarDeclAsn (t, id, e) -> ignore(stmt builder(S.Asn(id, e))); builder
      |  S.SAsn(id, e) -> let e' = expr builder e in ignore(L.build_store e' (lookup s)) builder; e'
      |  S.SReturn e -> ignore(match fdecl.styp with
                        A.Void -> L.build_ret_void builder
                      | _ -> L.build_ret (expr builder e) builder);
                      builder
      |  S.SPipeStmt -> 0
      |  S.SFor(e1, e2, e3, body) -> let loop predicate body = 
                let pred_bb = L.append_block context "while" the_function in
                ignore(L.build_br pred_bb builder);

                let body_bb = L.append block context "while_body" the_function in
                add_terminal (stmt (L.builder_at_end context body_bb) body) (L.build_br pred_bb);

                let pred_builder = L.builder_at_end context pred_bb in
                let bool_val = expr pred_builder predicate in

                let merge_bb = L.append_block context "merge" the_function in
                ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
                L.builder_at_end context merge_bb 

        in stmt builder
                (S.SBlock [S.SExpr e1 ;
                        loop (e2, S.SBlock [body ; 
                                                A.Expr e3]) ] )
       |  S.SForeach -> 0
       |  S.SIf (predicate, then_stmt, else_stmt) ->
                 let bool_val = expr builder predicate in
                 let merge_bb = L.append_block context
                                "merge" the_function in
                 let b_br_merge = L.build_br merge_bb in

                 let then_bb = L.append_block context "then" the_function in

                 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
                 b_br_merge;

                 let else_bb = L.append_block context "else" the_function in

                 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
                 b_br_merge;
                 
                 ignore(L.build_con_br bool_val then_bb else_bb builder);

                 L.builder_at_end context merge_bb 
        |  S.SElif -> 0
        in 

        let builder = stmt builder (S.SBlock fdecl.S.sbody) in

        add_terminal builder (match fdecl.S.styp with 
                A.Void -> L.build_return_void
              | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
        in

        List.iter build_function_body functions;
        the_module 


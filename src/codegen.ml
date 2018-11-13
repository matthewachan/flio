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
        and void_t = L.void_type context 
       (* let file_type = L.struct_type context [|str_ptr_t; i32_t; array_ptr_t|] in
        let dir_type = L.struct_type context [|str_ptr_t; array_ptr_t; array_ptr_t|] *) 
        
        in       

        let (*rec*) ltype_of_typ = function
                A.Int -> i32_t
              | A.String -> str_ptr_t
              | A.Void -> void_t
            (*  | A.File (t, _)-> L.struct_type context [|str_ptr_t; i32_t; L.pointer_type A.Array|]
              | A.Dir -> L.struct_type context [|str_ptr_t; L.pointer_type S.Array; L.pointer_type A.Array|]
              | A.Array (t, _) -> L.struct_type context [| i32_t; L.pointer_type (ltype_of_type t)|] *)

        in
        
        let global_vars : L.llvalue StringMap.t =
                let global_var m (t, n) =
                        let init = L.const_int (ltype_of_typ t) 0
                in StringMap.add n (L.define_global n init the_module) m in
            List.fold_left global_var StringMap.empty globals 
                
        in
        
        (* BUILT IN FUNCTION - PRINTF ADDED FOR NOW REST WILL BE ADDED LATER *)
        let printf_t : L.lltype = L.var_arg_function_type i32_t [| L.pointer_type i8_t|] in
        let printf_func : L.llvalue = L.declare_function "printf" printf_t the_module in

        let function_decls: (L.llvalue * S.sfdecl) StringMap.t =
                let function_decl m fdecl = 
                        let name = fdecl.S.sfname
                        and formal_types = Array.of_list
                                (List.map (fun (t, _) -> ltype_of_typ t) fdecl.S.sparams)
                        in let ftype = 
                                L.function_type (ltype_of_typ fdecl.S.styp) formal_types in 
                        StringMap.add name (L.define_function name ftype the_module, fdecl) m in
                List.fold_left function_decl StringMap.empty functions in

        let build_function_body fdecl =
                let (the_function, _) =
                        StringMap.find fdecl.S.sfname function_decls in
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
                          in StringMap.add n local_var m 
                          
          in

          let formals = List.fold_left2 add_formal StringMap.empty
                fdecl.S.sparams (Array.to_list (L.params the_function)) in 
          List.fold_left add_local formals fdecl.slocals in  

          let lookup n = try StringMap.find n local_vars
                         with Not_found -> StringMap.find n global_vars
          in

          let rec expr builder = function
                S.SNoexpr -> L.const_int i32_t 0
              | S.SIntLit i -> L.const_int i32_t i
              | S.SStringLit s -> L.build_global_stringptr s "string" builder 
              | S.SArrLit e -> L.const_int i32_t 0
              | S.SId s -> L.build_load (lookup s) s builder
              | S.SArrAccess (id, sexpr, typeexpr) -> L.const_int i32_t 0
              | S.SField (id, field, field2) -> L.const_int i32_t 0
              | S.SFuncCall ("print", [e], typ) -> L.build_call printf_func [| str_format_str ; (expr builder e) |] "printf" builder
              | S.SFuncCall(f, args, typ) -> 
                let (fdef, fdecl) = StringMap.find f function_decls in
                let llargs = List.rev (List.map (expr builder) (List.rev args)) in
                let result = (match fdecl.styp with
                        A.Void -> ""
                      | _ -> f ^ "_result") in
                L.build_call fdef (Array.of_list llargs) result builder
              | S.SBinop (e1, op, e2, typ) -> 
                              let e1' = expr builder e1
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
                            | A.Pipe -> L.build_add
                        ) e1' e2' "tmp" builder 
              | S.SUop (op, e, typ) -> L.const_int i32_t 0
                (*let e' = expr builder e in
                (match op with 
                        A.Neg -> L.build_neg
                      | A.Not -> L.build_not) e' "tmp" builder)*) in

      let add_terminal builder f =
          match L.block_terminator (L.insertion_block builder) with
              Some _ -> ()
              | None -> ignore (f builder) in
            
      let rec stmt builder = function
         S.SNostmt -> builder
      |  S.SBlock sl -> List.fold_left stmt builder sl
      |  S.SExpr (e, typ) -> ignore (expr builder e); builder
      |  S.SVarDecl (t, id) -> builder
      |  S.SVarDeclAsn (t, id, e) -> ignore (stmt builder(S.SAsn(id, e))); builder
      |  S.SAsn(id, e) -> (*let e' = expr builder e in ignore(L.build_store e' (lookup s)); e'*) builder
      |  S.SReturn (expr, typ) -> (*ignore (match fdecl.styp with
                        A.Void -> L.build_ret_void builder
                      | _ -> L.build_ret (expr builder e) builder);*)
                      builder
      |  S.SPipeStmt (s) -> builder
      |  S.SFor(e1, e2, e3, body) -> (*let loop predicate body = 
                let pred_bb = L.append_block context "while" the_function in
                ignore(L.build_br pred_bb builder);

                let body_bb = L.append_block context "while_body" the_function in
                add_terminal (stmt (L.builder_at_end context body_bb) body) (L.build_br pred_bb);

                let pred_builder = L.builder_at_end context pred_bb in
                let bool_val = expr pred_builder predicate in

                let merge_bb = L.append_block context "merge" the_function in
                ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
                L.builder_at_end context merge_bb 

        in stmt builder
                (S.SBlock [S.SExpr e1 typ;
                        loop (e2, S.SBlock [body ; 
                                                A.Expr e3]) ] ) *) builder
       |  S.SForeach (sexpr1, sexpr2, sstmt) -> builder
       |  S.SIf (predicate, then_stmt, else_stmt) ->
                 let bool_val = expr builder predicate in
                 let merge_bb = L.append_block context "merge" the_function in

                 let then_bb = L.append_block context "then" the_function in
                 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
                 (L.build_br merge_bb);

                 let else_bb = L.append_block context "else" the_function in
                 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
                 (L.build_br merge_bb);
                 
                 ignore(L.build_cond_br bool_val then_bb else_bb builder);

                 L.builder_at_end context merge_bb  
        |  S.SElif(sexprlist, sstmtlist) -> builder 
        
        in  

        let builder = stmt builder (S.SBlock fdecl.S.sbody) in

        add_terminal builder (match fdecl.S.styp with 
                A.Void -> L.build_ret_void
              | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))

        in

        List.iter build_function_body functions;
        the_module 


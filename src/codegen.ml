module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate(program) =
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

       let statements = 
           let statement_items = List.filter (fun x -> match x with 
                A.Stmt (x) -> true
              | _ -> false) program
           in List.map (fun x -> match x with 
                A.Stmt (x) -> x
              | _ -> failwith("Couldn't convert statements")) statement_items
        in 

        let globals = 
           let global_list = List.filter (fun x -> match x with 
              A.VarDecl(x) -> true
            | A.VarDeclAsn(x,_) -> true
            | _ -> false) statements
           in List.map (fun x -> match x with
              A.VarDecl(x) -> x
            |  A.VarDeclAsn(x, _) -> x
            | _ -> failwith ("couldn't determine globals")) global_list
         
         in

        let not_globals = List.filter (fun x -> match x with
            A.VarDecl(x) -> false
           | _ -> true) statements in

         let functions = 
                 let fdecl_main = A.Function({
                         typ = A.Int;
                         fname = "main";
                         params = [];
                         body = List.rev(A.Return(A.IntLit(0)) :: 
                    List.rev(not_globals))
                 })
                in let other_functions = List.filter (fun x -> match x with 
                        A.Function(x) -> true
                      | _ -> false) program
                in
                let all_other_functions = fdecl_main :: other_functions in List.map
                (fun x -> match x with
                      A.Function(x) -> x
                   | _ -> failwith "Couldn't determine function items") all_other_functions 
        in

        let global_vars : L.llvalue StringMap.t =
                let global_var m (t, n) =
                        (*initialize all types, how to initialize structs *)
                        let init = L.const_int (ltype_of_typ t) 0
                in StringMap.add n (L.define_global n init the_module) m in
           List.fold_left global_var StringMap.empty globals 
                
        in
        
        (* BUILT IN FUNCTION - PRINTF ADDED FOR NOW REST WILL BE ADDED LATER *)
        let printf_t : L.lltype = L.var_arg_function_type i32_t [| L.pointer_type i8_t|] in
        let printf_func : L.llvalue = L.declare_function "printf" printf_t the_module in

        let function_decls: (L.llvalue * A.fdecl) StringMap.t =
                let function_decl m fdecl = 
                        let name = fdecl.A.fname
                        and formal_types = Array.of_list
                                (List.map (fun (t, _) -> ltype_of_typ t) fdecl.A.params)
                        in let ftype = 
                                L.function_type (ltype_of_typ fdecl.A.typ) formal_types in 
                        StringMap.add name (L.define_function name ftype the_module, fdecl) m in
                List.fold_left function_decl StringMap.empty functions in

        let build_function_body fdecl =
                let (the_function, _) =
                        StringMap.find fdecl.A.fname function_decls in
                let builder =
                        L.builder_at_end context (L.entry_block the_function) in

         let int_format_str =
                 L.build_global_stringptr "%d" "fmt" builder
         and str_format_str =
                 L.build_global_stringptr "%s" "fmt" builder in

         let lookup n = StringMap.find n global_vars
          in

          let rec expr builder = function
                A.Noexpr -> L.const_int i32_t 0
              | A.IntLit i -> L.const_int i32_t i
              | A.StringLit s -> L.build_global_stringptr s "string" builder 
              | A.ArrLit e -> L.const_int i32_t 0
              | A.Id s -> L.build_load (lookup s) s builder
              | A.ArrAccess (id, sexpr) -> L.const_int i32_t 0
              | A.Field (id, field) -> L.const_int i32_t 0
              | A.FuncCall ("print", [e]) -> L.build_call printf_func [| str_format_str ; (expr builder e) |] "printf" builder
              | A.FuncCall(f, args) -> 
                let (fdef, fdecl) = StringMap.find f function_decls in
                let llargs = List.rev (List.map (expr builder) (List.rev args)) in
                let result = (match fdecl.typ with
                        A.Void -> ""
                      | _ -> f ^ "_result") in
                L.build_call fdef (Array.of_list llargs) result builder
              | A.Binop (e1, op, e2) -> 
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
                        ) e1' e2' "tmp" builder 
              | A.Uop (op, e) -> 
                let e' = expr builder e in
                (match op with 
                        A.Neg -> L.build_neg
                      | A.Not -> L.build_not) e' "tmp" builder in

      let add_terminal builder f =
          match L.block_terminator (L.insertion_block builder) with
              Some _ -> ()
              | None -> ignore (f builder) in
            
      let rec stmt builder = function
         A.Nostmt -> builder
      |  A.Block sl -> List.fold_left stmt builder sl
      |  A.Expr (e) -> ignore (expr builder e); builder
      |  A.VarDecl (t, id) -> builder
      |  A.VarDeclAsn (id, e) -> (*ignore (stmt builder(A.Asn(id, e)));*) builder
      |  A.Asn(id, e) -> (*let e' = expr builder e in ignore(L.build_store e' (lookup s)); e'*) builder
      |  A.Return (expr) -> (*ignore (match fdecl.styp with
                        A.Void -> L.build_ret_void builder
                      | _ -> L.build_ret (expr builder e) builder);*)
                      builder
      |  A.PipeStmt (s) -> builder
      |  A.For(e1, e2, e3, body) -> (*let loop predicate body = 
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
       |  A.Foreach (sexpr1, sexpr2, sstmt) -> builder
       |  A.If (predicate, then_stmt, else_stmt) ->
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
        |  A.Elif(sexprlist, sstmtlist) -> builder 
        
        in  

        let builder = stmt builder (A.Block fdecl.A.body) in

        add_terminal builder (match fdecl.A.typ with 
                A.Void -> L.build_ret_void
              | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))

        in

        List.iter build_function_body functions;
        the_module 


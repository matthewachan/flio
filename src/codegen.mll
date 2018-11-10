module L = Llvm
module S = Sast

module StringMap = Map.Make(String)

let translate(globals, functions) =
        let context = L.global_context () in
        let the_module = L.create_module context "Flio"
        and i32_t = L.i32_type context
        and i8_t = L.i8_type context
        and array_ptr_t = L.pointer_type (L.i8_type context)
        and str_ptr_t = L.pointer_type (L.i8_type context)
        and void_t = L.void_type context in
        let file_type = L.struct_type context [|str_ptr_t; i32_t; array_ptr_t|] in
        let dir_type = L.struct_type context [|str_ptr_t; array_ptr_t; array_ptr_t|] in       

        let ltype_of_type = function
                S.Int -> i32_t
              | S.String -> str_ptr_t
              | S.Void -> void_t
              | S.File -> file_type
              | S.Dir -> dir_type
              | S.Array -> array_ptr_t in
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
          List.fold_left add_local formals fdecl.slocals 

          let lookup n = try StringMap.find n local_vars
                         with Not_found -> StringMap.find n globla_vars
          in

          let rec expr builder ((_, e) : sexpr) = match e with
                S.SNoexpr -> L.const_int i32_t 0
              | S.SIntLit i -> L.const_int i32_t i
              | S.SStringLit -> L.build_global_stringptr s "string" builder 
              | S.SArrLit
              | S.SId
              | S.SFuncCall
              | S.SArrAccess
              | S.SField
              | S.SBinop
              | S.SUop


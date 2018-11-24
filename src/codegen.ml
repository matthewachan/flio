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
       | A.Array (typ, size) -> L.struct_type context [|L.array_type (ltype_of_typ typ) size; i32_t|]

   in 

   (* extract all the global variables from the statements *)
   let globals = 
           let global_list = List.filter (fun x -> match x with
           A.VarDecl(_, _) -> true
         | A.VarDeclAsn(_, _, _) -> true
         | _ -> false) program.A.stmts
           in List.map (fun x-> match x with
           A.VarDecl(x, name) -> (x, name)
         | A.VarDeclAsn(x, name, _) -> (x, name)
         | _ -> raise(Failure("Invalid variable type"))) global_list

   in
   (* Extract the functions from the AST *)
   let functions = program.A.funcs
   
   in

   (*Initialize all the global variables and add them to a string map*)
   let global_vars =
            let global_var m (t, n) =
                    let init = match t with
                    _ -> L.const_int (ltype_of_typ t) 0
             in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals 
                    
    in

  let add_terminal builder f =
    match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder) in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can call it even before we've created its body *)

(* let function_decls =
        let function_decl m fdecl =
                let name = fdecl.A.fname 
                and formal_types = Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.A.params)
                in let ftype = 
                        L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
          StringMap.add name (L.define_function name ftype the_module, fdecl) m in
  List.fold_left function_decl StringMap.empty functions in      

  let build_function_body fdecl =
          let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
          let builder_f = L.build_at_end context (L.entry_block the_function) in *)
  
(* Declare main function *)
  let main_t = L.function_type i32_t [| |] in
  let main_func = L.define_function "main" main_t the_module in

  let builder = L.builder_at_end context (L.entry_block main_func) in
  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

  let lookup n = try StringMap.find n global_vars 
                 with Not_found -> raise (Failure("The variable does not exist")) 
  in

  (* Build statments inside of the main function *) 
  let build_stmts s =
    (* Construct code for an expression; return its value *)
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
      | A.Id s -> L.build_load (lookup s) s builder
      | A.FuncCall(_, _) -> raise (Failure ("not implemented yet"))
      | A.ArrAccess (_, _) -> raise (Failure ("not implemented yet"))
      | A.ArrLit _ -> raise (Failure ("not implemented yet"))
    in

    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
        A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Nostmt -> builder
      | A.For (_, _, _, _) -> raise (Failure ("not implemented yet")) 
      | A.Foreach (_, _, _) -> raise (Failure ("not implemented yet"))
      | A.If (predicate, then_stmt, else_stmt) -> 
       (* let bool_val = expr builder predicate in
        let merge_bb = L.append_block context "merge" the_function in
        let b_br_merge = L.build_br merge_bb in
        let then_bb = L.append_block context "then" the_function in
        add_terminal(stmt (L.builder_at_end context then_bb) then_stmt)
        b_br_merge;

        let else_bb = L.append_block context "else" the_function in
        add_terminal (stmt (L.builder_at_end context else_bb) else_stmt) b_br_merge;
        ignore(L.build_cond_br bool_val then_bb else_bb builder);

        L.builder_at_end context merge_bb*)
      | A.Elif (_, _) -> raise (Failure ("not implemented yet"))
      | A.Return _  -> raise(Failure ("not implemented yet"))
      | A.VarDecl (_, _) -> builder
      | A.VarDeclAsn (_, name, expr) -> ignore (stmt builder (A.Asn(name, expr))); builder
      | A.Asn (s, e) ->  let e' = expr builder e in ignore(L.build_store e' (lookup s) builder); builder

    in

    (* Build the code for each statement in the function *)
    stmt builder (A.Block s) in

  ignore(build_stmts program.A.stmts);
  (* Add terminal for main function *)
  add_terminal builder (L.build_ret (L.const_int i32_t 0)); 
  the_module

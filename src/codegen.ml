module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate program =
  
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
  (*     | A.File -> raise (Failure ("not implemented yet")) *)
  (*     | A.Dir -> raise (Failure ("not implemented yet")) *)
  (*     | A.Array (_, _) -> raise (Failure ("not implemented yet")) *)
  in

    (* Return the value for a variable or formal argument *)
    let lookup n m = StringMap.find n m
    in

  (* Declare each global variable; remember its value in a map *)
  let main_vars = StringMap.empty in 
    (* let global_var m (t, n) = *)
    (*   let init = L.const_int (ltype_of_typ t) 0 *)
    (*   in StringMap.add n (L.define_global n init the_module) m in *)
    (* List.fold_left global_var StringMap.empty globals in *)

  let add_terminal builder f =
    match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder) in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* (1* Declare the built-in printbig() function *1) *)
  (* let printbig_t = L.function_type i32_t [| i32_t |] in *)
  (* let printbig_func = L.declare_function "printbig" printbig_t the_module in *)

  (* Declare main function *)
  let main_t = L.function_type i32_t [| |] in
  let main_func = L.define_function "main" main_t the_module in

  let builder = L.builder_at_end context (L.entry_block main_func) in
  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
  let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

  (* Build statments inside of the main function *) 
  let build_stmts s =
    (* Construct code for an expression; return its value *)
    let rec expr map builder = function
        A.IntLit i -> L.const_int i32_t i
      | A.Noexpr -> L.const_int i32_t 0
      | A.FuncCall ("print", [e]) -> 
                      L.build_call printf_func [| int_format_str ; (expr map builder e) |] "printf" builder
      | A.FuncCall ("prints", [e]) -> 
                      L.build_call printf_func [| str_format_str; (expr map builder e) |] "printf" builder
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
                      (* L.const_string context s *) 
      | A.Id s -> L.build_load (lookup s map) s builder 
      | A.FuncCall(_, _) -> raise (Failure ("not implemented yet"))
      | A.ArrAccess (_, _) -> raise (Failure ("not implemented yet"))
      | A.ArrLit _ -> raise (Failure ("not implemented yet"))
    in

    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt mb = function
        A.Block sl -> 
                (* stmt map builder (List.hd sl) *)
                (* let b = (map, builder) in *)
        (List.fold_left stmt mb sl)
      | A.Expr e -> ignore (expr (fst mb) (snd mb) e); mb
      | A.Nostmt -> raise (Failure ("not implemented yet"))
      | A.For (_, _, _, _) -> raise (Failure ("not implemented yet")) 
      | A.Foreach (_, _, _) -> raise (Failure ("not implemented yet"))
      | A.If (_, _, _) -> raise (Failure ("not implemented yet"))
      | A.Elif (_, _) -> raise (Failure ("not implemented yet"))
      | A.Return _ -> raise (Failure ("not implemented yet"))
      | A.VarDecl (t, n) -> let init = (match t with
                        A.Int -> L.build_alloca i32_t n (snd mb) 
                      | A.String -> L.build_alloca str_ptr_t n (snd mb)
                ) in
              ((StringMap.add n init (fst mb)), snd mb)
      | A.VarDeclAsn (t, s, e) -> let init = (match t with
                        A.Int -> L.build_alloca i32_t s (snd mb) 
                      | A.String -> L.build_alloca str_ptr_t s (snd mb)
                ) in
                let m = (StringMap.add s init (fst mb)) in
              let e' = expr (m) (snd mb) e in
                ignore(L.build_store e' (lookup s (m)) (snd mb)) ; (m, (snd mb))

      | A.Asn (s, e) -> let e' = expr (fst mb) (snd mb) e in
        ignore(L.build_store e' (lookup s (fst mb)) (snd mb)) ; mb
    in

    (* Build the code for each statement in the function *)
    stmt (main_vars, builder) (A.Block s) in

  ignore(build_stmts program.A.stmts);
  (* Add terminal for main function *)
  add_terminal builder (L.build_ret (L.const_int i32_t 0)); 
  the_module

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate program =
  
  let context = L.global_context () in
    let the_module = L.create_module context "flio"
    and i32_t = L.i32_type context
    and i8_t = L.i8_type context
    (* and str_ptr_t = L.pointer_type (L.i8_type context) *)
    (* and void_t = L.void_type context *) 
  in       

  (* Pattern match AST types to LLVM types *)
  (* let ltype_of_typ = function *)
  (*       A.Int -> i32_t *)
  (*     | A.String -> str_ptr_t *)
  (*     | A.Void -> void_t *)
  (*     | A.File -> raise (Failure ("not implemented yet")) *)
  (*     | A.Dir -> raise (Failure ("not implemented yet")) *)
  (*     | A.Array (_, _) -> raise (Failure ("not implemented yet")) *)
  (* in *)

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

  (* Build statments inside of the main function *) 
  let build_stmts s =
    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        A.IntLit i -> L.const_int i32_t i
      | A.Noexpr -> L.const_int i32_t 0
      | A.FuncCall ("print", [e]) -> 
                      L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
      | A.Binop (_, _, _) -> raise (Failure ("not implemented yet"))
      | A.Uop (_, _) -> raise (Failure ("not implemented yet"))
      | A.StringLit _ -> raise (Failure ("not implemented yet"))
      | A.Id _ -> raise (Failure ("not implemented yet"))
      | A.FuncCall(_, _) -> raise (Failure ("not implemented yet"))
      | A.ArrAccess (_, _) -> raise (Failure ("not implemented yet"))
      | A.ArrLit _ -> raise (Failure ("not implemented yet"))
    in

    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
        A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Nostmt -> raise (Failure ("not implemented yet"))
      | A.For (_, _, _, _) -> raise (Failure ("not implemented yet")) 
      | A.Foreach (_, _, _) -> raise (Failure ("not implemented yet"))
      | A.If (_, _, _) -> raise (Failure ("not implemented yet"))
      | A.Elif (_, _) -> raise (Failure ("not implemented yet"))
      | A.Return _ -> raise (Failure ("not implemented yet"))
      | A.VarDecl (_, _) -> raise (Failure ("not implemented yet"))
      | A.VarDeclAsn (_, _, _) -> raise (Failure ("not implemented yet"))
      | A.Asn (_, _) -> raise (Failure ("not implemented yet"))
    in

    (* Build the code for each statement in the function *)
    stmt builder (A.Block s) in

  ignore(build_stmts program.A.stmts);
  (* Add terminal for main function *)
  add_terminal builder (L.build_ret (L.const_int i32_t 0)); 
  the_module

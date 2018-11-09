module L = Llvm
module A = Ast

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
                A.Int -> i32_t
              | A.String -> str_ptr_t
              | A.Void -> void_t
              | A.File -> file_type
              | A.Dir -> dir_type
              | A.Array -> array_ptr_t in



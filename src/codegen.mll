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
        and file_ptr_t = L.pointer_type (L.i8_type context)
        and dir_ptr_t = L.pointer_type (L.i8_type context)
        and void_t = L.void_type context in

        let ltype_of_type = function
                A.Int -> i32_t
              | A.String -> ptr_t
              | A.Void -> void_t
              | A.File -> file_ptr_t 
              | A.Dir -> dir_ptr_t
              | A.Array -> array_ptr_t




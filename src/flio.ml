(*
 * flio.ml
 * Author: Matthew Chan
 *)
type action = Ast | LLVM_IR | Compile

let () =
        let action = ref Compile in
        let set_action a () = action := a in
        let speclist = [
                ("-a", Arg.Unit (set_action Ast), "Print the SAST");
                ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
                ("-c", Arg.Unit (set_action Compile), "Check and print the generated LLVM IR (default)");
                ] in
        let usage_msg = "usage: ./filo.native [-a|-s|-l|-c] [file.f]" in
        let channel = ref stdin in
        Arg.parse speclist
                (fun filename -> channel := open_in filename) usage_msg;
        
        let lexbuf = Lexing.from_channel !channel in

        let ast = Parser.program Scanner.token lexbuf in
        ignore(Semant.check ast);
        
        match !action with
             Ast -> print_string (Ast.string_of_program ast)
           | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
           | Compile -> let m = Codegen.translate ast in
                Llvm_analysis.assert_valid_module m;
                print_string (Llvm.string_of_llmodule m)

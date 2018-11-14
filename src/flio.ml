(*
  flio.
  Author: Matthew Chan
*)

open Ast

let _ =
let _ = print_endline "running flio" in
 let lexbuf = Lexing.from_channel stdin in
  try
    let ast = Parser.program Scanner.token lexbuf in
    Semant.check ast ; print_string (Ast.string_of_program ast)
  with e ->
          print_endline "Parsing failed - Invalid program.";
    raise e
(*
    let _err_msg = Printexc.to_string e
    and _trace = Printexc.get_backtrace () in
    print_endline ("Parsing failed: " ^ err_msg ^ trace);
    raise e
*)

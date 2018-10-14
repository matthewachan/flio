(*
  flio.ml
  Author: Matthew Chan
*)

open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  try
    let _p = Parser.program Scanner.token lexbuf in
    print_endline "Parsing OK - Valid program."
  with e ->
    print_endline "Parsing failed - Invalid program."
(*
    let _err_msg = Printexc.to_string e
    and _trace = Printexc.get_backtrace () in
    print_endline ("Parsing failed: " ^ err_msg ^ trace);
    raise e
*)

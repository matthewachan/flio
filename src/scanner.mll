(*
  scanner.mll
  Author: Matthew Chan
*)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
(* Types *)
| "int"		{ INT }
| "string"	{ STRING }
| "file"	{ FILE }
| "dir"		{ DIR }
(* Function keywords *)
| "def"		{ DEF }
| "return"	{ RETURN }
(* Loops and conditionals *)
| "for"		{ FOR }
| "foreach"	{ FOREACH }
| "in"		{ IN }
| "if"		{ IF }
| "elif"	{ ELIF }
| "else"	{ ELSE }
(* Misc *)
| "import"	{ IMPORT }
| "//"		{ comment lexbuf }
| '('		{ LPAREN }
| ')'		{ RPAREN }
| '{'		{ LBRACE }
| '}'		{ RBRACE }
| '['		{ LBRACK }
| ']'		{ RBRACK }
| ','		{ COMMA }
| ';'		{ SEQUENCING } 
(* Operators *)
| '.'		{ DOT }
| '+'		{ PLUS }
| '-'		{ MINUS }
| '*'		{ TIMES }
| '/'		{ DIVIDE }
| "|>"		{ PIPE }
| '>'		{ GT }
| '<'		{ LT }
| "=="		{ EQ }
| "!="		{ NEQ }
| "and"		{ AND }
| "or"		{ OR }
| '='		{ ASSIGNMENT }
| "not"		{ NOT }
(* Literals*)
| ['0'-'9']+ as lit 					{ INTLIT(int_of_string lit) }
| '\''([^'\'']* as string_lit)'\'' 			{ STRINGLIT(string_lit) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { ID(id) }

| eof 		{ EOF }

and comment = parse
  '\n'	{ token lexbuf }
| _	{ comment lexbuf }	

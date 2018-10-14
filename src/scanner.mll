{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "int"		{ INT }
| "string"	{ STRING }
| "file"	{ FILE }
| "dir"		{ DIR }
| "def"		{ DEF }
| "return"	{ RETURN }
| "for"		{ FOR }
| "foreach"	{ FOREACH }
| "in"		{ IN }
| "if"		{ IF }
| "else"	{ ELSE }
| "//"		{ comment lexbuf }
| '('		{ LPAREN }
| ')'		{ RPAREN }
| '{'		{ LBRACE }
| '}'		{ RBRACE }
| '['		{ LBRACK }
| ']'		{ RBRACK }
| ','		{ COMMA }
| '.'		{ DOT }
| '+'		{ PLUS }
| '-'		{ MINUS }
| '*'		{ TIMES }
| '/'		{ DIVIDE }
| '>'		{ GT }
| '<'		{ LT }
| "=="		{ EQ }
| "!="		{ NEQ }
| "and"		{ AND }
| "or"		{ OR }
| '='		{ ASSIGNMENT }
| "not"		{ NOT }
| ';'		{ SEQUENCING } 
| ['0'-'9']+ as lit 					{ INTLIT(int_of_string lit) }
| '\''([^'\'']* as string_lit)'\'' 			{ STRINGLIT(string_lit) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { ID(id) }
| eof 		{ EOF }
and comment = parse
  '\n'	{ token lexbuf }
| _	{ comment lexbuf }	

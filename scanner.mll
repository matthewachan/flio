{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "int"		{ INT }
| "string"	{ STRING }
| "def"		{ DEF }
| "return"	{ RETURN }
| "for"		{ FOR }
| "if"		{ IF }
| "else"	{ ELSE }
| '('		{ LPAREN }
| ')'		{ RPAREN }
| '{'		{ LBRACE }
| '}'		{ RBRACE }
| ','		{ COMMA }
| '+'		{ PLUS }
| '-'		{ MINUS }
| '*'		{ TIMES }
| '/'		{ DIVIDE }
| '>'		{ GT }
| '<'		{ LT }
| "=="		{ EQ }
| '='		{ ASSIGNMENT }
| ';'		{ SEQUENCING } 
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { ID(id) }
| eof 		{ EOF }

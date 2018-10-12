{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "//"          { single lexbuf } 
| "int"         { INT }
| "string"      { STRING }
| "file"        { FILE }
| "dir"         { DIRECTORY }
| '['           { LSQUARE }
| ']'           { RSQUARE }
| '{'           { LCURLY }
| '}'           { RCURLY }
| '('           { LPAREN }
| ')'           { RPAREN }
| ','           { COMMA }
| ';'           { SEMI }
| '+'           { PLUS }
| '-'           { MINUS }
| '*'           { MULTIPLY}
| '/'           { DIVIDE }
| '='           { ASSIGN }
| '<'           { LT }
| '>'           { GT }
| "|>"          { PIPE }
| "=="          { EQ }
| "!="          { NEQ }
| "if"          { IF }
| "elif"        { ELIF }
| "else"        { ELSE }
| "for"         { FOR }
| "and"         { AND }
| "or"          { OR }
| "not"         { NOT }
| "def"         { DEF }
| "return"      { RET }
(* int *)
| ['0'-'9']+ as int_lit { INT_LITERAL(int_of_string int_lit) }
(* string *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as string_lit { STRING_LITERAL(int_of_string string_lit) }
| eof           { EOF }



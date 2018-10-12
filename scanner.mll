{ open Parser }

rule token = parse
  [' ' '\t' '\r'] { token lexbuf }
| "//"          { single_comment lexbuf } 
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
| '\n'          { NEWLINE }
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
| '\''([^'\'']* as string_lit)'\'' { STRING_LITERAL(string_lit) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { VARIABLE(id) }
| eof           { EOF }

and single_comment = parse
  '\n'          { token lexbuf }
| _             { single_comment lexbuf }

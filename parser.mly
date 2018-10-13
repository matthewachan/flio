%{ open Ast %}

%token INT STRING
%token PLUS MINUS TIMES DIVIDE EOF ASSIGNMENT SEQUENCING
%token DEF
%token LBRACE RBRACE LPAREN RPAREN COMMA
%token <int> LITERAL
%token <string> ID

%left ASSIGNMENT SEQUENCING
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

/* { funcs: [<fdecl>]; stmts: [<stmt>] } */
program:
  decls EOF { $1 }

decls:
 	{ {stmts = []} }
| decls stmt	{ {stmts = ($2 :: $1.stmts)} }


vdecl_stmt:
  typ ID SEQUENCING	{ VarDecl($1, $2) }
| typ ID ASSIGNMENT expr SEQUENCING	{ VarDeclAsn($1, $2, $4) }

stmt:
  expr SEQUENCING	{ Expr($1) }
| vdecl_stmt		{ $1 }


expr:
  expr PLUS  expr		{ Binop($1, Add, $3) }
| expr MINUS expr		{ Binop($1, Sub, $3) }
| expr TIMES expr		{ Binop($1, Mul, $3) }
| expr DIVIDE expr		{ Binop($1, Div, $3) }
| ID ASSIGNMENT expr		{ Asn($1, $3) }
| LITERAL			{ Lit($1) }
| ID				{ Id($1) }

typ:
  INT		{ Int }
| STRING	{ String }

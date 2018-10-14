%{ open Ast %}

%token INT STRING
%token PLUS MINUS TIMES DIVIDE EOF ASSIGNMENT SEQUENCING GT LT EQ
%token DEF RETURN
%token FOR
%token LBRACE RBRACE LPAREN RPAREN COMMA SEMI
%token <int> LITERAL
%token <string> ID

%right ASSIGNMENT
%left SEQUENCING
%left LT GT EQ
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
  typ ID SEQUENCING			{ VarDecl($1, $2) }
| typ ID ASSIGNMENT expr SEQUENCING	{ VarDeclAsn($1, $2, $4) }

stmt_list:
			{ [] }
| stmt_list stmt 	{ $2 :: $1 }

stmt:
  expr SEQUENCING								{ Expr($1) }
| vdecl_stmt									{ $1 }
| RETURN expr									{ Return($2) }
| LBRACE stmt_list RBRACE							{ Block(List.rev $2) }
| FOR LPAREN expr_opt SEQUENCING expr_opt SEQUENCING expr_opt RPAREN stmt 	{ For($3, $5, $7, $9) }

expr_opt:
		{ Noexpr }
| expr		{ $1 }

expr:
  expr PLUS  expr		{ Binop($1, Add, $3) }
| expr MINUS expr		{ Binop($1, Sub, $3) }
| expr TIMES expr		{ Binop($1, Mul, $3) }
| expr DIVIDE expr		{ Binop($1, Div, $3) }
| expr LT  expr			{ Binop($1, Lt, $3) }
| expr GT  expr			{ Binop($1, Gt, $3) }
| expr EQ  expr			{ Binop($1, Eq, $3) }
| ID ASSIGNMENT expr		{ Asn($1, $3) }
| LITERAL			{ Lit($1) }
| ID				{ Id($1) }

typ:
  INT		{ Int }
| STRING	{ String }

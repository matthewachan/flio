%{ open Ast %}

%token INT STRING FILE DIR
%token PLUS MINUS TIMES DIVIDE ASSIGNMENT 
%token GT LT EQ NEQ NOT AND OR
%token DEF RETURN
%token DOT
%token FOR FOREACH IN IF ELSE
%token EOF LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK COMMA SEQUENCING
%token <int> INTLIT
%token <string> STRINGLIT
%token <string> ID

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGNMENT
%left OR
%left AND
%left SEQUENCING
%left LT GT EQ NEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG
%left DOT

%start program
%type <Ast.program> program

%%

/* { funcs: [<fdecl>]; stmts: [<stmt>] } */
program:
  decls EOF { $1 }

decls:
 		{ {funcs = []; stmts = []} }
| decls fdecl	{ {funcs = ($2 :: $1.funcs); stmts = $1.stmts} }
| decls stmt	{ {funcs = $1.funcs; stmts = ($2 :: $1.stmts)} }

fdecl:
  DEF ID LPAREN params RPAREN typ_opt LBRACE stmt_list RBRACE	{ {typ = $6; fname = $2; params = $4; body = List.rev $8} }

params:
  		{ [] }
| paramlist	{ List.rev $1 }

paramlist:
  typ ID 			{ [Parameter($1, $2)] }
| paramlist COMMA typ ID 	{ Parameter($3, $4) :: $1}

args:
		{ [] }
| arglist	{ List.rev $1 }

arglist:
  expr			{ [$1] }
| arglist COMMA expr 	{ $3 :: $1 }


vdecl_stmt:
  typ ID SEQUENCING				{ VarDecl($1, $2) }
| typ ID ASSIGNMENT expr SEQUENCING		{ VarDeclAsn($1, $2, $4) }
| typ ID ASSIGNMENT array_lit SEQUENCING 	{ VarDeclAsn($1, $2, $4) }

array_lit:
  LBRACE args RBRACE	{ ArrLit($2) }

stmt_list:
			{ [] }
| stmt_list stmt 	{ $2 :: $1 }

stmt:
  expr SEQUENCING								{ Expr($1) }
| vdecl_stmt									{ $1 }
| RETURN expr SEQUENCING							{ Return($2) }
| RETURN SEQUENCING								{ Return(Noexpr) }
| LBRACE stmt_list RBRACE							{ Block(List.rev $2) }
| FOR LPAREN expr_opt SEQUENCING expr_opt SEQUENCING expr_opt RPAREN stmt 	{ For($3, $5, $7, $9) }
| FOREACH expr IN expr stmt							{ Foreach($2, $4, $5) }
| IF LPAREN expr RPAREN stmt %prec NOELSE					{ If($3, $5, Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt 						{ If($3, $5, $7) }


expr_opt:
		{ Noexpr }
| expr		{ $1 }

expr:
  expr PLUS  expr			{ Binop($1, Add, $3) }
| expr MINUS expr			{ Binop($1, Sub, $3) }
| expr TIMES expr			{ Binop($1, Mul, $3) }
| expr DIVIDE expr			{ Binop($1, Div, $3) }
| expr LT  expr				{ Binop($1, Lt, $3) }
| expr GT  expr				{ Binop($1, Gt, $3) }
| expr EQ  expr				{ Binop($1, Eq, $3) }
| expr NEQ  expr			{ Binop($1, Neq, $3) }
| expr AND  expr			{ Binop($1, And, $3) }
| expr OR  expr				{ Binop($1, Or, $3) }
| NOT expr				{ Uop(Not, $2) }
| expr DOT ID				{ Field($1, $3) }
| ID ASSIGNMENT expr			{ Asn($1, $3) }
| ID ASSIGNMENT array_lit		{ Asn($1, $3) }
| ID LBRACK expr RBRACK ASSIGNMENT expr	{ Asn($1, $3) }
| INTLIT				{ IntLit($1) }
| STRINGLIT				{ StringLit($1) }
| ID					{ Id($1) }
| ID LPAREN args RPAREN			{ FuncCall($1, $3) }
| ID LBRACK expr RBRACK			{ ArrAccess($1, $3) }

typ_opt:
	{ Void }
| typ	{ $1 }

typ:
  INT		{ Int }
| STRING	{ String }
| FILE		{ File }
| DIR		{ Dir }
| typ LBRACK INTLIT RBRACK { Array($1, $3) }

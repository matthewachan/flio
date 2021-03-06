/*
  parser.mly
  Author: Matthew Chan
*/

%{ open Ast %}

%token EOF LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK COMMA SEQUENCING
%token INT STRING FILE DIR
%token PLUS MINUS TIMES DIVIDE ASSIGNMENT
%token GT LT EQ NEQ NOT AND OR
%token DEF RETURN
%token FOR IN IF ELSE
%token <int> INTLIT
%token <string> STRINGLIT
%token <string> ID

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGNMENT
%left CALL
%left OR
%left AND
// %left SEQUENCING
%left EQ NEQ
%left LT GT 
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

/* { funcs: [<fdecl>]; stmts: [<stmt>] } */
program:
        decls EOF { {funcs = $1.funcs; stmts = List.rev $1.stmts} }

decls:
 		{ {funcs = []; stmts = []} }
| decls fdecl	{ {funcs = ($2 :: $1.funcs); stmts = $1.stmts} }
| decls stmt	{ {funcs = $1.funcs; stmts = ($2 :: $1.stmts)} }


/* Function declaration / definition */
fdecl:
  DEF ID LPAREN params RPAREN typ_opt LBRACE stmt_list RBRACE	{ {typ = $6; fname = $2; params = $4; body = List.rev $8} }

params:
  		{ [] }
| paramlist	{ List.rev $1 }

paramlist:
  typ ID 			{ [($1, $2)] }
| paramlist COMMA typ ID 	{ ($3, $4) :: $1}


args:
		{ [] }
| arglist	{ List.rev $1 }

arglist:
  expr			{ [$1] }
| arglist COMMA expr 	{ $3 :: $1 }

/* Statements */
stmt_opt:
  SEQUENCING		{ Nostmt }
| stmt			{ $1 }

stmt_list:
			{ [] }
| stmt_list stmt 	{ $2 :: $1 }

stmt:
  expr SEQUENCING								{ Expr($1) }
| vdecl_stmt									{ $1 }
| asn_stmt									{ $1 }
| RETURN expr SEQUENCING							{ Return($2) }
| RETURN SEQUENCING								{ Return(Noexpr) }
| LBRACE stmt_list RBRACE							{ Block(List.rev $2) }
| FOR LPAREN stmt_opt expr_opt SEQUENCING stmt_opt RPAREN stmt 			{ For($3, $4, $6, $8) }
| IF LPAREN expr RPAREN stmt %prec NOELSE					{ If($3, $5, Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt 						{ If($3, $5, $7) }

vdecl_stmt:
  typ ID SEQUENCING				{ VarDecl($1, $2) }
| typ ID ASSIGNMENT expr SEQUENCING		{ VarDeclAsn($1, $2, $4) }


asn_stmt:
  ID ASSIGNMENT expr SEQUENCING							{ Asn($1, $3) }
| ID LBRACK expr RBRACK ASSIGNMENT expr	SEQUENCING				{ Asn($1, $3) }

/* Expressions */
expr_opt:
		{ Noexpr }
| expr		{ $1 }

expr:
| INTLIT				{ IntLit($1) }
| STRINGLIT				{ StringLit($1) }
| ID					{ Id($1) }
| ID LPAREN args RPAREN	%prec CALL	{ FuncCall($1, $3) }
| expr PLUS  expr			{ Binop($1, Add, $3) }
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
// | MINUS expr %prec NEG			{ Uop(Neg, $2) }

/* Types */
typ_opt:
	{ Void }
| typ	{ $1 }

typ:
  INT		{ Int }
| STRING	{ String }
| FILE		{ File }
| DIR		{ Dir }

/*
  parser.mly
  Author: Matthew Chan
*/

%{ open Ast %}

%token INT STRING FILE DIR
%token PLUS MINUS TIMES DIVIDE ASSIGNMENT PIPE
%token GT LT EQ NEQ NOT AND OR
%token DEF RETURN
%token DOT
%token IMPORT
%token FOR FOREACH IN IF ELIF ELSE
%token EOF LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK COMMA SEQUENCING
%token <int> INTLIT
%token <string> STRINGLIT
%token <string> ID

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc ELIF
%right ASSIGNMENT
%left CALL
%left OR
%left AND
%left SEQUENCING
%left LT GT EQ NEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left PIPE
%right NOT NEG
%left DOT

%start program
%type <Ast.program> program

%%

/* { funcs: [<fdecl>]; stmts: [<stmt>] } */
program:
        decls EOF { {imports = $1.imports; funcs = $1.funcs; stmts = List.rev $1.stmts} }

decls:
 		{ {imports = []; funcs = []; stmts = []} }
| decls import	{ {imports = ($2 :: $1.imports); funcs = $1.funcs; stmts = $1.stmts} }
| decls fdecl	{ {imports = $1.imports; funcs = ($2 :: $1.funcs); stmts = $1.stmts} }
| decls stmt	{ {imports = $1.imports; funcs = $1.funcs; stmts = ($2 :: $1.stmts)} }

/* Imports */
import:
  IMPORT STRINGLIT	{ $2 }

/* Function declaration / definition */
fdecl:
  DEF ID LPAREN params RPAREN typ_opt LBRACE stmt_list RBRACE	{ {typ = $6; fname = $2; params = $4; body = List.rev $8} }

params:
  		{ [] }
| paramlist	{ List.rev $1 }

paramlist:
  typ ID 			{ [($1, $2)] }
| paramlist COMMA typ ID 	{ ($3, $4) :: $1}


/* Arrays */
array_lit:
  LBRACE args RBRACE	{ ArrLit($2) }

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
| FOREACH expr IN expr stmt							{ Foreach($2, $4, $5) }
| IF LPAREN expr RPAREN stmt %prec NOELSE					{ If($3, $5, Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt 						{ If($3, $5, $7) }
| IF LPAREN expr RPAREN stmt elif_list %prec NOELSE 				{ Elif(($3 :: (List.rev(fst $6))), (List.rev((Block([]) :: (List.rev ($5 :: (List.rev (snd $6)))))))) }  
| IF LPAREN expr RPAREN stmt elif_list ELSE stmt 				{ Elif(($3 :: (List.rev(fst $6))), (List.rev(($8 :: (List.rev ($5 :: (List.rev (snd $6)))))))) }

vdecl_stmt:
  typ ID SEQUENCING				{ VarDecl($1, $2) }
| typ ID ASSIGNMENT expr SEQUENCING		{ VarDeclAsn($1, $2, $4) }
| typ ID ASSIGNMENT array_lit SEQUENCING 	{ VarDeclAsn($1, $2, $4) }


asn_stmt:
  ID ASSIGNMENT expr SEQUENCING							{ Asn($1, $3) }
| ID ASSIGNMENT array_lit SEQUENCING						{ Asn($1, $3) }
| ID LBRACK expr RBRACK ASSIGNMENT expr	SEQUENCING				{ Asn($1, $3) }

elif_list:
  elif			{ ([fst $1], [snd $1]) }
| elif_list elif	{ (fst $2 :: fst $1, snd $2 :: snd $1) }

elif:
  ELIF LPAREN expr RPAREN stmt	{ ($3, $5) }

/* Expressions */
expr_opt:
		{ Noexpr }
| expr		{ $1 }

expr:
  arith_expr				{ $1 }
| logic_expr				{ $1 }
| expr PIPE expr                        { Binop($1, Pipe, $3) }
| uop_expr				{ $1 }
| INTLIT				{ IntLit($1) }
| STRINGLIT				{ StringLit($1) }
| ID					{ Id($1) }
| ID LPAREN args RPAREN	%prec CALL	{ FuncCall($1, $3) }
| ID LBRACK expr RBRACK			{ ArrAccess($1, $3) }

arith_expr:
  expr PLUS  expr			{ Binop($1, Add, $3) }
| expr MINUS expr			{ Binop($1, Sub, $3) }
| expr TIMES expr			{ Binop($1, Mul, $3) }
| expr DIVIDE expr			{ Binop($1, Div, $3) }

logic_expr:
  expr LT  expr				{ Binop($1, Lt, $3) }
| expr GT  expr				{ Binop($1, Gt, $3) }
| expr EQ  expr				{ Binop($1, Eq, $3) }
| expr NEQ  expr			{ Binop($1, Neq, $3) }
| expr AND  expr			{ Binop($1, And, $3) }
| expr OR  expr				{ Binop($1, Or, $3) }

uop_expr:
  NOT expr				{ Uop(Not, $2) }
/*| MINUS expr %prec NEG			{ Uop(Neg, $2) }*/

/* Types */
typ_opt:
	{ Void }
| typ	{ $1 }

typ:
  INT		{ Int }
| STRING	{ String }
| FILE		{ File }
| DIR		{ Dir }
| typ LBRACK INTLIT RBRACK { Array($1, $3) }

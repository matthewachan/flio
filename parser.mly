%{ open Ast %}

%token INT STRING FILE DIR
%token PLUS MINUS TIMES DIVIDE ASSIGNMENT 
%token GT LT EQ NEQ NOT AND OR
%token DEF RETURN
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
| FOREACH expr IN expr stmt							{ Foreach($2, $4, $5) }
| IF LPAREN expr RPAREN stmt %prec NOELSE					{ If($3, $5, Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt 						{ If($3, $5, $7) }


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
| expr NEQ  expr		{ Binop($1, Neq, $3) }
| expr AND  expr		{ Binop($1, And, $3) }
| expr OR  expr			{ Binop($1, Or, $3) }
| NOT expr			{ Uop(Not, $2) }
| ID ASSIGNMENT expr		{ Asn($1, $3) }
| INTLIT			{ IntLit($1) }
| STRINGLIT			{ StringLit($1) }
| ID				{ Id($1) }



typ:
  INT		{ Int }
| STRING	{ String }
| FILE		{ File }
| DIR		{ Dir }
| typ LBRACK INTLIT RBRACK { Array($1, $3) }

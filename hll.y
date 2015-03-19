
%token TOK_PRIVATE
%token TOK_PUBLIC
%token TOK_GLOBALS
%token TOK_LOCALS
%token TOK_FUNCTION
%token TOK_IF
%token TOK_ELSE
%token TOK_RETURN
%token TOK_TRY
%token TOK_CATCH
%token TOK_FINALLY
%token TOK_IDENTIFIER
%token TOK_INTEGER
%token TOK_LBR
%token TOK_RBR
%token TOK_ASSIGN
%token TOK_LPAREN
%token TOK_RPAREN
%token TOK_DOT
%token TOK_COMMA
%token TOK_SEMIC

%start compilation_unit

%%

expr_list:
	expr_list TOK_COMMA expr
	| expr
	;

expr_list_or_empty:
	expr_list
	| /* empty */
	;

function_call:
	expr TOK_DOT TOK_IDENTIFIER TOK_LPAREN expr_list_or_empty TOK_RPAREN
	;

expr:
	TOK_LPAREN expr TOK_RPAREN
	| function_call
	| TOK_IDENTIFIER
	| TOK_INTEGER
	;

assignment_statement:
	TOK_IDENTIFIER TOK_ASSIGN expr TOK_SEMIC
	;

if_statement:
	TOK_IF expr statement_block TOK_ELSE statement_block
	| TOK_IF expr statement_block
	;

return_statement:
	TOK_RETURN expr TOK_SEMIC
	;

except_clause_try:
	TOK_TRY TOK_LBR statement_list_or_empty TOK_RBR
	;

except_clause_catch:
	TOK_CATCH TOK_LPAREN TOK_INTEGER TOK_RPAREN TOK_LBR statement_list_or_empty TOK_RBR
	;

except_clause_catch_list:
	except_clause_catch_list except_clause_catch
	| except_clause_catch
	;

except_clause_catch_list_or_empty:
	except_clause_catch_list
	| /* empty */
	;

except_clause_finally_or_empty:
	TOK_FINALLY TOK_LBR statement_list_or_empty TOK_RBR
	| /* empty */
	;

except_clause:
	except_clause_try
	except_clause_catch_list_or_empty
	except_clause_finally_or_empty
	;

statement:
	assignment_statement
	| if_statement
	| return_statement
	| except_clause
	| expr TOK_SEMIC
	;

statement_list:
	statement_list statement
	| statement
	;

statement_list_or_empty:
	statement_list
	| /* empty */
	;

locals_declaration:
	TOK_LOCALS identifier_list_or_empty TOK_SEMIC
	;

locals_declaration_or_empty:
	locals_declaration
	| /* empty */
	;

statement_block:
	TOK_LBR
	locals_declaration_or_empty
	statement_list_or_empty
	TOK_RBR
	;

scope_optional:
	TOK_PUBLIC
	| TOK_PRIVATE
	| /* empty */
	;

globals_declaration:
	scope_optional TOK_GLOBALS identifier_list_or_empty TOK_SEMIC
	;

function_name: TOK_IDENTIFIER;

function_body: statement_block;

identifier_list:
	identifier_list TOK_COMMA TOK_IDENTIFIER
	| TOK_IDENTIFIER
	;

identifier_list_or_empty:
	identifier_list
	| /* empty */
	;

function_arguments:
	TOK_LPAREN
	identifier_list_or_empty
	TOK_RPAREN
	;

function_declaration:
	scope_optional TOK_FUNCTION function_name function_arguments
	function_body
	;

top_level_statement:
	globals_declaration
	| function_declaration
	;

top_level_statement_list:
	top_level_statement_list top_level_statement
	| top_level_statement
	;

compilation_unit:
	top_level_statement_list
	| /* empty */
	;

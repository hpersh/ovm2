
%{

#include "hll.tab.h"

%}

%%

"private"		return (TOK_PRIVATE);
"public"		return (TOK_PUBLIC);
"globals"		return (TOK_GLOBALS);
"locals"		return (TOK_LOCALS);
"function"		return (TOK_FUNCTION);
"if"			return (TOK_IF);
"else"			return (TOK_ELSE);
"return"                return (TOK_RETURN);
"try"			return (TOK_TRY);
"catch"			return (TOK_CATCH);
"finally"		return (TOK_FINALLY);
[A-Za-z_][A-Za-z0-9_]*	return (TOK_IDENTIFIER);
"-"?[0-9]+		return (TOK_INTEGER);
"{"			return (TOK_LBR);
"}"			return (TOK_RBR);
":="			return (TOK_ASSIGN);
"("			return (TOK_LPAREN);
")"			return (TOK_RPAREN);
"."			return (TOK_DOT);
","			return (TOK_COMMA);
";"			return (TOK_SEMIC);

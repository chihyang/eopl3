%{
#include <stdlib.h>
#include "proc.h"
#include "proc_parser.h"
%}

%option reentrant bison-bridge nodefault noyywrap yylineno nounput noinput
%option extra-type="symbol_t"

newline    \n
whitespace [ \t\r]
identifier [a-zA-Z]([a-zA-Z]|[0-9])*
number      ("-"?[0-9]+)

%%
%{
symbol_t table = yyextra;
%}

{newline}    ;
{whitespace} ;
if           { return IF; }
in           { return IN; }
let          { return LET; }
else         { return ELSE; }
proc         { return PROC; }
then         { return THEN; }
zero\?       { return ZERO; }
letrec       { return LETREC; }
{identifier} { yylval->id = symbol_lookup(table, yytext) ;return IDENTIFIER; }
{number}     { yylval->num = atoi(yytext); return NUMBER; }
.            { return yytext[0]; }

%%

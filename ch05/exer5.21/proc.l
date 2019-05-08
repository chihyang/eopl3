%{
#include <stdlib.h>
#include "proc_parser.h"
%}

%option reentrant nodefault noyywrap yylineno

newline    \n
whitespace [ \t\r]
identifier [a-zA-Z]([a-zA-Z]|[0-9])*
number      ("-"?[0-9]+)

%%

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
{identifier} { yylval.id = new_symbol(yytext) ;return IDENTIFIER; }
{number}     { yylval.num = atoi(yytext); return NUMBER; }
.            { return yytext[0]; }

%%

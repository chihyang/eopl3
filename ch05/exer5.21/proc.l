%{
# include "exer5.21.h"
%}

%option reentrant nodefault noyywrap

newline    \n
whitespace [ \t\r]
identifier [a-zA-Z]([a-zA-Z]|[0-9])*
number      ("-"?[0-9]+)|("-"?[0-9]*\.[0-9]+)|("-"?[0-9]+\.[0-9]*)

%%

{newline}    ;
{whitespace} ;
if           { printf("kw if\n"); /* return IF; */ }
in           { printf("kw in\n"); /* return IN; */ }
let          { printf("kw let\n"); /* return LET; */ }
else         { printf("kw else\n"); /* return ELSE; */ }
proc         { printf("kw proc\n"); /* return PROC; */ }
then         { printf("kw then\n"); /* return THEN; */ }
zero\?       { printf("kw zero?\n"); /* return ZERO; */ }
letrec       { printf("kw letrec\n"); /* return LETREC; */ }
{identifier} { printf("id: %s\n", yytext); }
{number}     { printf("number: %s\n", yytext); }
.            { printf("others: %s\n", yytext); }

%%

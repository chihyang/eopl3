%{
#include <stdio.h>
%}

/* declare tokens */
%token IF IN LET ELSE PROC THEN ZERO LETREC
%token IDENTIFIER
%token NUMBER

%%

program: expression;

expression:     const_exp
        |       var_exp
        |       proc_exp
        |       letrec_exp
        |       zero_exp
        |       if_exp
        |       let_exp
        |       diff_exp
        |       call_exp;

const_exp:      NUMBER
        ;

var_exp:        IDENTIFIER {}
        ;

proc_exp:       PROC '(' IDENTIFIER ')' expression {}
        ;

letrec_exp:     LETREC IDENTIFIER '(' IDENTIFIER ')' '=' expression IN expression
                {}
        ;

zero_exp:       ZERO '(' expression ')'
                {}
        ;

if_exp:         IF expression THEN expression ELSE expression
        ;

let_exp:        LET IDENTIFIER '=' expression IN expression
                {}
        ;

diff_exp:       '-' '('expression ',' expression')' {}
        ;

call_exp:       '(' expression expression ')' {}
        ;

%%

void yyerror(char *s) {
    fprintf(stderr, "error: %s\n", s);
}

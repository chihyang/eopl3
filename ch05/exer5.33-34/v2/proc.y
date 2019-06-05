%{
#define YYERROR_VERBOSE

#include <stdarg.h>
#include <stdio.h>
#include "proc.h"
#include "proc_parser.h"
#include "proc_scanner.h"
%}

%define api.pure
%lex-param { void *scanner }
%parse-param { void *scanner }
%parse-param { symbol_t table }
%parse-param { ast_program_t *prgm }

%union {
    ast_program_t prgm;
    ast_node_t exp;
    symbol_t id;
    int num;
}

/* declare tokens */
%token                  IF IN LET ELSE PROC THEN ZERO LETREC
%token  <id>            IDENTIFIER
%token  <num>           NUMBER
%type   <exp>           expression const_exp var_exp proc_exp letrec_exp zero_exp if_exp let_exp diff_exp call_exp
%type   <prgm>          program

%destructor { ast_free($$); } expression
%%

program:        expression
                { $$ = new_ast_program($1); *prgm = $$; }
                ;

expression:     const_exp
        |       var_exp
        |       proc_exp
        |       letrec_exp
        |       zero_exp
        |       if_exp
        |       let_exp
        |       diff_exp
        |       call_exp
                ;

const_exp:      NUMBER { $$ = new_const_node($1); }
                ;

var_exp:        IDENTIFIER { $$ = new_var_node($1); }
                ;

proc_exp:       PROC '(' IDENTIFIER ')' expression
                { $$ = new_proc_node($3, $5); }
                ;

letrec_exp:     LETREC IDENTIFIER '(' IDENTIFIER ')' '=' expression IN expression
                { $$ = new_letrec_node($2, $4, $7, $9); }
                ;

zero_exp:       ZERO '(' expression ')'
                { $$ = new_zero_node($3); }
                ;

if_exp:         IF expression THEN expression ELSE expression
                { $$ = new_if_node($2, $4, $6); }
                ;

let_exp:        LET IDENTIFIER '=' expression IN expression
                { $$ = new_let_node($2, $4, $6); }
                ;

diff_exp:       '-' '(' expression ',' expression ')'
                { $$ = new_diff_node($3, $5); }
                ;

call_exp:       '(' expression expression ')'
                { $$ = new_call_node($2, $3); }
                ;

%%

void yyerror(void *lex, symbol_t table, ast_program_t *prgm, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
}

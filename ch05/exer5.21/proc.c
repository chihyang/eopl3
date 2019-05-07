#include <stdio.h>
#include "proc.h"
#include "proc_parser.h"
#include "proc_scanner.h"

struct ast_program_s {

};

struct ast_node_s {
    exp_type type;
    ast_node_t left;
    ast_node_t right;
};

typedef struct ast_const_s {
    exp_type type;
    int num;
} ast_const_s, *ast_const_t;

typedef struct ast_var_s {
    exp_type type;
    symbol_t var;
} ast_var_s, *ast_var_t;

typedef struct ast_proc_s {
    exp_type type;
    symbol_t var;
    ast_node_t body;
} ast_proc_s, *ast_proc_t;

typedef struct symbol_s *symbol_t;

ast_node_t new_const_node(int num) {

}

ast_node_t new_var_node(symbol_t id);
ast_node_t new_proc_node(symbol_t var, ast_node_t body);
ast_node_t new_letrec_node(
    symbol_t p_name, symbol_t p_var, ast_node_t p_body, ast_node_t letrec_body);
ast_node_t new_zero(ast_node_t exp);
ast_node_t new_if_node(ast_node_t cond, ast_node_t exp1, ast_node_t exp2);
ast_node_t new_let_node(symbol_t id, ast_node_t exp1, ast_node_t exp2);
ast_node_t new_diff_node(ast_node_t exp1, ast_node_t exp2);
ast_node_t new_call_node(ast_node_t exp1, ast_node_t exp2);

int main(int argc, char *argv[]) {
    yyscan_t yyscanner;
    if (yylex_init(&yyscanner) == 0) {
        yyparse();
        yylex_destroy(yyscanner);
        return 0;
    } else {
        fprintf(stderr, "Failed to initialize scanner!\n");
        exit(1);
    }
}

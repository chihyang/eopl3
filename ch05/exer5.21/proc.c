#include <stdio.h>
#include <stdlib.h>
#include "proc.h"
#include "proc_parser.h"
#include "proc_scanner.h"

typedef struct ast_program_s {
    ast_node_t exp;
} ast_program_s, *ast_program_t;

typedef struct ast_node_s {
    exp_type type;
    ast_node_t l;
    ast_node_t r;
} ast_node_s, *ast_node_t;

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

typedef struct symbol_s {
    char* name;
} symbol_s, *symbol_t;

ast_program_t new_ast_program(ast_node_t exp) {
    ast_program_t p = malloc(sizeof(ast_program_s));
    if (p) {
        p->exp = exp;
        return p;
    } else {
        fprintf(stderr, "failed to create a new program ast!\n");
        abort();
    }
}

ast_node_t new_const_node(int num) {
    ast_const_t e = malloc(sizeof(ast_const_s));
    if (e) {
        e->type = CONST_EXP;
        e->num = num;
        return (ast_node_t)e;
    } else {
        fprintf(stderr, "failed to create a new const ast node!\n");
        abort();
    }
}

ast_node_t new_var_node(symbol_t id) {
    ast_var_t e = malloc(sizeof(ast_var_s));
    if (e) {
        e->type = VAR_EXP;
        e->id = id;
        return (ast_node_t)e;
    } else {
        fprintf(stderr, "failed to create a new var ast node!\n");
        abort();
    }
}

ast_node_t new_proc_node(symbol_t var, ast_node_t body) {
    ast_proc_t e = malloc(sizeof(ast_proc_s));
    if (e) {
        e->var = var;
        e->body = body;
        return (ast_node_t)e;
    } else {
        fprintf(stderr , "failed to create a new proc ast node!\n");
        abort();
    }
}

ast_node_t new_letrec_node(
    symbol_t p_name, symbol_t p_var, ast_node_t p_body, ast_node_t letrec_body) {

}

ast_node_t new_zero(ast_node_t exp);
ast_node_t new_if_node(ast_node_t cond, ast_node_t exp1, ast_node_t exp2);
ast_node_t new_let_node(symbol_t id, ast_node_t exp1, ast_node_t exp2);
ast_node_t new_diff_node(ast_node_t exp1, ast_node_t exp2);
ast_node_t new_call_node(ast_node_t exp1, ast_node_t exp2);


void ast_program_free(ast_program_t prgm) {
    if (prgm) {
        ast_free(prgm->exp);
    }
}

void ast_free(ast_node_t exp) {
    if (exp) {
        switch (exp->type) {
            case CONST_EXP: {
                const_node_free((ast_const_t)exp);
                break;
            }
            case VAR_EXP: {
                var_node_free((ast_var_t)exp);
                break;
            }
            case PROC_EXP: {
                proc_node_free((ast_proc_t)exp);
                break;
            }
            default:
                break;
        }
    }
}

void const_node_free(ast_const_t exp) {
    free(exp);
}

void var_node_free(ast_var_t exp) {
    if (exp) {
        free(exp->id);
        free(exp);
    }
}

void proc_node_free(ast_var_t exp) {
    if (exp) {
        symbol_free(exp->var);
        ast_free(exp->node);
        free(exp);
    }
}

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

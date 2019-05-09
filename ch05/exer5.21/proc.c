#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "proc.h"
#include "proc_parser.h"
#include "proc_scanner.h"

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

typedef struct ast_letrec_s {
    exp_type type;
    symbol_t p_name;
    symbol_t p_var;
    ast_node_t p_body;
    ast_node_t letrec_body;
} ast_letrec_s, *ast_letrec_t;

typedef struct ast_zero_s {
    exp_type type;
    ast_node_t exp1;
} ast_zero_s, *ast_zero_t;

typedef struct ast_if_s {
    exp_type type;
    ast_node_t cond;
    ast_node_t exp1;
    ast_node_t exp2;
} ast_if_s, *ast_if_t;

typedef struct ast_let_s {
    exp_type type;
    symbol_t id;
    ast_node_t exp1;
    ast_node_t exp2;
} ast_let_s, *ast_let_t;

typedef struct ast_diff_s {
    exp_type type;
    ast_node_t exp1;
    ast_node_t exp2;
} ast_diff_s, *ast_diff_t;

typedef struct ast_call_s {
    exp_type type;
    ast_node_t rator;
    ast_node_t rand;
} ast_call_s, *ast_call_t;

void const_node_free(ast_const_t exp);
void var_node_free(ast_var_t exp);
void proc_node_free(ast_proc_t exp);
void letrec_node_free(ast_letrec_t exp);
void zero_node_free(ast_zero_t exp);
void if_node_free(ast_if_t exp);
void let_node_free(ast_let_t exp);
void diff_node_free(ast_diff_t exp);
void call_node_free(ast_call_t exp);
void report_ast_malloc_fail(const char* node_name);

symbol_t symbol_new(const char* name) {
    symbol_t s = malloc(sizeof(symbol_s));
    if (s) {
        s->name = malloc(strlen(name) + 1);
        if (s->name) {
            strcpy(s->name, name);
            return s;
        } else {
            free(s);
            report_ast_malloc_fail("symbol");
            return NULL;
        }
    } else {
        report_ast_malloc_fail("symbol");
        return NULL;
    }
}

ast_program_t new_ast_program(ast_node_t exp) {
    ast_program_t p = malloc(sizeof(ast_program_s));
    if (p) {
        p->exp = exp;
        return p;
    } else {
        report_ast_malloc_fail("program");
        return NULL;
    }
}

ast_node_t new_const_node(int num) {
    ast_const_t e = malloc(sizeof(ast_const_s));
    if (e) {
        e->type = CONST_EXP;
        e->num = num;
        return (ast_node_t)e;
    } else {
        report_ast_malloc_fail("const");
        return NULL;
    }
}

ast_node_t new_var_node(symbol_t id) {
    ast_var_t e = malloc(sizeof(ast_var_s));
    if (e) {
        e->type = VAR_EXP;
        e->var = id;
        return (ast_node_t)e;
    } else {
        report_ast_malloc_fail("var");
        return NULL;
    }
}

ast_node_t new_proc_node(symbol_t var, ast_node_t body) {
    ast_proc_t e = malloc(sizeof(ast_proc_s));
    if (e) {
        e->var = var;
        e->body = body;
        return (ast_node_t)e;
    } else {
        report_ast_malloc_fail("proc");
        return NULL;
    }
}

ast_node_t new_letrec_node(
    symbol_t p_name, symbol_t p_var, ast_node_t p_body, ast_node_t letrec_body) {
    ast_letrec_t e = malloc(sizeof(ast_letrec_s));
    if (e) {
        e->type = LETREC_EXP;
        e->p_name = p_name;
        e->p_var = p_var;
        e->p_body = p_body;
        e->letrec_body = letrec_body;
        return (ast_node_t)e;
    } else {
        report_ast_malloc_fail("letrec");
        return NULL;
    }
}

ast_node_t new_zero_node(ast_node_t exp) {
    ast_zero_t e = malloc(sizeof(ast_zero_s));
    if (e) {
        e->type = ZERO_EXP;
        e->exp1 = exp;
        return (ast_node_t)e;
    } else {
        report_ast_malloc_fail("zero");
        return NULL;
    }
}

ast_node_t new_if_node(ast_node_t cond, ast_node_t exp1, ast_node_t exp2) {
    ast_if_t e = malloc(sizeof(ast_if_s));
    if (e) {
        e->type = IF_EXP;
        e->cond = cond;
        e->exp1 = exp1;
        e->exp2 = exp2;
        return (ast_node_t)e;
    } else {
        report_ast_malloc_fail("if");
        return NULL;
    }
}

ast_node_t new_let_node(symbol_t id, ast_node_t exp1, ast_node_t exp2) {
    ast_let_t e = malloc(sizeof(ast_let_s));
    if (e) {
        e->type = LET_EXP;
        e->id = id;
        e->exp1 = exp1;
        e->exp2 = exp2;
        return (ast_node_t)e;
    } else {
        report_ast_malloc_fail("let");
        return NULL;
    }
}

ast_node_t new_diff_node(ast_node_t exp1, ast_node_t exp2) {
    ast_diff_t e = malloc(sizeof(ast_diff_t));
    if (e) {
        e->type = DIFF_EXP;
        e->exp1 = exp1;
        e->exp2 = exp2;
        return (ast_node_t)e;
    } else {
        report_ast_malloc_fail("diff");
        return NULL;
    }
}

ast_node_t new_call_node(ast_node_t exp1, ast_node_t exp2) {
    ast_call_t e = malloc(sizeof(ast_call_s));
    if (e) {
        e->type = CALL_EXP;
        e->rand = exp1;
        e->rator = exp2;
        return (ast_node_t)e;
    } else {
        report_ast_malloc_fail("call");
        return NULL;
    }
}

void report_ast_malloc_fail(const char* node_name) {
    fprintf(stderr, "failed to create a new %s ast node!\n", node_name);
    abort();
}

void ast_program_free(ast_program_t prgm) {
    if (prgm) {
        ast_free(prgm->exp);
        free(prgm);
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
            case LETREC_EXP: {
                letrec_node_free((ast_letrec_t)exp);
                break;
            }
            case ZERO_EXP: {
                zero_node_free((ast_zero_t)exp);
                break;
            }
            case IF_EXP: {
                if_node_free((ast_if_t)exp);
                break;
            }
            case LET_EXP: {
                let_node_free((ast_let_t)exp);
                break;
            }
            case DIFF_EXP: {
                diff_node_free((ast_diff_t)exp);
                break;
            }
            case CALL_EXP: {
                call_node_free((ast_call_t)exp);
                break;
            }
            default: {
                fprintf(stderr, "Unknown type of exp: %d", exp->type);
                abort();
            }
        }
    }
}

void const_node_free(ast_const_t exp) {
    free(exp);
}

void var_node_free(ast_var_t exp) {
    symbol_free(exp->var);
    free(exp);
}

void proc_node_free(ast_proc_t exp) {
    symbol_free(exp->var);
    ast_free(exp->body);
    free(exp);
}

void letrec_node_free(ast_letrec_t exp) {
    symbol_free(exp->p_name);
    symbol_free(exp->p_var);
    ast_free(exp->p_body);
    ast_free(exp->letrec_body);
    free(exp);
}

void zero_node_free(ast_zero_t exp) {
    ast_free(exp->exp1);
    free(exp);
}

void if_node_free(ast_if_t exp) {
    ast_free(exp->cond);
    ast_free(exp->exp1);
    ast_free(exp->exp2);
    free(exp);
}

void let_node_free(ast_let_t exp) {
    symbol_free(exp->id);
    ast_free(exp->exp1);
    ast_free(exp->exp2);
    free(exp);
}

void diff_node_free(ast_diff_t exp) {
    ast_free(exp->exp1);
    ast_free(exp->exp2);
    free(exp);
}

void call_node_free(ast_call_t exp) {
    ast_free(exp->rator);
    ast_free(exp->rand);
    free(exp);
}

void symbol_free(symbol_t id) {
    if (id) {
        free(id->name);
        free(id);
    }
}

int main(int argc, char *argv[]) {
    yyscan_t scaninfo = NULL;
    ast_program_t prgm = NULL;
    if (yylex_init(&scaninfo) == 0) {
        yyparse(scaninfo, &prgm);
        yylex_destroy(scaninfo);
        ast_program_free(prgm);
        return 0;
    } else {
        fprintf(stderr, "Failed to initialize scanner!\n");
        exit(1);
    }
}

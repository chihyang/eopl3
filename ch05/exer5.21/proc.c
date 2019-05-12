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

typedef struct proc_s {
    symbol_t id;
    ast_node_t body;
    env_t env;
} proc_s, *proc_t;

typedef struct exp_val_s {
    EXP_VAL type;
    union {
        boolean_t bv;
        int iv;
        proc_t pv;
    };
} exp_val_s, *exp_val_t;

typedef struct env_s {
    ENV_TYPE type;
} env_s, *env_t;

typedef struct extend_env_s {
    ENV_TYPE type;
    symbol_t var;
    exp_val_t val;
    env_t env;
} extend_env_s, *extend_env_t;

typedef struct extend_rec_env_s {
    ENV_TYPE type;
    symbol_t p_name;
    symbol_t p_var;
    ast_node_t p_body;
    exp_val_t proc_val;
    env_t env;
} extend_rec_env_s, *extend_rec_env_t;

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
void report_exp_val_malloc_fail(const char *val_type);
void report_invalid_exp_val(const char *val_type);
void report_no_binding_found(symbol_t search_var);
void report_invalid_env(env_t env);

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
        e->type = PROC_EXP;
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
    ast_diff_t e = malloc(sizeof(ast_diff_s));
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
        e->rator = exp1;
        e->rand = exp2;
        return (ast_node_t)e;
    } else {
        report_ast_malloc_fail("call");
        return NULL;
    }
}

void report_ast_malloc_fail(const char* node_name) {
    fprintf(stderr, "failed to create a new %s ast node!\n", node_name);
    exit(1);
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
                exit(1);
            }
        }
    }
}

void const_node_free(ast_const_t exp) {
    free(exp);
}

void var_node_free(ast_var_t exp) {
    free(exp);
}

void proc_node_free(ast_proc_t exp) {
    ast_free(exp->body);
    free(exp);
}

void letrec_node_free(ast_letrec_t exp) {
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

proc_t new_proc(symbol_t id, ast_node_t body, env_t env) {
    proc_t p = malloc(sizeof(proc_s));
    if (p) {
        p->id = id;
        p->body = body;
        p->env = env;
        return p;
    } else {
        report_exp_val_malloc_fail("procedure");
        return NULL;
    }
}

void proc_free(proc_t p) {
    free(p);
}

exp_val_t new_bool_val(boolean_t val) {
    exp_val_t ev = malloc(sizeof(exp_val_s));
    if (ev) {
        ev->type = BOOL_VAL;
        ev->bv = val;
        return ev;
    } else {
        report_exp_val_malloc_fail("bool");
        return NULL;
    }
}

exp_val_t new_int_val(int val) {
    exp_val_t ev = malloc(sizeof(exp_val_s));
    if (ev) {
        ev->type = NUM_VAL;
        ev->iv = val;
        return ev;
    } else {
        report_exp_val_malloc_fail("num");
        return NULL;
    }
}

exp_val_t new_proc_val(proc_t val) {
    exp_val_t ev = malloc(sizeof(exp_val_s));
    if (ev) {
        ev->type = PROC_VAL;
        ev->pv = val;
        return ev;
    } else {
        report_exp_val_malloc_fail("procedure");
        return NULL;
    }
}

exp_val_t copy_exp_val(exp_val_t val) {
    exp_val_t cv = malloc(sizeof(exp_val_s));
    if (cv) {
        if (val->type == PROC_VAL) {
            cv->type = PROC_VAL;
            cv->pv = new_proc(val->pv->id, val->pv->body, val->pv->env);
        } else {
            memcpy(cv, val, sizeof(*val));
        }
        return cv;
    } else {
        fprintf(stderr, "failed to copy a exp value!\n");
        exit(1);
    }
}

void print_exp_val(exp_val_t val) {
    switch (val->type) {
        case NUM_VAL: {
            printf("%d\n", val->iv);
            break;
        }
        case BOOL_VAL: {
            printf("%s\n", val->bv == TRUE ? "#t" : "#f");
            break;
        }
        case PROC_VAL: {
            printf("(procedure (%s) ...)\n", val->pv->id->name);
            break;
        }
        default: {
            fprintf(stderr, "not a valid exp val: %d!\n", val->type);
            break;
        }
    }
}

void exp_val_free(exp_val_t val) {
    if (val) {
        if (val->type == PROC_VAL) {
            proc_free(val->pv);
        }
        free(val);
    }
}

boolean_t expval_to_bool(exp_val_t val) {
    if (val->type == BOOL_VAL) {
        return val->bv;
    } else {
        report_invalid_exp_val("boolean");
        exit(1);
    }
}

int expval_to_int(exp_val_t val) {
    if (val->type == NUM_VAL) {
        return val->iv;
    } else {
        report_invalid_exp_val("number");
        exit(1);
    }
}

proc_t expval_to_proc(exp_val_t val) {
    if (val->type == PROC_VAL) {
        return val->pv;
    } else {
        report_invalid_exp_val("procedure");
        exit(1);
    }
}

env_t empty_env() {
    env_t env = malloc(sizeof(env_s));
    if (env) {
        env->type = EMPTY_ENV;
        return env;
    } else {
        fprintf(stderr, "failed to create a new empty env!\n");
        exit(1);
    }
}

env_t extend_env(symbol_t var, exp_val_t val, env_t env) {
    extend_env_t e = malloc(sizeof(extend_env_s));
    if (e) {
        e->type = EXTEND_ENV;
        e->var = var;
        e->val = val;
        e->env = env;
        return (env_t)e;
    } else {
        fprintf(stderr, "failed to create a new extend env!\n");
        exit(1);
    }
}

env_t extend_env_rec(symbol_t p_name, symbol_t p_var, ast_node_t p_body, env_t env) {
    extend_rec_env_t e = malloc(sizeof(extend_rec_env_s));
    if (e) {
        e->type = EXTEND_REC_ENV;
        e->p_name = p_name;
        e->p_var = p_var;
        e->p_body = p_body;
        e->proc_val = NULL;
        e->env = env;
        return (env_t)e;
    } else {
        fprintf(stderr, "failed to create a new extend rec env!\n");
        exit(1);
    }
}

exp_val_t apply_env(env_t env, symbol_t var) {
    switch (env->type) {
        case EMPTY_ENV: {
            report_no_binding_found(var);
            exit(1);
        }
        case EXTEND_ENV: {
            extend_env_t e = (extend_env_t)env;
            if (strcmp(e->var->name, var->name) == 0) {
                return e->val;
            } else {
                return apply_env(e->env, var);
            }
        }
        case EXTEND_REC_ENV: {
            extend_rec_env_t e = (extend_rec_env_t)env;
            if (strcmp(e->p_name->name, var->name) == 0) {
                if (e->proc_val) {
                    return e->proc_val;
                } else {
                    e->proc_val = new_proc_val(new_proc(e->p_var, e->p_body, env));
                    return e->proc_val;
                }
            } else {
                return apply_env(e->env, var);
            }
        }
        default: {
            report_invalid_env(env);
            exit(1);
        }
    }
}

env_t env_pop(env_t env) {
    switch (env->type) {
        case EMPTY_ENV: {
            free(env);
            return NULL;
        }
        case EXTEND_ENV: {
            extend_env_t e = (extend_env_t)env;
            exp_val_free(e->val);
            env_t next = e->env;
            free(e);
            return next;
        }
        case EXTEND_REC_ENV: {
            extend_rec_env_t e = (extend_rec_env_t)env;
            exp_val_free(e->proc_val);
            env_t next = e->env;
            free(e);
            return next;
        }
        default: {
            report_invalid_env(env);
            exit(1);
        }
    }
}

void value_of_program(ast_program_t prgm) {
    env_t e = empty_env();
    env_t *current_env = &e;
    exp_val_t val = value_of(prgm->exp, current_env);
    print_exp_val(val);
    exp_val_free(val);
    while(*current_env) {
        *current_env = env_pop(*current_env);
    }
}

exp_val_t value_of(ast_node_t node, env_t *env) {
    switch (node->type) {
        case CONST_EXP: {
            ast_const_t exp = (ast_const_t)node;
            return new_int_val(exp->num);
        }
        case VAR_EXP: {
            ast_var_t exp = (ast_var_t)node;
            return copy_exp_val(apply_env(*env, exp->var));
        }
        case PROC_EXP: {
            ast_proc_t exp = (ast_proc_t)node;
            return new_proc_val(new_proc(exp->var, exp->body, *env));
        }
        case LETREC_EXP: {
            ast_letrec_t exp = (ast_letrec_t)node;
            *env = extend_env_rec(exp->p_name, exp->p_var, exp->p_body, *env);
            exp_val_t val = value_of(exp->letrec_body, env);
            *env = env_pop(*env);
            return val;
        }
        case ZERO_EXP: {
            ast_zero_t exp = (ast_zero_t)node;
            exp_val_t val1 = value_of(exp->exp1, env);
            if (expval_to_int(val1) == 0) {
                exp_val_free(val1);
                return new_bool_val(TRUE);
            } else {
                exp_val_free(val1);
                return new_bool_val(FALSE);
            }
        }
        case IF_EXP: {
            ast_if_t exp = (ast_if_t)node;
            exp_val_t val1 = value_of(exp->cond, env);
            if (expval_to_bool(val1)) {
                exp_val_free(val1);
                return value_of(exp->exp1, env);
            } else {
                exp_val_free(val1);
                return value_of(exp->exp2, env);
            }
        }
        case LET_EXP: {
            ast_let_t exp = (ast_let_t)node;
            exp_val_t val1 = value_of(exp->exp1, env);
            *env = extend_env(exp->id, val1, *env);
            exp_val_t val2 = value_of(exp->exp2, env);
            *env = env_pop(*env);
            return val2;
        }
        case DIFF_EXP: {
            ast_diff_t exp = (ast_diff_t)node;
            exp_val_t val1 = value_of(exp->exp1, env);
            exp_val_t val2 = value_of(exp->exp2, env);
            int diff_val = expval_to_int(val1) - expval_to_int(val2);
            exp_val_free(val2);
            exp_val_free(val1);
            return new_int_val(diff_val);
        }
        case CALL_EXP: {
            ast_call_t exp = (ast_call_t)node;
            exp_val_t rator_val = value_of(exp->rator, env);
            exp_val_t rand_val = value_of(exp->rand, env);
            exp_val_t call_val = apply_procedure(expval_to_proc(rator_val),
                                                 copy_exp_val(rand_val));
            exp_val_free(rand_val);
            exp_val_free(rator_val);
            return call_val;
        }
        default: {
            fprintf(stderr, "unknown type of expression: %d\n", node->type);
            exit(1);
        }
    }
}

exp_val_t apply_procedure(proc_t proc1, exp_val_t val) {
    env_t env = extend_env(proc1->id, val, proc1->env);
    env_t *current_env = &env;
    exp_val_t call_val = value_of(proc1->body, current_env);
    env_pop(*current_env);
    return call_val;
}

void report_exp_val_malloc_fail(const char *val_type) {
    fprintf(stderr, "failed to create a new %s exp value!\n", val_type);
    exit(1);
}

void report_invalid_exp_val(const char *val_type) {
    fprintf(stderr, "not a valid exp val of type %s!\n", val_type);
}

void report_no_binding_found(symbol_t search_var) {
    fprintf(stderr, "no binding for %s\n", search_var->name);
}

void report_invalid_env(env_t env) {
    fprintf(stderr, "bad environment: %p", env);
}

int main(int argc, char *argv[]) {
    yyscan_t scaninfo = NULL;
    ast_program_t prgm = NULL;
    if (yylex_init_extra(symtab, &scaninfo) == 0) {
        int v = yyparse(scaninfo, symtab, &prgm);
        if (v == 0) {
            value_of_program(prgm);
            ast_program_free(prgm);
        }
        yylex_destroy(scaninfo);
        symbol_table_free(symtab);
        return 0;
    } else {
        fprintf(stderr, "Failed to initialize scanner!\n");
        exit(1);
    }
}

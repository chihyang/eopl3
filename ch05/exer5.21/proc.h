#ifndef __PROC_LANG_H__
#define __PROC_LANG_H__

/* symbol */
typedef struct symbol_s {
    char* name;
} symbol_s, *symbol_t;

symbol_t symbol_new(const char* name);
void symbol_free(symbol_t sym);

/* abstract tree */
typedef enum {
    CONST_EXP = 0x01,
    VAR_EXP,
    PROC_EXP,
    LETREC_EXP,
    ZERO_EXP,
    IF_EXP,
    LET_EXP,
    DIFF_EXP,
    CALL_EXP
} exp_type;

typedef struct ast_node_s {
    exp_type type;
} ast_node_s, *ast_node_t;

typedef struct ast_program_s {
    ast_node_t exp;
} ast_program_s, *ast_program_t;


ast_program_t new_ast_program(ast_node_t exp);
ast_node_t new_const_node(int num);
ast_node_t new_var_node(symbol_t id);
ast_node_t new_proc_node(symbol_t var, ast_node_t body);
ast_node_t new_letrec_node(
    symbol_t p_name, symbol_t p_var, ast_node_t p_body, ast_node_t letrec_body);
ast_node_t new_zero_node(ast_node_t exp);
ast_node_t new_if_node(ast_node_t cond, ast_node_t exp1, ast_node_t exp2);
ast_node_t new_let_node(symbol_t id, ast_node_t exp1, ast_node_t exp2);
ast_node_t new_diff_node(ast_node_t exp1, ast_node_t exp2);
ast_node_t new_call_node(ast_node_t exp1, ast_node_t exp2);

void ast_free(ast_node_t ast);
void ast_program_free(ast_program_t prgm);

/* environment */
typedef struct env_s *env_t;

/* procedure */
typedef struct proc_s *proc_t;
proc_t new_proc(symbol_t id, ast_node_t body, env_t env);

/* expressed value */
typedef struct exp_val_s *exp_val_t;
exp_val_t new_bool_val(int val);
exp_val_t new_int_val(int val);
exp_val_t new_proc_val(proc_t val);

/* continuation */
typedef struct continuation_s *continuation_t;
/* bounce */
typedef struct bounce_s *bounce_t;
/* value-of/k */
bounce_t value_of_k(ast_node_t node, env_t env, continuation_t cont);
bounce_t apply_cont(exp_val_t val, continuation_t cont);
bounce_t apply_env(env_t env, symbol_t var);
bounce_t apply_procedure(proc_t proc1, exp_val_t val, continuation_t cont);
struct exp_val_s trampoline(bounce_t bnc);
void value_of_program(ast_program_t prgm);

#endif

#ifndef __PROC_LANG_H__
#define __PROC_LANG_H__

/* symbol */
typedef struct symbol_s {
    char *name;
} symbol_s, *symbol_t;

symbol_t symbol_new(const char *name);
void symbol_free(symbol_t sym);

/* simple symtab of fixed size */
#define NHASH 9997
symbol_s symtab[NHASH];
void symbol_table_free(symbol_t table);
symbol_t symbol_lookup(symbol_t table, char *name);

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
ast_node_t new_letrec_node(symbol_t p_name,
                           symbol_t p_var,
                           ast_node_t p_body,
                           ast_node_t letrec_body);
ast_node_t new_zero_node(ast_node_t exp);
ast_node_t new_if_node(ast_node_t cond, ast_node_t exp1, ast_node_t exp2);
ast_node_t new_let_node(symbol_t id, ast_node_t exp1, ast_node_t exp2);
ast_node_t new_diff_node(ast_node_t exp1, ast_node_t exp2);
ast_node_t new_call_node(ast_node_t exp1, ast_node_t exp2);

void ast_program_free(ast_program_t prgm);
void ast_free(ast_node_t ast);

/* environment */
typedef struct env_s *env_t;

/* procedure */
typedef struct proc_s *proc_t;
proc_t new_proc(symbol_t id, ast_node_t body, env_t env);
void proc_free(proc_t p);

/* expressed value */
typedef enum {
    BOOL_VAL = 0x01,
    NUM_VAL,
    PROC_VAL
} EXP_VAL;

typedef enum {
    FALSE = 0x00,
    TRUE = 0x01
} boolean_t;

typedef struct exp_val_s *exp_val_t;
exp_val_t new_bool_val(boolean_t val);
exp_val_t new_int_val(int val);
exp_val_t new_proc_val(proc_t val);
void exp_val_free(exp_val_t val);
boolean_t expval_to_bool(exp_val_t val);
int expval_to_int(exp_val_t val);
proc_t expval_to_proc(exp_val_t val);
exp_val_t copy_exp_val(exp_val_t val);
void print_exp_val(exp_val_t val);

/* environment */
typedef enum {
    EMPTY_ENV = 0x01,
    EXTEND_ENV,
    EXTEND_REC_ENV
} ENV_TYPE;

env_t empty_env();
env_t extend_env(symbol_t var, exp_val_t val, env_t env);
env_t extend_env_rec(symbol_t p_name, symbol_t p_var, ast_node_t p_body, env_t env);
exp_val_t apply_env(env_t env, symbol_t var);
env_t env_pop(env_t env);

/* continuation */
typedef enum {
    END_CONT = 0x01,
    ZERO1_CONT,
    LET_CONT,
    IF_TEST_CONT,
    DIFF1_CONT,
    DIFF2_CONT,
    RATOR_CONT,
    RAND_CONT,
    LET2_CONT,
    LETREC_CONT,
    APPLY_PROC_CONT,
    APPLY_PROC2_CONT
} CONT_TYPE;

typedef struct continuation_s *continuation_t;

/* bounce */
typedef enum {
    EXPVAL_BOUNCE = 0x01,
    VALUE_OF_BOUNCE
} BOUNCE_TYPE;

typedef struct bounce_s *bounce_t;

/* value-of */
exp_val_t value_of(ast_node_t node, env_t env);
exp_val_t apply_procedure(proc_t proc1, exp_val_t val);
void value_of_program(ast_program_t prgm);

/* value-of/k */
struct bounce_s value_of_k(ast_node_t node, env_t env, continuation_t cont);
struct bounce_s apply_cont(continuation_t cont, exp_val_t val);
struct bounce_s apply_procedure_k(proc_t proc1, exp_val_t val, continuation_t cont);
exp_val_t trampoline(struct bounce_s bnc);
void value_of_program_k(ast_program_t prgm);

/* error reporter */
void yyerror(void *lex, symbol_t table, ast_program_t *prgm, const char *fmt, ...);

#endif

/* simple symbol table from the book bison and flex */
#include <stdlib.h>
#include <string.h>
#include "proc.h"
#include "proc_parser.h"

static unsigned symhash(const char *sym) {
    unsigned int hash = 0;
    unsigned c;
    while ((c = *sym++)) {
        hash = hash * 9 ^ c;
    }
    return hash;
}

symbol_t symbol_lookup(symbol_t table, char *name) {
    symbol_t sp = table + (symhash(name) % NHASH);
    int scount = NHASH; /* how many have we looked at */
    while (--scount >= 0) {
        if (sp->name && !strcmp(sp->name, name)) {
            return sp;
        }
        if (!sp->name) { /* new entry */
            sp->name = malloc(strlen(name) + 1);
            strcpy(sp->name, name);
            return sp;
        }
        if (++sp >= table + NHASH) {
            sp = table; /* try the next entry */
        }
    }
    yyerror(table, "symbol table overflow\n");
    abort(); /* tried them all, table is full */
}

void symbol_table_free(symbol_t table) {
    for (int i = 0; i < NHASH; ++i) {
        free(table[i].name);
    }
}

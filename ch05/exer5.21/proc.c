#include <stdio.h>
#include "proc_parser.h"
#include "proc_scanner.h"

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

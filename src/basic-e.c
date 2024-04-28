/*
 * Tiny BASIC, 2024
 */

#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Տվյալների կառուցվածքներ */

typedef enum _expression_kind {
    NUMBER,
    VARIABLE,
    UNARY,
    BINARY
} expression_kind_t;

typedef struct _expression expression_t;

typedef struct _number {
    double value;
} number_t;

typedef struct _variable { char name; } variable_t;

typedef enum _operation {
    ADD = 0,
    SUB,
    MUL,
    DIV,
    EQ,
    NE,
    GT,
    GE,
    LT,
    LE
} operation_t;

typedef struct _unary {
    operation_t operation;
    expression_t *subexpr;
} unary_t;

typedef struct _binary {
    operation_t operation;
    expression_t *left;
    expression_t *right;
} binary_t;

struct _expression {
    expression_kind_t kind;
    union {
        number_t *number;
        variable_t *variable;
        unary_t *unary;
        binary_t *binary;
    } body;
};

typedef enum _statement_kind {
    END = 0,
    INPUT,
    PRINT,
    LET,
    IF,
    GOTO,
    GOSUB,
    RETURN
} statement_kind_t;

typedef struct _statement statement_t;

typedef struct _end {} end_t;

typedef struct _input {
    variable_t **variables;
} input_t;

typedef struct _print {
    expression_t **expressions;
} print_t;

typedef struct _let {
    variable_t *variable;
    expression_t *right;
} let_t;

typedef struct _if {
    expression_t *condition;
    statement_t *decision;
    statement_t *alternative;
} if_t;

typedef struct _goto {
    expression_t *target;
} goto_t;

typedef struct _gosub {
    expression_t *target;
} gosub_t;

typedef struct _return {} return_t;

struct _statement {
    statement_kind_t kind;
    unsigned int line;
    union {
        end_t *end;
        input_t *input;
        print_t *print;
        let_t *let;
        if_t *ifc;
        goto_t *gotoc;
        gosub_t *gosub;
        return_t *returnc;
    } body;
};


/* Լեքսեմը */
typedef enum _token {
    T_NONE,
    T_INTEGER,
    T_REAL,
    T_NAME,
    T_END,
    T_INPUT,
    T_PRINT,
    T_LET,
    T_IF,
    T_GOTO,
    T_GOSUB,
    T_RETURN,
    T_ADD,
    T_SUB,
    T_MUL,
    T_DIV,
    T_EQ,
    T_NE,
    T_GT,
    T_GE,
    T_LT,
    T_LE,
    T_EOL,
    T_EOF
} token_t;

typedef struct _lexeme {
    token_t token;
    union {
        char name;
        double number;
        size_t label;
    } value;
} lexeme_t;

/* Շարայուսական վերլուծություն */
#define BUFFER_SIZE 128

typedef struct _scanner {
    FILE *source;
} scanner_t;

scanner_t *create_scanner(const char *file)
{
    scanner_t *scanner = malloc(sizeof(scanner_t));
    if( scanner == NULL ) return NULL;

    FILE *fp = fopen(file, "r");
    if( fp == NULL ) return NULL;

    scanner->source = fp;

    return scanner;
}

lexeme_t next_lexeme(scanner_t *scanner)
{
    lexeme_t lex = { .token = T_NONE };

    FILE *fp = scanner->source;
    char c = fgetc(fp);

    while( isspace(c) )
        c = fgetc(fp);

    if( isdigit(c) ) {
        int number = 0;
        do {
            number = number * 10 + (c - '0');
            c = fgetc(fp);
        } while( isdigit(c) );
        if( '.' == c ) {
            double k = 1;
            while( isdigit(c = fgetc(fp)) ) {
                number = number * 10 + (c - '0');
                k *= 10;
            }
            lex.token = T_REAL;
            lex.value.number = number / k;
        }
        else {
            lex.token = T_INTEGER;
            lex.value.number = number;
        }
    }
    else if( isalpha(c) ) {
        
    }

    return lex;
}

typedef struct _parser {
    scanner_t *scanner;
    lexeme_t lookehead;
} parser_t;

parser_t *create_parser(scanner_t *scanner)
{
    parser_t *parser = malloc(sizeof(parser_t));
    parser->scanner = scanner;
    return parser;
}

void parse(parser_t *parser)
{
    // TODO
}


int main(int argc, char **argv)
{
    printf("Tiny BASIC, 2024\n");

    scanner_t *scanner = create_scanner("../cases/case01.bas");
    next_lexeme(scanner);

    return 0;
}

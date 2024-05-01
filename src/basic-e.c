/*
 * Tiny BASIC, 2024
 */

#include <ctype.h>
#include <math.h>
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

bool is_keyword(lexeme_t *lex, token_t tok)
{
    return lex->token == tok;
}

bool is_integer(lexeme_t *lex, int val)
{
    return T_INTEGER == lex->token && val == (int)lex->value.number;
}

bool is_real(lexeme_t *lex, double val)
{
    return T_REAL == lex->token && fabs(val - lex->value.number) < 1e-16;
}


/* Շարայուսական վերլուծություն */
#define BUFFER_SIZE 128

typedef struct _scanner {
    FILE *source;
    char ch;
} scanner_t;

scanner_t *create_scanner(const char *file)
{
    scanner_t *scanner = malloc(sizeof(scanner_t));
    if( scanner == NULL ) return NULL;

    FILE *fp = fopen(file, "r");
    if( fp == NULL ) return NULL;

    scanner->source = fp;
    scanner->ch = fgetc(scanner->source);

    return scanner;
}

bool advance(scanner_t *scanner)
{
    if( feof(scanner->source) )
        return false;

    scanner->ch = fgetc(scanner->source);
    return true;
}

void retreat(scanner_t *scanner)
{
    ungetc(scanner->ch, scanner->source);
}

lexeme_t scan_number(scanner_t *scanner)
{
    int number = 0;
    do {
        number = number * 10 + (scanner->ch - '0');
        advance(scanner);
    } while( isdigit(scanner->ch) );
    if( '.' != scanner->ch ) {
        retreat(scanner);
        return (lexeme_t){ .token = T_INTEGER, .value.number = number };
    }

    double k = 1;
    advance(scanner);
    while( isdigit(scanner->ch) ) {
        number = number * 10 + (scanner->ch - '0');
        k *= 10;
        advance(scanner);
    }
    retreat(scanner);
    return (lexeme_t){ .token = T_REAL, .value.number = number / k };
}

lexeme_t scan_identifier_or_name(scanner_t *scanner)
{
    char buffer[40] = { 0 };
    char *p = buffer;
    do {
        *p = scanner->ch;
        ++p;
        advance(scanner);
    } while( isalpha(scanner->ch) );
    retreat(scanner);

    if( strlen(buffer) == 1 )
        return (lexeme_t){ .token = T_NAME, .value.name = buffer[0] };

    lexeme_t kw = { .token = T_NONE };
    if( 0 == strcmp("END", buffer) )
        kw.token = T_END;
    else if( 0 == strcmp("INPUT", buffer) )
        kw.token = T_INPUT;
    else if( 0 == strcmp("PRINT", buffer) )
        kw.token = T_PRINT;
    else if( 0 == strcmp("LET", buffer) )
        kw.token = T_LET;
    else if( 0 == strcmp("IF", buffer) )
        kw.token = T_IF;
    else if( 0 == strcmp("GOTO", buffer) )
        kw.token = T_GOTO;
    else if( 0 == strcmp("GOSUB", buffer) )
        kw.token = T_GOSUB;
    else if( 0 == strcmp("RETURN", buffer) )
        kw.token = T_RETURN;

    return kw;
}

// lexeme_t scan_operation(scanner_t *scanner)
// {

// }

lexeme_t next_lexeme(scanner_t *scanner)
{
    while( ' ' == scanner->ch || '\t' == scanner->ch )
        advance(scanner);

    if( feof(scanner->source) )
        return (lexeme_t){ .token = T_EOF };

    if( '\n' == scanner->ch ) {
        while( '\n' == scanner->ch )
            advance(scanner);
        return (lexeme_t){ .token = T_EOL };
    }

    if( isdigit(scanner->ch) ) 
        return scan_number(scanner);

    if( isalpha(scanner->ch) )
        return scan_identifier_or_name(scanner);

    // գործողություններ
    token_t token = T_NONE;
    switch( scanner->ch ) {
        case '+':
            token = T_ADD;
            break;
        case '-':
            token = T_SUB;
            break;
        case '*':
            token = T_MUL;
            break;
        case '/':
            token = T_DIV;
            break;
        case '=':
            token = T_EQ;
            break;
        case '>':
            advance(scanner);
            if( '=' == scanner->ch )
                token = T_GE;
            else {
                retreat(scanner);
                token = T_GT;
            }
            break;
        case '<':
            advance(scanner);
            if( '=' == scanner->ch )
                token = T_LE;
            else if( '>' == scanner->ch )
                token = T_NE;
            else {
                retreat(scanner);
                token = T_LT;
            }
            break;
    }
    advance(scanner);
    return (lexeme_t){ .token = token };
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


/* TESTS */
#define EXPECT(a) if(!(a)) fprintf(stderr, "FAILED: %s\n", #a)

void test_scanner()
{
    scanner_t *scanner = create_scanner("../cases/case01.bas");
    EXPECT(scanner != NULL);

    lexeme_t lex = next_lexeme(scanner);
    EXPECT(is_integer(&lex, 10));

    lex = next_lexeme(scanner);
    EXPECT(is_keyword(&lex, T_PRINT));

    lex = next_lexeme(scanner);
    EXPECT(is_real(&lex, 3.1415));

    lex = next_lexeme(scanner);
    EXPECT(lex.token == T_EOL);

    lex = next_lexeme(scanner);
    EXPECT(is_integer(&lex, 20));

    lex = next_lexeme(scanner);
    EXPECT(is_keyword(&lex, T_END));

    lex = next_lexeme(scanner);
    EXPECT(lex.token == T_EOL);

    lex = next_lexeme(scanner);
    EXPECT(lex.token == T_EOF);
}



int main(int argc, char **argv)
{
    printf("Tiny BASIC, 2024\n");

    test_scanner();

    return 0;
}

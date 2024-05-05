/*
 * Tiny BASIC, 2024

line = number statement NL | statement NL.
statement = PRINT expr-list
    | IF expression relop expression THEN statement
    | GOTO expression
    | INPUT var-list
    | LET var = expression
    | GOSUB expression
    | RETURN
    | END
expr-list = (string | expression) (, (string | expression) *).
var-list = var (, var)*
expression = (+|-|ε) term ((+|-) term)*
term = factor ( (* | /) factor)*
factor = var | number | (expression)
 */

#include <ctype.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* Դինամիկ զանգված */

typedef struct _vector {
    size_t capacity;
    size_t count;
    void **items;
} vector_t;

vector_t *create_vector(size_t cap)
{
    vector_t *vec = malloc(sizeof(vector_t));
    vec->capacity = cap;
    vec->count = 0;
    vec->items = malloc(vec->capacity * sizeof(void *));
    return vec;
}

void destroy_vector(vector_t *vec)
{
    free(vec->items);
    free(vec);
}

void add_back(vector_t *vec, void *el)
{
    if( vec->count == vec->capacity ) {
        vec->capacity *= 2;
        vec->items = realloc(vec->items, vec->capacity * sizeof(void *));
    }

    ++vec->count;
    vec->items[vec->count] = el;
}


/* Տվյալների կառուցվածքներ */

typedef enum _expression_kind {
    NUMBER,
    VARIABLE,
    UNARY,
    BINARY
} expression_kind_t;

typedef struct _expression expression_t;

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
        double number;
        char name;
        unary_t *unary;
        binary_t *binary;
    } value;
};

expression_t *_create_expression(expression_kind_t kind)
{
    expression_t *ex = malloc(sizeof(expression_t));
    ex->kind = kind;
    return ex;
}

expression_t *create_number(double value)
{
    expression_t *ex = _create_expression(NUMBER);
    ex->value.number = value;
    return ex;
}

expression_t *create_variable(char name)
{
    expression_t *ex = _create_expression(VARIABLE);
    ex->value.name = name;
    return ex;
}

expression_t *create_unary(operation_t op, expression_t *se)
{
    expression_t *ex = _create_expression(UNARY);
    ex->value.unary = malloc(sizeof(unary_t));
    ex->value.unary->operation = op;
    ex->value.unary->subexpr = se;
    return ex;
}

expression_t *create_binary(operation_t op, expression_t *l, expression_t *r)
{
    expression_t *ex = _create_expression(BINARY);
    ex->value.binary = malloc(sizeof(binary_t));
    ex->value.binary->operation = op;
    ex->value.binary->left = l;
    ex->value.binary->right = r;
    return ex;
}

void destroy_expression(expression_t *expr);

void destroy_unary(unary_t *ue)
{
    destroy_expression(ue->subexpr);
    free(ue);
}

void destroy_binary(binary_t *be)
{
    destroy_expression(be->left);
    destroy_expression(be->right);
    free(be);
}

void destroy_expression(expression_t *expr)
{
    if( UNARY == expr->kind )
        destroy_unary(expr->value.unary);
    else if( BINARY == expr->kind )
        destroy_binary(expr->value.binary);
    free(expr);
}


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

statement_t *_create_statement(statement_kind_t kind)
{
    statement_t *st = malloc(sizeof(statement_t));
    st->kind = kind;
    return st;
}

statement_t *create_end()
{
    return _create_expression(END);
}

typedef struct _input {
    char *variables;
} input_t;

statement_t *create_input(char *vars)
{
    statement_t *st = _create_statement(INPUT);
    st->body.input = malloc(sizeof(input_t));
    st->body.input->variables = vars;
    return st;
}

typedef struct _print {
    vector_t *expressions;
} print_t;

statement_t *create_print(vector_t *exprs)
{
    statement_t *st = _create_statement(PRINT);
    st->body.print = malloc(sizeof(print_t));
    st->body.print->expressions = exprs;
    return st;
}

typedef struct _let {
    char variable;
    expression_t *right;
} let_t;

statement_t *create_let(char var, expression_t *expr)
{
    statement_t *st = _create_statement(LET);
    st->body.let = malloc(sizeof(let_t));
    st->body.let->variable = var;
    st->body.let->right = expr;
    return st;
}

typedef struct _if {
    expression_t *condition;
    statement_t *decision;
    statement_t *alternative;
} if_t;

statement_t *create_if(expression_t *cond, statement_t *dec, statement_t *alt)
{
    statement_t *st = _create_statement(IF);
    st->body.ifc = malloc(sizeof(if_t));
    st->body.ifc->condition = cond;
    st->body.ifc->decision = dec;
    st->body.ifc->alternative = alt;
    return st;
}

typedef struct _goto {
    expression_t *target;
} goto_t;

statement_t *create_goto(expression_t *tg)
{
    statement_t *st = _create_statement(GOTO);
    st->body.gotoc = malloc(sizeof(goto_t));
    st->body.gotoc->target = tg;
    return st;
}

typedef struct _gosub {
    expression_t *target;
} gosub_t;

statement_t *create_gosub(expression_t *tg)
{
    statement_t *st = _create_statement(GOTO);
    st->body.gosub = malloc(sizeof(gosub_t));
    st->body.gosub->target = tg;
    return st;
}

statement_t *create_return()
{
    return _create_statement(RETURN);
}

struct _statement {
    statement_kind_t kind;
    unsigned int line;
    union {
        input_t *input;
        print_t *print;
        let_t *let;
        if_t *ifc;
        goto_t *gotoc;
        gosub_t *gosub;
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
    T_LPAR,
    T_RPAR,
    T_COMMA,
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
        case '(':
            token = T_LPAR;
            break;
        case ')':
            token = T_RPAR;
            break;
        case ',':
            token = T_COMMA;
            break;
    }
    advance(scanner);
    return (lexeme_t){ .token = token };
}



typedef struct _parser {
    scanner_t *scanner;
    lexeme_t lookahead;
} parser_t;

parser_t *create_parser(scanner_t *scanner)
{
    parser_t *parser = malloc(sizeof(parser_t));
    parser->scanner = scanner;
    return parser;
}

bool match(parser_t *parser, token_t expected)
{
    if( parser->lookahead.token == expected ) {
        parser->lookahead = next_lexeme(parser->scanner);
        return true;
    }

    return false;
}

typedef unsigned int error_code_t;
const error_code_t P_OK = 0;

typedef struct _result {
    void *item;
    error_code_t ec; 
} result_t;

result_t parse_expression(parser_t *parser);

result_t parse_factor(parser_t *parser)
{
    token_t token = parser->lookahead.token;

    if( T_NAME == parser->lookahead.token ) {
        char name = parser->lookahead.value.name;
        match(parser, T_NAME);
        expression_t *vr = create_variable(name);
        return (result_t){ .item = vr, .ec = 0 };
    }

    if( T_INTEGER == token || T_REAL == token ) {
        double num = parser->lookahead.value.number;
        match(parser, token);
        expression_t *nm = create_number(num);
        return (result_t){ .item = nm, .ec = 0 };
    }

    if( T_LPAR == token ) {
        match(parser, T_LPAR);
        result_t rs = parse_expression(parser);
        if( rs.ec != P_OK )
            return rs;
        if( !match(parser, T_RPAR) ) {
            destroy_expression(rs.item);
            return (result_t){ .item = NULL, .ec = 0x0101 };
        }
        return rs;
    }

    return (result_t){ .item = NULL, .ec = 0 };
}

result_t parse_term(parser_t *parser)
{
    result_t rs = parse_factor(parser);
    if( rs.ec != P_OK )
        return rs;

    while( T_MUL == parser->lookahead.token || T_DIV == parser->lookahead.token ) {
        operation_t oper = T_DIV == parser->lookahead.token ? DIV : MUL;
        match(parser, parser->lookahead.token);
        result_t r2 = parse_factor(parser);
        if( r2.ec != P_OK )
            return r2;
        rs.item = create_binary(oper, rs.item, r2.item);
    }

    return rs;
}

result_t parse_expression(parser_t *parser)
{
    operation_t unop = ADD;
    if( T_ADD == parser->lookahead.token )
        match(parser, T_ADD);
    else if( T_SUB == parser->lookahead.token ) {
        unop = SUB;
        match(parser, T_SUB);
    }

    result_t rs = parse_term(parser);
    if( rs.ec != P_OK )
        return rs;
    
    if( SUB == unop )
        rs.item = create_unary(unop, rs.item);

    while( T_ADD == parser->lookahead.token || T_SUB == parser->lookahead.token ) {
        operation_t oper = T_ADD == parser->lookahead.token ? ADD : SUB;
        match(parser, parser->lookahead.token);
        result_t r2 = parse_term(parser);
        if( r2.ec != P_OK )
            return r2;
        rs.item = create_binary(oper, rs.item, r2.item);
    }

    return rs;
}

result_t parse_end(parser_t *parser)
{
    if( !match(parser, T_END) )
        return (result_t){ .item = NULL, .ec = 0x0102 };
    
    // TODO: 
    return (result_t){ .item = NULL, .ec = P_OK };
}

statement_t *parse_statement(parser_t *parser)
{
    switch( parser->lookahead.token ) {
        case T_END:
            break;
        case T_INPUT:
            break;
        case T_PRINT:
            break;
        case T_LET:
            break;
        case T_IF:
            break;
        case T_GOTO:
            break;
        case T_GOSUB:
            break;
        case T_RETURN:
            break;
    }

    return NULL;
}

statement_t *parse_line(parser_t *parser)
{
    if( !match(parser, T_INTEGER) ) return NULL;
    // TODO: parse statement
    if( !match(parser, T_EOL) ) return NULL;
}

void parse(parser_t *parser)
{
    parser->lookahead = next_lexeme(parser->scanner);
    while( parser->lookahead.token == T_INTEGER ) {
        parse_line(parser);
    }
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


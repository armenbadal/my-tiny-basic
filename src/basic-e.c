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
    | S_END
expr-list = (string | expression) (, (string | expression) *).
var-list = var (, var)*
expression = (+|-|ε) term ((+|-) term)*
term = factor ( (* | /) factor)*
factor = var | number | (expression)
 */

#include <ctype.h>
#include <float.h>
#include <math.h>
#include <stdarg.h>
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
    if( NULL == vec ) return NULL;
    vec->capacity = cap;
    vec->count = 0;
    vec->items = malloc(vec->capacity * sizeof(void *));
    if( NULL == vec->items ) {
        free(vec);
        return NULL;
    }
    return vec;
}

bool is_empty(const vector_t *vec)
{
    return vec->count > 0;
}

void *get_elem(const vector_t *vec, size_t index)
{
    return vec->items[index];
}

void *get_last(const vector_t *vec)
{
    return vec->items[vec->count - 1];
}

void add_back(vector_t *vec, void *el)
{
    if( vec->count == vec->capacity ) {
        vec->capacity *= 2;
        vec->items = realloc(vec->items, vec->capacity * sizeof(void *));
    }

    vec->items[vec->count] = el;
    ++vec->count;
}

void *pop_last(vector_t *vec)
{
    if( is_empty(vec) )
        return NULL;

    void *last = get_last(vec);
    --vec->count;
    return last;
}

typedef void(*action_t)(void *);

void for_each_item(const vector_t *vec, action_t f)
{
    for( size_t i = 0; i < vec->count; ++i )
        f(vec->items[i]);
}

void destroy_vector(vector_t *vec)
{
    free(vec->items);
    free(vec);
}

void destroy_vector_and_elements(vector_t *vec, void(*destroyer)(void *))
{
    if( NULL != vec ) {
        for_each_item(vec, destroyer);
        destroy_vector(vec);
    }
}


/* Տվյալների կառուցվածքներ */

typedef enum _expression_kind {
    E_NUMBER,
    E_VARIABLE,
    E_UNARY,
    E_BINARY
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
    if( NULL == ex ) return NULL;
    ex->kind = kind;
    return ex;
}

void destroy_expression(expression_t *expr);

expression_t *create_number(double value)
{
    expression_t *ex = _create_expression(E_NUMBER);
    ex->value.number = value;
    return ex;
}

expression_t *create_variable(char name)
{
    expression_t *ex = _create_expression(E_VARIABLE);
    ex->value.name = name;
    return ex;
}

expression_t *create_unary(operation_t op, expression_t *se)
{
    expression_t *ex = _create_expression(E_UNARY);
    ex->value.unary = malloc(sizeof(unary_t));
    if( NULL == ex->value.unary ) {
        destroy_expression(ex);
        return NULL;
    }
    ex->value.unary->operation = op;
    ex->value.unary->subexpr = se;
    return ex;
}

expression_t *create_binary(operation_t op, expression_t *l, expression_t *r)
{
    expression_t *ex = _create_expression(E_BINARY);
    ex->value.binary = malloc(sizeof(binary_t));
    if( NULL == ex->value.binary ) {
        destroy_expression(ex);
        return NULL;
    }
    ex->value.binary->operation = op;
    ex->value.binary->left = l;
    ex->value.binary->right = r;
    return ex;
}

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
    if( E_UNARY == expr->kind )
        destroy_unary(expr->value.unary);
    else if( E_BINARY == expr->kind )
        destroy_binary(expr->value.binary);
    free(expr);
}


typedef enum _statement_kind {
    S_END = 0,
    S_INPUT,
    S_PRINT,
    S_LET,
    S_IF,
    S_GOTO,
    S_GOSUB,
    S_RETURN
} statement_kind_t;

typedef struct _statement statement_t;

typedef struct _input {
    vector_t *variables;
} input_t;

typedef struct _print {
    vector_t *expressions;
} print_t;

typedef struct _let {
    char variable;
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

/*
#define s_input(s) (s)->body.input
#define s_print(s) (s)->body.print
#define s_let(s) (s)->body.let
#define s_if(s) (s)->body.ifc
#define s_goto(s) (s)->body.gotoc
#define s_gosub(s) (s)->body.gosub
*/

statement_t *_create_statement(statement_kind_t kind)
{
    statement_t *st = malloc(sizeof(statement_t));
    if( NULL == st ) return NULL;
    st->kind = kind;
    return st;
}

void destroy_statement(statement_t *s);

statement_t *create_end()
{
    return _create_statement(S_END);
}

statement_t *create_input(vector_t *vars)
{
    statement_t *st = _create_statement(S_INPUT);
    st->body.input = malloc(sizeof(input_t));
    if( NULL == st->body.input ) {
        destroy_statement(st);
        return NULL;
    }
    st->body.input->variables = vars;
    return st;
}

statement_t *create_print(vector_t *exprs)
{
    statement_t *st = _create_statement(S_PRINT);
    st->body.print = malloc(sizeof(print_t));
    if( NULL == st->body.print ) {
        destroy_statement(st);
        return NULL;
    }
    st->body.print->expressions = exprs;
    return st;
}

statement_t *create_let(char var, expression_t *expr)
{
    statement_t *st = _create_statement(S_LET);
    st->body.let = malloc(sizeof(let_t));
    if( NULL == st->body.let ) {
        destroy_statement(st);
        return NULL;
    }
    st->body.let->variable = var;
    st->body.let->right = expr;
    return st;
}

statement_t *create_if(expression_t *cond, statement_t *dec, statement_t *alt)
{
    statement_t *st = _create_statement(S_IF);
    st->body.ifc = malloc(sizeof(if_t));
    if( NULL == st->body.ifc ) {
        destroy_statement(st);
        return NULL;
    }
    st->body.ifc->condition = cond;
    st->body.ifc->decision = dec;
    st->body.ifc->alternative = alt;
    return st;
}

statement_t *create_goto(expression_t *tg)
{
    statement_t *st = _create_statement(S_GOTO);
    st->body.gotoc = malloc(sizeof(goto_t));
    if( NULL == st->body.gotoc ) {
        destroy_statement(st);
        return NULL;
    }
    st->body.gotoc->target = tg;
    return st;
}

statement_t *create_gosub(expression_t *tg)
{
    statement_t *st = _create_statement(S_GOSUB);
    st->body.gosub = malloc(sizeof(gosub_t));
    if( NULL == st->body.gosub ) {
        destroy_statement(st);
        return NULL;
    }
    st->body.gosub->target = tg;
    return st;
}

statement_t *create_return()
{
    return _create_statement(S_RETURN);
}

void destroy_statement(statement_t *s)
{
    switch( s->kind ) {
        case S_INPUT:
            destroy_vector_and_elements(s->body.input->variables, free);
            free(s->body.input);
            break;
        case S_PRINT:
            destroy_vector_and_elements(s->body.print->expressions,
                                        (action_t)destroy_expression);
            free(s->body.print);
            break;
        case S_LET:
            destroy_expression(s->body.let->right);
            free(s->body.let);
            break;
        case S_IF:
            destroy_expression(s->body.ifc->condition);
            destroy_statement(s->body.ifc->decision);
            //destroy_statement(s->body.ifc->alternative);
            free(s->body.ifc);
            break;
        case S_GOTO:
            destroy_expression(s->body.gotoc->target);
            free(s->body.gotoc);
            break;
        case S_GOSUB:
            destroy_expression(s->body.gosub->target);
            free(s->body.gotoc);
            break;
        case S_END:
        case S_RETURN:
            break;
    }

    free(s);
}


/* Լեքսեմը */
typedef enum _token {
    T_NONE = 0,
    T_INTEGER,
    T_REAL,
    T_NAME,
    T_END,
    T_INPUT,
    T_PRINT,
    T_LET,
    T_IF,
    T_THEN,
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

void destroy_scanner(scanner_t *scanner)
{
    fclose(scanner->source);
    free(scanner);
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
    //retreat(scanner);

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
    else if( 0 == strcmp("THEN", buffer) )
        kw.token = T_THEN;
    else if( 0 == strcmp("GOTO", buffer) )
        kw.token = T_GOTO;
    else if( 0 == strcmp("GOSUB", buffer) )
        kw.token = T_GOSUB;
    else if( 0 == strcmp("RETURN", buffer) )
        kw.token = T_RETURN;

    return kw;
}

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
        default:
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
    if( NULL == parser ) return NULL;
    parser->scanner = scanner;
    return parser;
}

void destroy_parser(parser_t *parser)
{
    free(parser);
}

bool match(parser_t *parser, token_t expected)
{
    if( parser->lookahead.token == expected ) {
        parser->lookahead = next_lexeme(parser->scanner);
        return true;
    }

    return false;
}

bool match_any(parser_t *parser, size_t n, ...)
{
    bool matched = false;

    va_list tokens;
    va_start(tokens, n);

    for( size_t i = 0; i < n; ++i ) {
        token_t token = va_arg(tokens, token_t);
        if( match(parser, token) ) {
            matched = true;
            break;
        }
    }

    va_end(tokens);

    return matched;
}

typedef unsigned int error_code_t;
const error_code_t P_OK = 0;

typedef struct _result {
    void *item;
    error_code_t ec; 
} result_t;

bool failed(const result_t *r)
{
    return P_OK != r->ec;
}

result_t result_with_ptr(void *p)
{
    return (result_t){ .item = p, .ec = P_OK };
}

result_t result_with_error(error_code_t ec)
{
    return (result_t){ .item = NULL, .ec = ec };
}

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

result_t parse_comparison(parser_t *parser)
{
    result_t rl = parse_expression(parser);
    if( failed(&rl) )
        return result_with_error(0x0102);

    token_t token = parser->lookahead.token;
    if( !match_any(parser, 6, T_EQ, T_NE, T_GT, T_GE, T_LT, T_LE) ) {
        destroy_expression(rl.item);
        return result_with_error(0x0102);
    }
    operation_t oper = EQ;
    switch( token ) {
        case T_EQ:
            oper = EQ;
            break;
        case T_NE:
            oper = NE;
            break;
        case T_GT:
            oper = GT;
            break;
        case T_GE:
            oper = GE;
            break;
        case T_LT:
            oper = LT;
            break;
        case T_LE:
            oper = LE;
            break;
        default:
            break;
    }

    result_t rr = parse_expression(parser);
    if( failed(&rr) ) {
        destroy_expression(rl.item);
        return result_with_error(0x0102);
    }

    expression_t *comp = create_binary(oper, rl.item, rr.item);
    if( NULL == comp ) {
        destroy_expression(rl.item);
        destroy_expression(rr.item);
        return result_with_error(0x0102);
    }

    return result_with_ptr(comp);
}

result_t parse_statement(parser_t *parser);

result_t parse_end(parser_t *parser)
{
    if( !match(parser, T_END) )
        return result_with_error(0x0102);
    
    return result_with_ptr(create_end());
}

result_t parse_input(parser_t *parser)
{
    if( !match(parser, T_INPUT) )
        return result_with_error(0x0103);

    vector_t *variables = create_vector(4);

    char name = parser->lookahead.value.name;
    if( !match(parser, T_NAME) )
        return result_with_error(0x0104);
    add_back(variables, create_variable(name));

    while( T_COMMA == parser->lookahead.token ) {
        match(parser, T_COMMA);
        name = parser->lookahead.value.name;
        if( !match(parser, T_NAME) )
            return result_with_error(0x0105);
        add_back(variables, create_variable(name));
    }

    return result_with_ptr(create_input(variables));
}

result_t parse_print(parser_t *parser)
{
    if( !match(parser, T_PRINT) )
        return result_with_error(0x0106);

    result_t rs = parse_expression(parser);
    if( rs.ec != P_OK )
        return result_with_error(0x0107);
    vector_t *exprs = create_vector(4);
    add_back(exprs, rs.item);

    while( T_COMMA == parser->lookahead.token ) {
        match(parser, T_COMMA);
        result_t r2 = parse_expression(parser);
        if( r2.ec != P_OK ) {
            for_each_item(exprs, (action_t)destroy_expression);
            return result_with_error(0x0108);
        }
        add_back(exprs, r2.item);
    }

    return result_with_ptr(create_print(exprs));
}

result_t parse_let(parser_t *parser)
{
    if( !match(parser, T_LET) )
        return result_with_error(0x0109);

    char name = parser->lookahead.value.name;
    if( !match(parser, T_NAME) )
        return result_with_error(0x010a);

    if( !match(parser, T_EQ) )
        return result_with_error(0x010b);

    result_t rs = parse_expression(parser);
    if( rs.ec != P_OK )
        return result_with_error(0x010c);

    return result_with_ptr(create_let(name, rs.item));
}

result_t parse_if(parser_t *parser)
{
    if( !match(parser, T_IF) )
        return result_with_error(0x010d);

    result_t rc = parse_comparison(parser);
    if( failed(&rc) )
        return result_with_error(0x010f);

    if( !match(parser, T_THEN) )
        return result_with_error(0x010f);

    result_t ds = parse_statement(parser);
    if( failed(&ds) ) {
        destroy_expression(rc.item);
        return result_with_error(0x010f);
    }

    statement_t *st = create_if(rc.item, ds.item, NULL);
    if( NULL == st ) {
        destroy_expression(rc.item);
        destroy_statement(ds.item);
        return result_with_error(0x010f);
    }

    return result_with_ptr(st);
}

result_t parse_goto(parser_t* parser)
{
    if( !match(parser, T_GOTO) )
        return result_with_error(0x0110);

    result_t rs = parse_expression(parser);
    if( failed(&rs) )
        return result_with_error(0x111);

    return result_with_ptr(create_goto(rs.item));
}

result_t parse_gosub(parser_t* parser)
{
    if( !match(parser, T_GOSUB) )
        return result_with_error(0x0112);

    result_t rs = parse_expression(parser);
    if( failed(&rs) )
        return result_with_error(0x113);

    return result_with_ptr(create_gosub(rs.item));
}

result_t parse_return(parser_t* parser)
{
    if( !match(parser, T_RETURN) )
        return result_with_error(0x0114);

    return result_with_ptr(create_return());
}

result_t parse_statement(parser_t *parser)
{
    switch( parser->lookahead.token ) {
        case T_END:
            return parse_end(parser);
        case T_INPUT:
            return parse_input(parser);
        case T_PRINT:
            return parse_print(parser);
        case T_LET:
            return parse_let(parser);
        case T_IF:
            return parse_if(parser);
        case T_GOTO:
            return parse_goto(parser);
        case T_GOSUB:
            return parse_gosub(parser);
        case T_RETURN:
            return parse_return(parser);
        default:
            break;
    }

    return result_with_error(0x0115);
}

result_t parse_line(parser_t *parser)
{
    unsigned int line = (unsigned int)parser->lookahead.value.number;
    if( !match(parser, T_INTEGER) )
        return result_with_error(0x0116);
    
    result_t rs = parse_statement(parser);
    if( failed(&rs) )
        return result_with_error(0x0117);

    statement_t *st = (statement_t *)rs.item;
    st->line = line;

    if( !match(parser, T_EOL) )
        return result_with_error(0x0118);

    return result_with_ptr(st);
}

result_t parse(parser_t *parser)
{
    vector_t *program = create_vector(16);

    parser->lookahead = next_lexeme(parser->scanner);
    while( parser->lookahead.token == T_INTEGER ) {
        result_t rs = parse_line(parser);
        if( failed(&rs) ) {
            for_each_item(program, (action_t)destroy_statement);
            destroy_vector(program);
            return result_with_error(0x0120);
        }
        add_back(program, rs.item);
    }

    return result_with_ptr(program);
}


/* Ինտերպրետատոր */

typedef struct _interpreter {
    vector_t *program;
    unsigned int pc;
    double environment[26];
    unsigned int stack[16+1];
} interpreter_t;

interpreter_t *create_interpreter(vector_t *program)
{
    interpreter_t *vi = malloc(sizeof(interpreter_t));
    if( NULL == vi ) return NULL;
    vi->program = program;
    vi->pc = 0;
    memset(vi->environment, 0, 26 * sizeof(double));
    memset(vi->stack, 0, (16+1) * sizeof(unsigned int)); // review this
    return vi;
}

void destroy_interpreter(interpreter_t *vi)
{
    free(vi);
}

typedef double value_t;
value_t evaluate_expression(interpreter_t *vi, expression_t *e);

value_t evaluate_unary(interpreter_t *vi, unary_t *e)
{
    value_t value = evaluate_expression(vi, e->subexpr);
    if( e->operation == SUB )
        value = -value;
    return value;
}

value_t evaluate_binary(interpreter_t *vi, binary_t *e)
{
    value_t vleft = evaluate_expression(vi, e->left);
    value_t vright = evaluate_expression(vi, e->right);

    value_t value = 0.0;
    switch( e->operation ) {
        case ADD:
            value = vleft + vright;
            break;
        case SUB:
            value = vleft - vright;
            break;
        case MUL:
            value = vleft * vright;
            break;
        case DIV:
            value = vleft / vright;
            break;
        case EQ:
            value = fabs(vleft - vright) < DBL_EPSILON;
            break;
        case NE:
            value = fabs(vleft - vright) >= DBL_EPSILON;
            break;
        case GT:
            value = vleft > vright;
            break;
        case GE:
            value = vleft >= vright;
            break;
        case LT:
            value = vleft < vright;
            break;
        case LE: 
            value = vleft <= vright;
            break;
    }

    return value;
}

size_t index_of(char name)
{
    return toupper(name) - 'A';
}

value_t evaluate_expression(interpreter_t *vi, expression_t *e)
{
    value_t value = 0.0;

    switch( e->kind ) {
        case E_NUMBER:
            value = e->value.number;
            break;
        case E_VARIABLE:
            value = vi->environment[index_of(e->value.name)];
            break;
        case E_UNARY:
            value = evaluate_unary(vi, e->value.unary);
            break;
        case E_BINARY:
            value = evaluate_binary(vi, e->value.binary);
            break;
    }

    return value;
}

void execute_statement(interpreter_t *vi, const statement_t *s);

void execute_input(interpreter_t *vi, const input_t *s)
{
    printf("? ");
    for( int i = 0; i < s->variables->count; ++i ) {
        double value = 0.0;
        scanf("%lf", &value);
        const expression_t *ex = (expression_t *)(s->variables->items[i]);
        char name = ex->value.name;
        vi->environment[index_of(name)] = value;
    }
}

void execute_print(interpreter_t *vi, const print_t *s)
{
    for( int i = 0; i < s->expressions->count; ++i ) {
        value_t value = evaluate_expression(vi, s->expressions->items[i]);
        printf("%lf\t", value);
    }
    putchar('\n');
}

void execute_let(interpreter_t *vi, const let_t *s)
{
    value_t val = evaluate_expression(vi, s->right);
    vi->environment[index_of(s->variable)] = val;
}

void execute_if(interpreter_t *vi, const if_t *s)
{
    value_t cond_val = evaluate_expression(vi, s->condition);
    if( cond_val != 0 )
        execute_statement(vi, s->decision);
}

void execute_goto(interpreter_t *vi, const goto_t *s)
{
    value_t val = evaluate_expression(vi, s->target);
    unsigned int line = (unsigned int)val;
    for(int i = 0; i < vi->program->count; ++i) {
        statement_t *s = (statement_t *)vi->program->items[i];
        if( s->line == line ) {
            vi->pc = i;
            break;
        }
    }
}

void execute_gosub(interpreter_t *vi, const gosub_t *s)
{
    vi->stack[vi->stack[16]] = vi->pc + 1;
    ++vi->stack[16];

    value_t val = evaluate_expression(vi, s->target);
    unsigned int line = (unsigned int)val;
    for(int i = 0; i < vi->program->count; ++i) {
        statement_t *s = (statement_t *)vi->program->items[i];
        if( s->line == line ) {
            vi->pc = i;
            break;
        }
    }
}

void execute_return(interpreter_t *vi)
{
    --vi->stack[16];
    vi->pc = vi->stack[vi->stack[16]];
}

void execute_statement(interpreter_t *vi, const statement_t *s)
{
    switch( s->kind ) {
        case S_END:
            break;
        case S_INPUT:
            execute_input(vi, s->body.input);
            break;
        case S_PRINT:
            execute_print(vi, s->body.print);
            break;
        case S_LET:
            execute_let(vi, s->body.let);
            break;
        case S_IF:
            execute_if(vi, s->body.ifc);
            break;
        case S_GOTO:
            execute_goto(vi, s->body.gotoc);
            break;
        case S_GOSUB:
            execute_gosub(vi, s->body.gosub);
            break;
        case S_RETURN:
            execute_return(vi);
            break;
    }
}

void run(interpreter_t *vi)
{
    while( true ) {
        unsigned int line = vi->pc;
        const statement_t *s = get_elem(vi->program, line);
        if( s->kind == S_END )
            break;
        execute_statement(vi, s);
        if( line == vi->pc )
            vi->pc += 1;
    }
}


void execute_file(const char *file)
{
    scanner_t *scanner = NULL;
    parser_t *parser = NULL;
    vector_t *program = NULL;
    interpreter_t *interpreter = NULL;

    scanner = create_scanner(file);
    if( NULL == scanner ) {
        fprintf(stderr, "ERROR: Cannot open file: '%s'.", file);
        goto cleanup;
    }

    parser = create_parser(scanner);
    if( NULL == parser ) {
        fprintf(stderr, "ERROR: Failed to parse the program.\n");
        goto cleanup;
    }

    const result_t rs = parse(parser);
    if( failed(&rs) ) {
        fprintf(stderr, "ERROR: Failed to parse the program.\n");
        goto cleanup;
    }

    // interprete
    program = (vector_t *)rs.item;
    interpreter = create_interpreter(rs.item);
    if( NULL == interpreter ) {
        goto cleanup;
    }

    run(interpreter);

cleanup:
    destroy_interpreter(interpreter);
    destroy_vector_and_elements(program, (void(*)(void *))destroy_statement);
    destroy_parser(parser);
    destroy_scanner(scanner);
}


int main(int argc, char **argv)
{
    execute_file("/home/armen/Projects/my-tiny-basic/cases/case03.bas");

    if( argc < 2 ) {
        printf("Tiny BASIC, 2024\n");
        return 0;
    }

    //execute_file(argv[1]);
  
    return 0;
}


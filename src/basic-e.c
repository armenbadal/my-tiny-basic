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

void destroy_vector_and_elements(vector_t *vec, action_t destroyer)
{
    if( NULL != vec ) {
        for_each_item(vec, destroyer);
        destroy_vector(vec);
    }
}


/* Տվյալների կառուցվածքներ */

typedef enum _expression_kind {
    E_NUMBER,
    E_STRING,
    E_VARIABLE,
    E_UNARY,
    E_BINARY,
    E_BUILTIN
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

typedef struct _builtin {
    char name[32];
    vector_t *arguments;
} builtin_t;

struct _expression {
    expression_kind_t kind;
    union {
        double number;
        char name;
        char *string;
        unary_t *unary;
        binary_t *binary;
        builtin_t *call;
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

expression_t *create_string(char *str)
{
    expression_t *ex = _create_expression(E_STRING);
    ex->value.string = str;
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

expression_t *create_builtin_call(const char *name, vector_t *args)
{
    expression_t *ex = _create_expression(E_BUILTIN);
    ex->value.call = malloc(sizeof(builtin_t));
    if( NULL == ex->value.call ) {
        destroy_expression(ex);
        return NULL;
    }
    strcpy(ex->value.call->name, name);
    ex->value.call->arguments = args;
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

void destroy_builtin(builtin_t *bi)
{
    for_each_item(bi->arguments, (action_t)&destroy_expression);
    free(bi);
}

void destroy_expression(expression_t *expr)
{
    if( E_STRING == expr->kind )
        free(expr->value.string);
    else if( E_UNARY == expr->kind )
        destroy_unary(expr->value.unary);
    else if( E_BINARY == expr->kind )
        destroy_binary(expr->value.binary);
    else if( E_BUILTIN == expr->kind )
        destroy_builtin(expr->value.call);
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

typedef vector_t program_t;

/* Շարայուսական վերլուծություն */

/* Լեքսեմը */
typedef enum _token {
    T_NONE = 0,
    T_INTEGER,
    T_REAL,
    T_STRING,
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
    T_EOF,
    T_REM,
    T_BUILTIN
} token_t;

typedef struct _lexeme {
    token_t token;
    union {
        char name[32];
        double number;
        char *string;
    } value;
} lexeme_t;


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

void skip_until(scanner_t* scanner, char c)
{
    while( c != scanner->ch )
        advance(scanner);
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

    return (lexeme_t){ .token = T_REAL, .value.number = number / k };
}

typedef struct _pair_of_text_and_token {
    const char *text;
    const token_t token;
} pair_of_text_and_token_t;

const pair_of_text_and_token_t keywords[] = {
    {"END", T_END},
    {"INPUT", T_INPUT},
    {"PRINT", T_PRINT},
    {"LET", T_LET},
    {"IF", T_IF},
    {"THEN", T_THEN},
    {"GOTO", T_GOTO},
    {"GOSUB", T_GOSUB},
    {"RETURN", T_RETURN},
    {"REM", T_REM},

    {NULL, T_NONE}
};

const pair_of_text_and_token_t builtin_functions[] = {
    {"SQR", T_BUILTIN},

    {NULL, T_NONE}
};

lexeme_t scan_identifier_or_name(scanner_t *scanner)
{
    lexeme_t lex;
    memset(&lex, 0, sizeof(lexeme_t));

    char *p = lex.value.name;
    do {
        *p = scanner->ch;
        ++p;
        advance(scanner);
    } while( isalpha(scanner->ch) );
    *p = '\0';

    if( strlen(lex.value.name) == 1 ) {
        lex.token = T_NAME;
        return lex;
    }

    for( const pair_of_text_and_token_t *kw = keywords; kw->text != NULL; ++kw )
        if( 0 == strcmp(kw->text, lex.value.name) ) {
            lex.token = kw->token;
            return lex;
        }

    for( const pair_of_text_and_token_t *kw = builtin_functions; kw->text != NULL; ++kw )
        if( 0 == strcmp(kw->text, lex.value.name) ) {
            lex.token = T_BUILTIN;
            return lex;
        }

    return (lexeme_t){ .token = T_NONE };
}

lexeme_t scan_string(scanner_t* scanner)
{
    advance(scanner);

    char buffer[128] = { 0 };
    char *p = buffer;
    while( '"' != scanner->ch ) {
        *p = scanner->ch;
        ++p;
        advance(scanner);
    }
    advance(scanner);

    lexeme_t str= {
        .token = T_STRING,
        .value.string = malloc(1 + strlen(buffer))
    };
    strcpy(str.value.string, buffer);
    return str;
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

    if( '"' == scanner->ch )
        return scan_string(scanner);

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



typedef enum _error_code {
    R_OK = 0,
    R_MANDATORY_LINE_NUMBER,
    R_MANDATORY_END_OF_LINE,
    R_EXPECTED_THEN,
    R_EXPECTED_NAME,
    R_EXPECTED_EQ,
    R_EXPECTED_RELOP,
    R_EXPECTED_COMMAND,
    R_EXPECTET_LPAR,
    R_EXPECTET_RPAR
} error_code_t;

const char *const error_message[] = {
    "Ok",
    "Տողը պետք է սկսվի թվով",
    "Հրամանը պետք է ավարտվի նոր տողի նիշով",
    "Սպասվում է THEN",
    "Սպասվում է իդենտիֆիկատոր",
    "Սպասվում է «=»",
    "Սպասվում է համեմատության գործողություն",
    "Հրամանը պետք է սկսվի ծառայողական բառով",
    "Սպասվում է «(»",
    "Սպասվում է «)»",
    NULL
};

typedef struct _parser {
    scanner_t *scanner;
    lexeme_t lookahead;
    size_t line;
} parser_t;

parser_t *create_parser(scanner_t *scanner)
{
    parser_t *parser = malloc(sizeof(parser_t));
    if( NULL == parser ) return NULL;
    parser->scanner = scanner;
    parser->lookahead = (lexeme_t){ .token = T_NONE };
    parser->line = 0;
    return parser;
}

void destroy_parser(parser_t *parser)
{
    free(parser);
}

bool has(const parser_t *parser, token_t expected)
{
    return expected == parser->lookahead.token;
}

bool has_any_of(const parser_t *parser, size_t n, ...)
{
    va_list tokens;
    va_start(tokens, n);

    bool found = false;
    for( size_t i = 0; i < n; ++i ) {
        if( has(parser, va_arg(tokens, token_t)) ) {
            found = true;
            break;
        }
    }

    va_end(tokens);

    return found;
}

bool match(parser_t *parser, token_t expected)
{
    if( has(parser, expected) ) {
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

typedef struct _result {
    void *item;
    error_code_t ec; 
} result_t;

bool failed(const result_t *r)
{
    return R_OK != r->ec;
}

result_t result_with_ptr(void *p)
{
    return (result_t){ .item = p, .ec = R_OK };
}

result_t result_with_error(error_code_t ec)
{
    return (result_t){ .item = NULL, .ec = ec };
}

result_t parse_expression(parser_t *parser);

result_t parse_name(parser_t *parser)
{
    char name = parser->lookahead.value.name[0];
    match(parser, T_NAME);
    expression_t *vr = create_variable(name);
    return (result_t){ .item = vr, .ec = 0 };
}

result_t parse_number(parser_t *parser)
{
    double num = parser->lookahead.value.number;
    match_any(parser, 2, T_INTEGER, T_REAL);
    expression_t *nm = create_number(num);
    return (result_t){ .item = nm, .ec = 0 };
}

result_t parse_string(parser_t *parser)
{
    char *str = parser->lookahead.value.string;
    match(parser, T_STRING);
    expression_t *es = create_string(str);
    return (result_t){ .item = es, .ec = 0 };
}

result_t parse_expression_list(parser_t *parser)
{
    vector_t *exprs = create_vector(4);

    result_t re = parse_expression(parser);
    if( failed(&re) ) {
        destroy_vector(exprs);
        return re;
    }
    add_back(exprs, re.item);

    while( has(parser, T_COMMA) ) {
        match(parser, T_COMMA);
        re = parse_expression(parser);
        if( failed(&re) ) {
            destroy_vector_and_elements(exprs, (action_t)&destroy_expression);
            return re;
        }
        add_back(exprs, re.item);
    }

    return result_with_ptr(exprs);
}

result_t parse_builtin_call(parser_t *parser)
{
    char name[32] = { 0 };
    strcpy(name, parser->lookahead.value.name);
    match(parser, T_BUILTIN);

    if( !match(parser, T_LPAR) )
        return result_with_error(R_EXPECTET_LPAR);

    result_t rel = parse_expression_list(parser);
    if( failed(&rel) )
        return rel;

    vector_t *args = rel.item;

    if( !match(parser, T_RPAR) ) {
        destroy_vector_and_elements(args, (action_t)&destroy_expression);
        return result_with_error(R_EXPECTET_RPAR);
    }

    expression_t *bic = create_builtin_call(name, args);
    return result_with_ptr(bic);
}

result_t parse_parenthesed(parser_t *parser)
{
    match(parser, T_LPAR);
    result_t rs = parse_expression(parser);
    if( rs.ec != R_OK )
        return rs;
    if( !match(parser, T_RPAR) ) {
        destroy_expression(rs.item);
        return (result_t){ .item = NULL, .ec = 0x0101 };
    }
    return rs;
}

result_t parse_factor(parser_t *parser)
{
    if( has(parser, T_NAME) )
        return parse_name(parser);

    if( has_any_of(parser, 2, T_INTEGER, T_REAL) ) 
        return parse_number(parser);

    if( has(parser, T_STRING) )
        return parse_string(parser);

    if( has(parser, T_BUILTIN) )
        return parse_builtin_call(parser);

    if( has(parser, T_LPAR) )
        return parse_parenthesed(parser);

    return (result_t){ .item = NULL, .ec = 0 };
}

result_t parse_term(parser_t *parser)
{
    result_t rs = parse_factor(parser);
    if( rs.ec != R_OK )
        return rs;

    while( has_any_of(parser, 2, T_MUL, T_DIV) ) {
        operation_t oper = T_DIV == parser->lookahead.token ? DIV : MUL;
        match_any(parser, 2, T_MUL, T_DIV);
        result_t r2 = parse_factor(parser);
        if( r2.ec != R_OK )
            return r2;
        rs.item = create_binary(oper, rs.item, r2.item);
    }

    return rs;
}

result_t parse_expression(parser_t *parser)
{
    operation_t unop = ADD;
    if( has(parser, T_ADD) )
        match(parser, T_ADD);
    else if( has(parser, T_SUB) ) {
        unop = SUB;
        match(parser, T_SUB);
    }

    result_t rs = parse_term(parser);
    if( failed(&rs) )
        return rs;
    
    if( SUB == unop )
        rs.item = create_unary(unop, rs.item);

    while( has_any_of(parser, 2, T_ADD, T_SUB) ) {
        operation_t oper = T_ADD == parser->lookahead.token ? ADD : SUB;
        match_any(parser, 2, T_ADD, T_SUB);
        result_t r2 = parse_term(parser);
        if( failed(&r2) ) {
            destroy_expression(rs.item);
            return r2;
        }
        rs.item = create_binary(oper, rs.item, r2.item);
    }

    return rs;
}

result_t parse_comparison(parser_t *parser)
{
    result_t rl = parse_expression(parser);
    if( failed(&rl) )
        return rl;

    token_t token = parser->lookahead.token;
    if( !match_any(parser, 6, T_EQ, T_NE, T_GT, T_GE, T_LT, T_LE) ) {
        destroy_expression(rl.item);
        return result_with_error(R_EXPECTED_RELOP);
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
        return rr;
    }

    expression_t *comp = create_binary(oper, rl.item, rr.item);
    return result_with_ptr(comp);
}

result_t parse_statement(parser_t *parser);

result_t parse_end(parser_t *parser)
{
    match(parser, T_END);    
    return result_with_ptr(create_end());
}

result_t parse_input(parser_t *parser)
{
    match(parser, T_INPUT);

    vector_t *variables = create_vector(4);

    char name = parser->lookahead.value.name[0];
    if( !match(parser, T_NAME) )
        return result_with_error(R_EXPECTED_NAME);
    add_back(variables, create_variable(name));

    while( has(parser, T_COMMA) ) {
        match(parser, T_COMMA);
        name = parser->lookahead.value.name[0];
        if( !match(parser, T_NAME) ) {
            for_each_item(variables, (action_t)destroy_expression);
            return result_with_error(R_EXPECTED_NAME);
        }
        add_back(variables, create_variable(name));
    }

    return result_with_ptr(create_input(variables));
}

result_t parse_print(parser_t *parser)
{
    match(parser, T_PRINT);

    result_t rel = parse_expression_list(parser);
    if( failed(&rel) )
        return rel;

    return result_with_ptr(create_print(rel.item));
}

result_t parse_let(parser_t *parser)
{
    match(parser, T_LET);

    char name = parser->lookahead.value.name[0];
    if( !match(parser, T_NAME) )
        return result_with_error(R_EXPECTED_NAME);

    if( !match(parser, T_EQ) )
        return result_with_error(R_EXPECTED_EQ);

    result_t rs = parse_expression(parser);
    if( failed(&rs) )
        return rs;

    return result_with_ptr(create_let(name, rs.item));
}

result_t parse_if(parser_t *parser)
{
    match(parser, T_IF);

    result_t rc = parse_comparison(parser);
    if( failed(&rc) )
        return rc;

    if( !match(parser, T_THEN) ) {
        destroy_expression(rc.item);
        return result_with_error(R_EXPECTED_THEN);
    }

    result_t ds = parse_statement(parser);
    if( failed(&ds) ) {
        destroy_expression(rc.item);
        return ds;
    }

    statement_t *st = create_if(rc.item, ds.item, NULL);
    return result_with_ptr(st);
}

result_t parse_goto(parser_t* parser)
{
    match(parser, T_GOTO);

    result_t rs = parse_expression(parser);
    if( failed(&rs) )
        return rs;

    return result_with_ptr(create_goto(rs.item));
}

result_t parse_gosub(parser_t* parser)
{
    match(parser, T_GOSUB);

    result_t rs = parse_expression(parser);
    if( failed(&rs) )
        return rs;

    return result_with_ptr(create_gosub(rs.item));
}

result_t parse_return(parser_t* parser)
{
    match(parser, T_RETURN);
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

    return result_with_error(R_EXPECTED_COMMAND);
}

result_t parse_line(parser_t *parser)
{
    parser->line = (unsigned int)parser->lookahead.value.number;
    if( !match(parser, T_INTEGER) )
        return result_with_error(R_MANDATORY_LINE_NUMBER);
    
    if( T_REM == parser->lookahead.token ) {
        skip_until(parser->scanner, '\n');
        advance(parser->scanner);
        parser->lookahead.token = T_EOL;
        match(parser, T_EOL);

        parser->line = (unsigned int)parser->lookahead.value.number;
        if( !match(parser, T_INTEGER) )
            return result_with_error(R_MANDATORY_LINE_NUMBER);
    }

    result_t rs = parse_statement(parser);
    if( failed(&rs) )
        return rs;

    statement_t *st = (statement_t *)rs.item;
    st->line = parser->line;

    if( !match(parser, T_EOL) )
        return result_with_error(R_MANDATORY_END_OF_LINE);

    return result_with_ptr(st);
}

result_t parse(parser_t *parser)
{
    program_t *program = create_vector(16);

    match(parser, T_NONE);
    while( has(parser, T_INTEGER) ) {
        result_t rs = parse_line(parser);
        if( failed(&rs) ) {
            destroy_vector_and_elements(program, (action_t)&destroy_statement);
            return rs;
        }
        add_back(program, rs.item);
    }

    return result_with_ptr(program);
}


/* Ինտերպրետատոր */

typedef enum _value_kind {
    V_NUMBER,
    V_TEXT
} value_kind_t;

typedef struct _value {
    value_kind_t kind;
    union {
        double real;
        char *text;
    } v;
} value_t;

typedef struct _interpreter {
    program_t *program;
    unsigned int pc;
    value_t environment[26];
    unsigned int stack[16+1];
} interpreter_t;

interpreter_t *create_interpreter(program_t *program)
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

value_t evaluate_expression(interpreter_t *vi, expression_t *e);

value_t evaluate_unary(interpreter_t *vi, unary_t *e)
{
    value_t value = evaluate_expression(vi, e->subexpr);
    if( value.kind == V_NUMBER && e->operation == SUB )
        value.v.real *= -1;
    return value;
}

value_t evaluate_binary(interpreter_t *vi, binary_t *e)
{
    value_t vleft = evaluate_expression(vi, e->left);
    value_t vright = evaluate_expression(vi, e->right);

    value_t value = { .kind = V_NUMBER, .v.real = 0.0 };
    switch( e->operation ) {
        case ADD:
            value.v.real = vleft.v.real + vright.v.real;
            break;
        case SUB:
            value.v.real = vleft.v.real - vright.v.real;
            break;
        case MUL:
            value.v.real = vleft.v.real * vright.v.real;
            break;
        case DIV:
            value.v.real = vleft.v.real / vright.v.real;
            break;
        case EQ:
            value.v.real = fabs(vleft.v.real - vright.v.real) < DBL_EPSILON;
            break;
        case NE:
            value.v.real = fabs(vleft.v.real - vright.v.real) >= DBL_EPSILON;
            break;
        case GT:
            value.v.real = vleft.v.real > vright.v.real;
            break;
        case GE:
            value.v.real = vleft.v.real >= vright.v.real;
            break;
        case LT:
            value.v.real = vleft.v.real < vright.v.real;
            break;
        case LE: 
            value.v.real = vleft.v.real <= vright.v.real;
            break;
    }

    return value;
}

value_t evaluate_builtin_call(interpreter_t *vi, builtin_t *bi)
{
    if( 0 == strcmp(bi->name, "SQR") ) {
        value_t val = evaluate_expression(vi, get_elem(bi->arguments, 0));
        val.v.real = sqrt(val.v.real);
        return val;
    }

    return (value_t){ .kind = V_NUMBER, .v.real = 0.0 };
}

size_t index_of(char name)
{
    return toupper(name) - 'A';
}

value_t evaluate_expression(interpreter_t *vi, expression_t *e)
{
    value_t value = { .kind = V_NUMBER, .v.real = 0.0 };

    switch( e->kind ) {
        case E_NUMBER:
            value.v.real = e->value.number;
            break;
        case E_STRING:
            value.kind = V_TEXT;
            value.v.text = e->value.string;
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
        case E_BUILTIN:
            value = evaluate_builtin_call(vi, e->value.call);
            break;
    }

    return value;
}

void execute_statement(interpreter_t *vi, const statement_t *s);

void execute_input(interpreter_t *vi, const input_t *s)
{
    printf("? ");
    for( int i = 0; i < s->variables->count; ++i ) {
        value_t value = { .kind = V_NUMBER, .v.real = 0.0 };
        scanf("%lf", &value.v.real);
        const expression_t *ex = (expression_t *)(s->variables->items[i]);
        char name = ex->value.name;
        vi->environment[index_of(name)] = value;
    }
}

void execute_print(interpreter_t *vi, const print_t *s)
{
    for( int i = 0; i < s->expressions->count; ++i ) {
        value_t value = evaluate_expression(vi, s->expressions->items[i]);
        if( value.kind == V_NUMBER )
            printf("%lf\t", value.v.real);
        else if( value.kind == V_TEXT )
            printf("%s\t", value.v.text);
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
    if( cond_val.v.real != 0.0 )
        execute_statement(vi, s->decision);
}

void execute_goto(interpreter_t *vi, const goto_t *s)
{
    value_t val = evaluate_expression(vi, s->target);
    unsigned int line = (unsigned int)val.v.real;
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
    unsigned int line = (unsigned int)val.v.real;
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
    program_t *program = NULL;
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
        fprintf(stderr, "ERROR: %s:\n", error_message[rs.ec]);
        goto cleanup;
    }

    // interprete
    program = (program_t *)rs.item;
    interpreter = create_interpreter(program);
    if( NULL == interpreter ) {
        goto cleanup;
    }

    run(interpreter);

cleanup:
    destroy_interpreter(interpreter);
    destroy_vector_and_elements(program, (action_t)&destroy_statement);
    destroy_parser(parser);
    destroy_scanner(scanner);
}


int main(int argc, char **argv)
{
    execute_file("/home/armen/Projects/my-tiny-basic/cases/case05.bas");

    if( argc < 2 ) {
        printf("Tiny BASIC, 2024\n");
        return 0;
    }

    execute_file(argv[1]);
  
    return 0;
}


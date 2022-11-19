#include <stdlib.h>
#include <err.h>
#include <string.h>
#include <stdio.h>

#include "parser.h"

struct expression *parse_expression(struct parser *p, enum precedence precedence);
int parse_statement(struct parser *p, struct statement *s);
void expression_to_str(char *str, struct expression *expr);
void free_expression(struct expression *expr);
enum operator parse_operator(enum token_type t);

enum precedence get_token_precedence(struct token t) {
    switch (t.type) {
        case TOKEN_EQ: return EQUALS;
        case TOKEN_NOT_EQ: return EQUALS;
        case TOKEN_LT: return LESSGREATER;
        case TOKEN_GT: return LESSGREATER;
        case TOKEN_PLUS: return SUM;
        case TOKEN_MINUS: return SUM;
        case TOKEN_SLASH: return PRODUCT;
        case TOKEN_ASTERISK: return PRODUCT;
        case TOKEN_LPAREN: return CALL;
        case TOKEN_LBRACKET: return INDEX;
        default: return LOWEST;
    }

    return LOWEST;
}

void next_token(struct parser * p) {
    p->current_token = p->next_token;
    gettoken(p->lexer, &p->next_token);
}

struct parser new_parser(struct lexer *l) {
    struct parser p = {
        .lexer = l,
        .errors = 0,
    };
   
    // read two tokens so that both current_token and next_token are set
    gettoken(p.lexer, &p.current_token);
    gettoken(p.lexer, &p.next_token);
    return p;
}

int current_token_is(struct parser *p, enum token_type t) {
    return t == p->current_token.type;
}

int next_token_is(struct parser *p, enum token_type t) {
    return t == p->next_token.type;
}

int expect_next_token(struct parser *p, enum token_type t) {
    if (next_token_is(p, t)) {
        next_token(p);
        return 1;
    }

    sprintf(p->error_messages[p->errors++], "expected next token to be %s, got %s instead", token_type_to_str(t), token_type_to_str(p->next_token.type));
    return 0;
}

int parse_let_statement(struct parser *p, struct statement *s) {
    s->type = STMT_LET;
    s->token = p->current_token;

    if (!expect_next_token(p, TOKEN_IDENT)) {
        return -1;
    }

    // parse identifier
    struct identifier id = {
        .token = p->current_token,
    };
    strncpy(id.value, p->current_token.literal, MAX_IDENT_LENGTH);
    s->name = id;

    if (!expect_next_token(p, TOKEN_ASSIGN)) {
        return -1;
    }

    // parse expression
    next_token(p);
    s->value = parse_expression(p, LOWEST);
    if (next_token_is(p, TOKEN_SEMICOLON)) {
        next_token(p);
    }

    return 1;
}

int parse_return_statement(struct parser *p, struct statement *s) {
    s->type = STMT_RETURN;
    s->token = p->current_token;

    // parse expression
    next_token(p);
    s->value = parse_expression(p, LOWEST);

    if (next_token_is(p, TOKEN_SEMICOLON)) {
        next_token(p);
    }

    return 1;
}

struct expression *parse_identifier_expression(struct parser *p) {
    struct expression *expr = malloc(sizeof *expr);
    if (!expr) {
        err(EXIT_FAILURE, "out of memory");
    }

    expr->type = EXPR_IDENT;
    expr->token = expr->value.ident.token = p->current_token;
    strncpy(expr->value.ident.value, p->current_token.literal, MAX_IDENT_LENGTH);
    return expr;
}

struct expression *parse_string_literal(struct parser *p) {
    struct expression *expr = malloc(sizeof *expr);
    if (!expr) {
        err(EXIT_FAILURE, "out of memory");
    }

    int len = strlen(p->current_token.literal) + 1;
    expr->value.string = malloc(len);
    if (!expr->value.string) {
        err(EXIT_FAILURE, "out of memory");
    }
    expr->type = EXPR_STRING;
    expr->token = p->current_token;
    strncpy(expr->value.string, p->current_token.literal, len);
    return expr;
}

struct expression *parse_int_expression(struct parser *p) {
    struct expression *expr = malloc(sizeof *expr);
    if (!expr) {
        err(EXIT_FAILURE, "out of memory");
    }

    expr->type = EXPR_INT;
    expr->token = p->current_token;
    expr->value.integer = atoi(p->current_token.literal);
    return expr;
}

struct expression *parse_prefix_expression(struct parser *p) {
    struct expression *expr = malloc(sizeof *expr);
    if (!expr) {
        err(EXIT_FAILURE, "out of memory");
    }

    expr->type = EXPR_PREFIX;
    expr->token = p->current_token;
    expr->value.prefix.operator = parse_operator(p->current_token.type);
    next_token(p);
    expr->value.prefix.right = parse_expression(p, PREFIX);
    return expr;
}

struct expression_list parse_expression_list(struct parser *p, enum token_type end_token) {
    struct expression_list list = {
        .size = 0,
        .cap = 0,
    };

    if (next_token_is(p, end_token)) {
        next_token(p);
        return list;
    }

    // allocate memory here, so we do not need an alloc for calls without any arguments
    list.cap = 4;
    list.values = malloc(list.cap * sizeof *list.values);
    if (!list.values) {
        err(EXIT_FAILURE, "out of memory");
    }
    next_token(p);
    list.values[list.size++] = parse_expression(p, LOWEST);

    while (next_token_is(p, TOKEN_COMMA)) {
        next_token(p);
        next_token(p);

        list.values[list.size++] = parse_expression(p, LOWEST);

        // double capacity if needed
        if (list.size >= list.cap) {
            list.cap *= 2;
            list.values = realloc(list.values, list.cap * sizeof *list.values);
        }
    }

    if (!expect_next_token(p, end_token)) {
        free(list.values);
        return list;
    }

    return list;
}

struct expression *parse_array_literal(struct parser *p) {
    struct expression *expr = malloc(sizeof *expr);
    if (!expr) {
        err(EXIT_FAILURE, "out of memory");
    }
    expr->type = EXPR_ARRAY;
    expr->token = p->current_token;
    expr->value.array = parse_expression_list(p, TOKEN_RBRACKET);
    return expr;
}


struct expression *parse_index_expression(struct parser *p, struct expression *left) {
    struct expression *expr = malloc(sizeof *expr);
    if (!expr) {
        err(EXIT_FAILURE, "out of memory");
    }
    expr->type = EXPR_INDEX;
    expr->token = p->current_token;
    expr->value.index.left = left;

    next_token(p);
    expr->value.index.index = parse_expression(p, LOWEST);
    if (!expect_next_token(p, TOKEN_RBRACKET)) {
        free(expr);
        return NULL;
    }
    return expr;
}

struct expression *parse_call_expression(struct parser *p, struct expression *left) {
    struct expression *expr = malloc(sizeof *expr);
    if (!expr) {
        err(EXIT_FAILURE, "out of memory");
    }
    expr->type = EXPR_CALL;
    expr->token = p->current_token;
    expr->value.call.function = left;
    expr->value.call.arguments = parse_expression_list(p, TOKEN_RPAREN);
    return expr;
}

struct expression *parse_infix_expression(struct parser *p, struct expression *left) {
    struct expression * expr = malloc(sizeof *expr);
    if (!expr) {
        err(EXIT_FAILURE, "out of memory");
    }

    expr->type = EXPR_INFIX;
    expr->token = p->current_token;
    expr->value.infix.left = left;
    expr->value.infix.operator = parse_operator(p->current_token.type);
    int precedence = get_token_precedence(p->current_token);
    next_token(p);
    expr->value.infix.right = parse_expression(p, precedence);
    return expr;
}

struct expression *parse_boolean_expression(struct parser *p) {
    struct expression *expr = malloc(sizeof *expr);
    if (!expr) {
        err(EXIT_FAILURE, "out of memory");
    }

    expr->type = EXPR_BOOL;
    expr->token = p->current_token;
    expr->value.boolean = current_token_is(p, TOKEN_TRUE);
    return expr;
}

struct expression *parse_grouped_expression(struct parser *p) {
    next_token(p);
    
    struct expression *expr = parse_expression(p, LOWEST);

    if (!expect_next_token(p, TOKEN_RPAREN)) {
        free(expr);
        return NULL;
    }

    return expr;
}

struct block_statement *parse_block_statement(struct parser *p) {
    struct block_statement *b = malloc(sizeof *b);
    if (!b) {
        err(EXIT_FAILURE, "out of memory");
    }

    b->cap = 16;
    b->size = 0;
    b->statements = malloc(b->cap * sizeof (struct statement));
    if (!b->statements) {
        err(EXIT_FAILURE, "out of memory");
    }
    next_token(p);

    while (!current_token_is(p, TOKEN_RBRACE) && !current_token_is(p, TOKEN_EOF)) {
        struct statement s;
        if (parse_statement(p, &s) > -1) {
            b->statements[b->size++] = s;

            if (b->size >= b->cap) {
                b->cap *= 2;
                b->statements = realloc(b->statements, b->cap * sizeof *b->statements);
            }
        }
        next_token(p);
    }

    return b;
}

struct expression *make_expression(enum expression_type type, struct token tok) {
    struct expression *expr = malloc(sizeof *expr);
    if (!expr) {
        err(EXIT_FAILURE, "out of memory");
    }

    expr->type = type;
    expr->token = tok;
    return expr;
}

struct expression *parse_while_expression(struct parser *p) {
    struct expression *expr = make_expression(EXPR_WHILE, p->current_token);
    if (!expect_next_token(p, TOKEN_LPAREN)) {
        free(expr);
        return NULL;
    }

    next_token(p);
    expr->value.whilst.condition = parse_expression(p, LOWEST);
     if (!expect_next_token(p, TOKEN_RPAREN)) {
        free(expr->value.whilst.condition);
        free(expr);
        return NULL;
    }

    if (!expect_next_token(p, TOKEN_LBRACE)) {
        free(expr->value.whilst.condition);
        free(expr);
        return NULL;
    }

    expr->value.whilst.body = parse_block_statement(p);
    return expr;
}

struct expression *parse_if_expression(struct parser *p) {
    struct expression *expr = malloc(sizeof *expr);
    if (!expr) {
        err(EXIT_FAILURE, "out of memory");
    }

    expr->type = EXPR_IF;
    expr->token = p->current_token;

    if (!expect_next_token(p, TOKEN_LPAREN)) {
        free(expr);
        return NULL;
    }

    next_token(p);
    expr->value.ifelse.condition = parse_expression(p, LOWEST);

    if (!expect_next_token(p, TOKEN_RPAREN)) {
        free(expr->value.ifelse.condition);
        free(expr);
        return NULL;
    }

    if (!expect_next_token(p, TOKEN_LBRACE)) {
        free(expr->value.ifelse.condition);
        free(expr);
        return NULL;
    }

    expr->value.ifelse.consequence = parse_block_statement(p);

    if (next_token_is(p, TOKEN_ELSE)) {
        next_token(p);

        if (!expect_next_token(p, TOKEN_LBRACE)) {
            free(expr->value.ifelse.consequence);
            free(expr->value.ifelse.condition);
            free(expr);
            return NULL;
        }

        expr->value.ifelse.alternative = parse_block_statement(p);
    } else {
        expr->value.ifelse.alternative = NULL;
    }

    return expr;
}

struct identifier_list parse_function_parameters(struct parser *p) {
    struct identifier_list params = {
        .size = 0,
        .cap = 4,
    };
    params.values = malloc(sizeof *params.values * params.cap);
    if (!params.values) {
        err(EXIT_FAILURE, "out of memory");
    }

    if (next_token_is(p, TOKEN_RPAREN)) {
        next_token(p);
        return params;
    }

    next_token(p);
    struct identifier i;
    i.token = p->current_token;
    strncpy(i.value, i.token.literal, MAX_IDENT_LENGTH);
    params.values[params.size++] = i;

    while (next_token_is(p, TOKEN_COMMA)) {
        next_token(p);
        next_token(p);

        struct identifier i;
        i.token = p->current_token;
        strncpy(i.value, i.token.literal, MAX_IDENT_LENGTH);
        params.values[params.size++] = i;

        if (params.size >= params.cap) {
            params.cap *= 2;
            params.values = realloc(params.values, params.cap * sizeof *params.values);
        }
    }

    if (!expect_next_token(p, TOKEN_RPAREN)) {
        free(params.values);
        return params;
    }

    return params;
}

struct expression *parse_function_literal(struct parser *p) {
    struct expression *expr = malloc(sizeof *expr);
    if (!expr) {
        err(EXIT_FAILURE, "out of memory");
    }

    expr->type = EXPR_FUNCTION;
    expr->token = p->current_token;

    if (!expect_next_token(p, TOKEN_LPAREN)) {
        free(expr);
        return NULL;
    }

    expr->value.function.parameters = parse_function_parameters(p);
    if (!expect_next_token(p, TOKEN_LBRACE)) {
        free(expr);
        return NULL;
    }

    expr->value.function.body = parse_block_statement(p);
    return expr;
}

struct expression *parse_expression(struct parser *p, enum precedence precedence) {
    struct expression *left;
    switch (p->current_token.type) {
        case TOKEN_IDENT: 
            left = parse_identifier_expression(p); 
        break;
        case TOKEN_INT: 
            left = parse_int_expression(p); 
        break;
        case TOKEN_BANG:
        case TOKEN_MINUS: 
            left = parse_prefix_expression(p);
        break;
        case TOKEN_TRUE:
        case TOKEN_FALSE: 
            left = parse_boolean_expression(p); 
        break;
        case TOKEN_LPAREN:
            left = parse_grouped_expression(p);
        break;
        case TOKEN_IF:
            left = parse_if_expression(p);
        break; 
        case TOKEN_WHILE: 
            left = parse_while_expression(p);
        break;
        case TOKEN_FUNCTION:
            left = parse_function_literal(p);
        break;   
        case TOKEN_STRING: 
            left = parse_string_literal(p);
        break;
        case TOKEN_LBRACKET: 
            left = parse_array_literal(p);
        break;
        default: 
            if (p->errors < 8) {
                sprintf(p->error_messages[p->errors++], "no prefix parse function found for %s", token_type_to_str(p->current_token.type));
            }
            return NULL;
        break;
    }

    // maybe parse right (infix) expression
    while (!next_token_is(p, TOKEN_SEMICOLON) && precedence < get_token_precedence(p->next_token)) {
        enum token_type type = p->next_token.type;
        switch (type) {
            case TOKEN_PLUS: 
            case TOKEN_MINUS: 
            case TOKEN_ASTERISK: 
            case TOKEN_SLASH: 
            case TOKEN_EQ: 
            case TOKEN_NOT_EQ: 
            case TOKEN_LT: 
            case TOKEN_GT: 
                next_token(p);
                left = parse_infix_expression(p, left);
            break; 

            case TOKEN_LPAREN: 
                next_token(p);
                left = parse_call_expression(p, left);
            break; 

            case TOKEN_LBRACKET: 
                next_token(p);
                left = parse_index_expression(p, left);
            break;

            default: 
                return left;
            break;
        }
    }

    return left;
}

int parse_expression_statement(struct parser *p, struct statement *s) {
    s->type = STMT_EXPR;
    s->token = p->current_token;
    s->value = parse_expression(p, LOWEST);

    if (next_token_is(p, TOKEN_SEMICOLON)) {
        next_token(p);
    } 

    return 1;
}

int parse_statement(struct parser *p, struct statement *s) {
    switch (p->current_token.type) {
        case TOKEN_LET: return parse_let_statement(p, s); break;
        case TOKEN_RETURN: return parse_return_statement(p, s); break;
        default: return parse_expression_statement(p, s); break;
    }
  
   return -1;
}

struct program *parse_program(struct parser *parser) {
    struct program *program = malloc(sizeof *program);
    if (!program) {
        err(EXIT_FAILURE, "out of memory");
    }

    program->size = 0;
    program->cap = 32;
    program->statements = malloc(program->cap * sizeof *program->statements);
    if (!program->statements) {
        err(EXIT_FAILURE, "out of memory");
    }

    struct statement s;
    while (parser->current_token.type != TOKEN_EOF) {
        
        // if an error occured, skip token & continue
        if (parse_statement(parser, &s) == -1) {
            next_token(parser);
            continue;
        }
        
        program->statements[program->size++] = s;

        // double program capacity if needed
        if (program->size >= program->cap) {
            program->cap *= 2;
            program->statements = realloc(program->statements, sizeof *program->statements * program->cap);
        }

        next_token(parser);        
    }

    return program;
}

void let_statement_to_str(char *str, struct statement *stmt) {    
    strcat(str, stmt->token.literal);
    strcat(str, " ");
    strcat(str, stmt->name.value);
    strcat(str, " = ");
    expression_to_str(str, stmt->value);
    strcat(str, ";");
}

void return_statement_to_str(char *str, struct statement *stmt) {
    strcat(str, stmt->token.literal);
    strcat(str, " ");
    expression_to_str(str, stmt->value);
    strcat(str, ";");
}

void statement_to_str(char *str, struct statement *stmt) {
    switch (stmt->type) {
        case STMT_LET: let_statement_to_str(str, stmt); break;
        case STMT_RETURN: return_statement_to_str(str, stmt); break;
        case STMT_EXPR: expression_to_str(str, stmt->value); break;
    }
}

void block_statement_to_str(char *str, struct block_statement *b) {
    for (unsigned int i=0; i < b->size; i++) {
        statement_to_str(str, &b->statements[i]);
    }
}

void identifier_list_to_str(char *str, struct identifier_list *identifiers) {
    for (unsigned int i=0; i < identifiers->size; i++) {
        strcat(str, identifiers->values[i].value);
        if (i < (identifiers->size - 1)) {
            strcat(str, ", ");
        }
    }
}

void expression_to_str(char *str, struct expression *expr) {
    switch (expr->type) {
        case EXPR_PREFIX: 
            strcat(str, "(");
            strcat(str, expr->token.literal);
            expression_to_str(str, expr->value.prefix.right);
            strcat(str, ")");
        break;

        case EXPR_INFIX: 
            strcat(str, "(");
            expression_to_str(str, expr->value.infix.left);
            strcat(str, " ");
            strcat(str, expr->token.literal);
            strcat(str, " ");
            expression_to_str(str, expr->value.infix.right);
            strcat(str, ")");
        break;

        case EXPR_STRING:
            strcat(str, expr->value.string);
        break;

        case EXPR_IDENT:
            strcat(str, expr->value.ident.value);
        break;

        case EXPR_BOOL:
            strcat(str, expr->value.boolean ? "true" : "false");
        break;

        case EXPR_INT:
            strcat(str, expr->token.literal);
        break;

        case EXPR_IF:
            strcat(str, "if ");
            expression_to_str(str, expr->value.ifelse.condition);
            strcat(str, " ");
            block_statement_to_str(str, expr->value.ifelse.consequence);
            if (expr->value.ifelse.alternative) {
                strcat(str, "else ");
                block_statement_to_str(str, expr->value.ifelse.alternative);
            }
        break;

        case EXPR_WHILE: 
            strcat(str, "while ");
            expression_to_str(str, expr->value.whilst.condition);
            strcat(str, " ");
            block_statement_to_str(str, expr->value.whilst.body);
        break;

        case EXPR_FUNCTION:
            strcat(str, expr->token.literal);
            strcat(str, "(");
            identifier_list_to_str(str, &expr->value.function.parameters);
            strcat(str, ") ");
            block_statement_to_str(str, expr->value.function.body);
        break;

        case EXPR_CALL:
            expression_to_str(str, expr->value.call.function);
            strcat(str, "(");
            for (unsigned int i=0; i < expr->value.call.arguments.size; i++){
                expression_to_str(str, expr->value.call.arguments.values[i]);
                if (i < (expr->value.call.arguments.size - 1)) {
                    strcat(str, ", ");
                }
            }
            strcat(str, ")");
        break;

        case EXPR_ARRAY: 
            strcat(str, "[");
            for (unsigned int i=0; i < expr->value.array.size; i++) {
                expression_to_str(str, expr->value.array.values[i]);

                if (i < (expr->value.array.size - 1)) {
                    strcat(str, ", ");
                }
            }
            strcat(str, "]");
        break;

        case EXPR_INDEX: 
            strcat(str, "(");
            expression_to_str(str, expr->value.index.left);
            strcat(str, "[");
            expression_to_str(str, expr->value.index.index);
            strcat(str, "])");
        break;
    }

}

char *program_to_str(struct program *p) {
    // TODO: Use some kind of buffer here that dynamically grows
    char *str = malloc(256);
    if (!str) {
        err(EXIT_FAILURE, "out of memory");
    }

    *str = '\0';

    for (unsigned int i = 0; i < p->size; i++) {  
        statement_to_str(str, &p->statements[i]);
        
        if (i < p->size -1) {
            str = realloc(str, sizeof str + 256);
        }
    }    

    return str;
}

enum operator parse_operator(enum token_type t) {
    switch (t) {
        case TOKEN_BANG: return OP_NEGATE; break;
        case TOKEN_MINUS: return OP_SUBTRACT; break;
        case TOKEN_PLUS: return OP_ADD; break;
        case TOKEN_ASTERISK: return OP_MULTIPLY; break;
        case TOKEN_GT: return OP_GT; break;
        case TOKEN_LT: return OP_LT; break;
        case TOKEN_EQ: return OP_EQ; break;
        case TOKEN_NOT_EQ: return OP_NOT_EQ; break;
        case TOKEN_SLASH: return OP_DIVIDE; break;
        default: 
        break;
    }

    return OP_UNKNOWN;
}

char *operator_to_str(enum operator operator) {
    switch (operator) {
        case OP_ADD: return "+"; break;
        case OP_SUBTRACT: return "-"; break;
        case OP_MULTIPLY: return "*"; break;
        case OP_DIVIDE: return "/"; break;
        case OP_NEGATE: return "!"; break;
        case OP_NOT_EQ: return "!="; break;
        case OP_EQ: return "=="; break;
        case OP_LT: return "<"; break;
        case OP_GT: return ">"; break;
        case OP_UNKNOWN: break;
    }

    return "???";
}

void free_statements(struct statement *stmts, unsigned int size) {
    for (unsigned int i=0; i < size; i++) {
        free_expression(stmts[i].value);
    }
    
    free(stmts);
}

void free_block_statement(struct block_statement *b) {
    free_statements(b->statements, b->size);
    free(b);
}

void free_expression(struct expression *expr) {
    if (expr == NULL) {
        return;
    }

    switch (expr->type) {
        case EXPR_PREFIX: 
            free_expression(expr->value.prefix.right);
        break;

        case EXPR_INFIX:
            free_expression(expr->value.infix.left);
            free_expression(expr->value.infix.right);
        break;

        case EXPR_FUNCTION:
            free(expr->value.function.parameters.values);
            free_block_statement(expr->value.function.body);
        break;

        case EXPR_IF:
            free_expression(expr->value.ifelse.condition);
            free_block_statement(expr->value.ifelse.consequence);

            if (expr->value.ifelse.alternative) {
                free_block_statement(expr->value.ifelse.alternative);
            }
        break;

        case EXPR_WHILE: 
            free_expression(expr->value.whilst.condition);
            free_block_statement(expr->value.whilst.body);
        break;

        case EXPR_CALL:
            for (unsigned int i=0; i < expr->value.call.arguments.size; i++) {
                free_expression(expr->value.call.arguments.values[i]);
            }
            free(expr->value.call.arguments.values);
            free_expression(expr->value.call.function);
        break;

        case EXPR_STRING:
            free(expr->value.string);
        break;

       case EXPR_ARRAY: 
            for (unsigned int i=0; i < expr->value.array.size; i++) {
                free_expression(expr->value.array.values[i]);
            }
            free(expr->value.array.values);
       break;

       case EXPR_INDEX: 
            free_expression(expr->value.index.left);
            free_expression(expr->value.index.index);
       break;

       case EXPR_INT: 
       case EXPR_IDENT: 
       case EXPR_BOOL: 
            //nothing to free
       break;
    }

    free(expr);
}

void free_program(struct program *p) {
    free_statements(p->statements, p->size);
    free(p);
}
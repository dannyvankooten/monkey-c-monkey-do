#include <stdio.h>
#include <string.h>
#include <stdlib.h> 
#include <stdarg.h> 

#include "../src/parser/parser.h"
#include "test_helpers.h"

void test_expression(struct expression *e, union expression_value expected);

void assert_parser_errors(struct parser *p) {
    if (p->errors > 0) {
        printf("parser has %d errors: \n", p->errors);
        for (unsigned int i = 0; i < p->errors; i++) {
            printf("  - %s\n", p->error_messages[i]);
        }

        exit(1);
    }
}

void assert_program_size(struct program *p, unsigned int expected_size) {
    assertf(p->size == expected_size, "wrong program size. expected %d, got %d\n", expected_size, p->size); 
}

void test_let_statements() {
    char *input = ""
        "let x = 5;\n"
        "let y = true;\n"
        "let foo = y;\n";

    struct lexer l = {input, 0 };
    struct parser parser = new_parser(&l);
    struct program *program = parse_program(&parser);
    
    assert_parser_errors(&parser);
    assert_program_size(program, 3);    

    struct test {
        char *literal;
        char *name;
        union expression_value value;
    } tests[3] = {
        {"let", "x", {.integer = 5}},
        {"let", "y", {.boolean = 1}},
        {"let", "foo", {.string = "y"}}
    };

    for (unsigned int i = 0; i < 3; i++) {
        struct statement stmt = program->statements[i];
        assertf(strcmp(stmt.token.literal, tests[i].literal) == 0, "wrong literal. expected %s, got %s\n", tests[i].literal, stmt.token.literal);
        assertf(strcmp(stmt.name.value, tests[i].name) == 0, "wrong name value. expected %s, got %s\n", tests[i].name, stmt.name.value);
        assertf(strcmp(stmt.name.token.literal, tests[i].name) == 0, "wrong name literal. expected %s, got %s", tests[i].name, stmt.token.literal);
        test_expression(stmt.value, tests[i].value);
    }

    free_program(program);
}


void test_return_statements() {
    char *input = ""
        "return 5;\n"
        "return true;\n"
        "return x;\n";

    struct lexer l = {input, 0 };
    struct parser parser = new_parser(&l);
    struct program *program = parse_program(&parser);
    
    assert_parser_errors(&parser);
    assert_program_size(program, 3);   

    struct test {
        char *literal;
        char *name;
        union expression_value value;
    } tests[3] = {
        {"return", "", {.integer = 5}},
        {"return", "", {.boolean = 1}},
        {"return", "", {.string = "x"}}
    };

    for (unsigned int i = 0; i < 3; i++) {
        struct statement stmt = program->statements[i];
        assertf(strcmp(stmt.token.literal, tests[i].literal) == 0, "wrong literal. expected %s, got %s\n", tests[i].literal, stmt.token.literal);
        test_expression(stmt.value, tests[i].value);
    }

    free_program(program);
}

void test_program_string() {
    struct expression e1 = {
        .type = EXPR_INT,
        .token = {
            .literal = "5",
            .type = TOKEN_INT,
        },
        .value.integer = 5,
    };
    struct expression e2 = {
        .type = EXPR_IDENT,
        .value.ident = {
            .value = "foo"
        },
    };
    struct expression expressions[] = {
        {
            .type = EXPR_IDENT,
            .value.ident = {
                .token = {
                    .type = TOKEN_IDENT,
                    .literal = "anotherVar",
                },
                .value = "anotherVar"
            }
        },
        {
            .type = EXPR_INFIX,
            .token = {
                .type = TOKEN_PLUS,
                .literal = "+",
            },
            .value.infix = {
                .operator = OP_ADD,
                .left = &e1,
                .right = &e2,
            }
        }
    };
    struct statement statements[] = {
        {
            .type = STMT_LET,
            .token = {
                .type = TOKEN_LET,
                .literal = "let",
            },
            .name = {
                .token = {
                    .type = TOKEN_IDENT,
                    .literal = "myVar",
                },
                .value = "myVar",
            },
            .value = &expressions[0],
        }, 
        {
            .type = STMT_RETURN,
            .token = {
                .type = TOKEN_RETURN,
                .literal = "return",
            },
            .value = &expressions[1]
        }, 
    };

    struct program program = {
        .statements = statements, 
        .size = 2
    };

    char *str = program_to_str(&program);
    char *expected = "let myVar = anotherVar;return (5 + foo);";
    assertf(strcmp(str, expected) == 0, "wrong program string. expected \"%s\", got \"%s\"\n", expected, str);
    free(str);
}

void test_identifier_expression(struct expression *e, char *expected) {
    assertf(e->type == EXPR_IDENT, "wrong expression type: expected %d, got %d\n", EXPR_IDENT, e->type);
    assertf(strcmp(e->value.ident.token.literal, expected) == 0, "wrong token literal: expected \"%s\", got \"%s\"\n", expected, e->value.ident.token.literal);
    assertf(strcmp(e->value.ident.value, expected) == 0, "wrong expression value: expected \"%s\", got \"%s\"\n", expected, e->value.ident.value);
}


void test_identifier_expression_parsing() {
    char *input = "foobar;";
    struct lexer l = {input, 0};
    struct parser parser = new_parser(&l);
    struct program *program = parse_program(&parser);
    assert_program_size(program, 1);

    struct statement stmt = program->statements[0];
    assertf(stmt.token.type == TOKEN_IDENT, "wrong token type: expected %s, got %s\n", token_type_to_str(TOKEN_IDENT), stmt.token.type);
    assertf(strcmp(stmt.token.literal, "foobar") == 0, "wrong token literal: expected %s, got %s\n", "foobar", stmt.token.literal);
    test_identifier_expression(stmt.value, "foobar");
    free_program(program);
}


void test_integer_expression(struct expression *expr, int expected) {
    assertf(expr->type == EXPR_INT, "wrong expression type: expected %d, got %d\n", EXPR_INT, expr->type);
    assertf(expr->value.integer == expected, "wrong integer value: expected %d, got %d\n", expected, expr->value.integer);

    char expected_str[8];
    sprintf(expected_str, "%d", expected);
    assertf(strcmp(expr->token.literal, expected_str) == 0, "wrong token literal: expected %s, got %s\n", expected_str, expr->token.literal);
}


void test_integer_expression_parsing() {
    char *input = "5;";
    struct lexer l = {input, 0};
    struct parser parser = new_parser(&l);
    struct program *program = parse_program(&parser);
    assert_parser_errors(&parser);
    assert_program_size(program, 1);

    struct statement stmt = program->statements[0];
    assertf(stmt.token.type == TOKEN_INT, "wroken token type: expected %s, got %s", token_type_to_str(TOKEN_INT), stmt.token.type);
    assertf (strcmp(stmt.token.literal, "5") == 0, "wrong token literal: expected %s, got %s", "foobar", stmt.token.literal);
    test_integer_expression(stmt.value, 5);
    free_program(program);
}


void test_boolean_expression(struct expression * expr, char expected) {
    assertf(expr->type == EXPR_BOOL, "wrong expression type: expected %d, got %d\n", EXPR_BOOL, expr->type);
    assertf(expr->value.boolean == expected, "wrong boolean value: expected %d, got %d\n", expected, expr->value.boolean);
    
    char *expected_str = expected ? "true" : "false";
    assertf(strcmp(expr->token.literal, expected_str) == 0, "wrong token literal: expected %s, got %s\n", expected_str, expr->token.literal);
}

void test_boolean_expression_parsing() {
    struct test {
        char * input;
        char expected;
    } tests[] = {
      {"true;", 1},
      {"false;", 0}
    };

    for (unsigned int i=0; i < sizeof tests / sizeof tests[0]; i++) {
        struct lexer l = {tests[i].input, 0};
        struct parser parser = new_parser(&l);
        struct program *program = parse_program(&parser);

        assert_parser_errors(&parser);
        assert_program_size(program, 1);
        struct statement stmt = program->statements[0];

        test_boolean_expression(stmt.value, tests[i].expected);
        free_program(program);
    }
}

void test_expression(struct expression *e, union expression_value expected) {
    switch (e->type) {
        case EXPR_BOOL: test_boolean_expression(e, expected.boolean); break;
        case EXPR_INT: test_integer_expression(e, expected.integer); break;
        case EXPR_IDENT: test_identifier_expression(e, expected.string); break;
        default: break;
    }
}

void test_infix_expression(struct expression *expr, union expression_value left_value, enum operator operator, union expression_value right_value) {
    assertf(expr->type == EXPR_INFIX, "wrong expression type. expected %d, got %d\n", EXPR_INFIX, expr->type);
    test_expression(expr->value.infix.left, left_value);
    assertf(expr->value.infix.operator == operator, "wrong operator: expected %d, got %d\n", operator, expr->value.infix.operator);
    test_expression(expr->value.infix.right, right_value);
}

void test_infix_expression_parsing() {
    struct test{
        char *input;
        union expression_value left_value;
        enum operator operator;
        union expression_value right_value;        
    };
    struct test tests[] = {
       {"5 + 5", {5}, OP_ADD, {5}},
       {"5 - 5", {5}, OP_SUBTRACT, {5}},
       {"5 * 5", {5}, OP_MULTIPLY, {5}},
       {"5 / 5", {5}, OP_DIVIDE, {5}},
       {"5 > 5", {5}, OP_GT, {5}},
       {"5 < 5", {5}, OP_LT, {5}},
       {"5 == 5", {5}, OP_EQ, {5}},
       {"5 != 5", {5}, OP_NOT_EQ, {5}},
       {"true == true", {1}, OP_EQ, {1}},
       {"true != false", {1}, OP_NOT_EQ, {0}},
       {"false == false", {0}, OP_EQ, {0}},
    };

    for (unsigned int i=0; i < sizeof tests / sizeof tests[0]; i++) {
        struct test t = tests[i];
        struct lexer l = new_lexer(t.input);
        struct parser parser = new_parser(&l);
        struct program *program = parse_program(&parser);
        assert_parser_errors(&parser);
        assert_program_size(program, 1);
        struct statement stmt = program->statements[0];
        test_infix_expression(stmt.value, t.left_value, t.operator, t.right_value);
        free_program(program);   
    }
}

void test_prefix_expression_parsing() {
    typedef struct test {
        char *input;
        enum operator operator;
        union expression_value value;
    } test;

    test tests[] = {
        {"!5", OP_NEGATE, { .integer = 5 }},
        {"-15", OP_SUBTRACT, { .integer = 15 }},
        {"!true", OP_NEGATE, { .boolean = 1 }},
        {"!false", OP_NEGATE, { .boolean = 0 }},
    };
    test t;
    for (unsigned int i=0; i < sizeof tests / sizeof tests[0]; i++) {
        t = tests[i];
        struct lexer l = {tests[i].input, 0};
        struct parser parser = new_parser(&l);
        struct program *program = parse_program(&parser);

        assert_parser_errors(&parser);
        assert_program_size(program, 1);
        struct statement stmt = program->statements[0];

        assertf(stmt.value->type == EXPR_PREFIX, "wrong expression type. expected %d, got %d\n", EXPR_PREFIX, stmt.value->type);
        assertf(stmt.value->value.prefix.operator == t.operator, "wrong operator. expected %d, got %d\n", t.operator, stmt.value->value.prefix.operator);
        test_expression(stmt.value->value.prefix.right, t.value); 
        free_program(program);       
    }
}

void test_operator_precedence_parsing() {
    struct test {
        char *input;
        char *expected;
    } tests[] = {
       {"-a * b", "((-a) * b)"},
       {"!-a", "(!(-a))"},
       {"a + b + c", "((a + b) + c)"},
       {"a + b - c", "((a + b) - c)"},
       {"a * b * c", "((a * b) * c)"},
       {"a * b / c", "((a * b) / c)"},
       {"a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"},
       {"3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"},
       {"5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"},
       {"5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"},
       {"3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"},
       {"true", "true"},
       {"false", "false"},
       {"3 > 5 == false", "((3 > 5) == false)"},
       {"3 < 5 == true", "((3 < 5) == true)"},
       {"1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"},
       {"(5 + 5) * 2", "((5 + 5) * 2)"},
       {"2 / ( 5 + 5)", "(2 / (5 + 5))"},
       {"-(5 + 5)", "(-(5 + 5))"},
       {"!(true == true)", "(!(true == true))"},
       {"a + add(b * c) +d", "((a + add((b * c))) + d)"},
       {"add(a, b, 1, 2 * 3, 4 + 5, add(6, 7* 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"},
       {"add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"},
       {
        "a * [1, 2, 3, 4][b * c] * d",
        "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        },
        {
        "add(a * b[2], b[1], 2 * [1, 2][1])",
        "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        },
    };

    for (unsigned int i=0; i < sizeof tests / sizeof tests[0]; i++) {
        struct lexer l = {tests[i].input, 0};
        struct parser parser = new_parser(&l);
        struct program *program = parse_program(&parser);
        assert_parser_errors(&parser);
        
        char *program_str = program_to_str(program);
        assertf(strcmp(program_str, tests[i].expected) == 0, "wrong program string: expected %s, got %s\n", tests[i].expected, program_str);        
        free(program_str);
        free_program(program);
    }
}

void test_if_expression_parsing() {
    char *input = "if (x < y) { x }";
    struct lexer lexer = {input, 0};
    struct parser parser = new_parser(&lexer);
    struct program *program = parse_program(&parser);
    assert_parser_errors(&parser);
    assert_program_size(program, 1);

    struct statement stmt = program->statements[0];
    struct expression *expr = stmt.value;
    assertf (expr->type == EXPR_IF, "invalid statement type: expected %d, got %d\n", EXPR_IF, stmt.type);

    union expression_value left = {.string = "x"};
    union expression_value right = {.string = "y"};
    test_infix_expression(expr->value.ifelse.condition, left, OP_LT, right);

    struct block_statement *consequence = expr->value.ifelse.consequence;
    assertf(!!consequence, "expected consequence block statement, got NULL\n");
    assertf(consequence->size == 1, "invalid consequence size: expected %d, got %d\n", 1, consequence->size);
    assertf(consequence->statements[0].type == STMT_EXPR, "statements[0] is not a statement expression, got %d\n", consequence->statements[0].type);
    test_identifier_expression(consequence->statements[0].value, "x");
    assertf(!expr->value.ifelse.alternative, "expected NULL, got alternative block statement\n");
    free_program(program);
}

void test_if_else_expression_parsing() {
    char *input = "if (x < y) { x } else { 5 }";
    struct lexer lexer = {input, 0};
    struct parser parser = new_parser(&lexer);
    struct program *program = parse_program(&parser);
    assert_parser_errors(&parser);
    assert_program_size(program, 1);

    struct statement stmt = program->statements[0];
    struct expression *expr = stmt.value;
    assertf(expr->type == EXPR_IF, "invalid statement type: expected %d, got %d\n", EXPR_IF, stmt.type);

    union expression_value left = {.string = "x"};
    union expression_value right = {.string = "y"};
    test_infix_expression(expr->value.ifelse.condition, left, OP_LT, right);

    struct block_statement *consequence = expr->value.ifelse.consequence;
    assertf(!!consequence, "expected consequence block statement, got NULL\n");
    assertf(consequence->size == 1, "invalid consequence size: expected %d, got %d\n", 1, consequence->size);
    assertf(consequence->statements[0].type == STMT_EXPR, "statements[0] is not a statement expression, got %d", consequence->statements[0].type);
    test_identifier_expression(consequence->statements[0].value, "x");

    struct block_statement *alternative = expr->value.ifelse.alternative;
    assertf(alternative != NULL, "expected alternative, got NULL");
    assertf(alternative->size == 1, "invalid alternative size: expected %d, got %d\n", 1, alternative->size);
    free_program(program);
}

void test_function_literal_parsing() {
    char *input = "fn(x, y) { x + y; }";
    struct lexer lexer = {input, 0};
    struct parser parser = new_parser(&lexer);
    struct program *program = parse_program(&parser);
    assert_parser_errors(&parser);
    assert_program_size(program, 1);

    struct statement stmt = program->statements[0];
    assertf(stmt.type == STMT_EXPR, "invalid statement type. expected STMT_EXPR, got %d\n", stmt.type);

    struct expression *expr = stmt.value;
    assertf(expr->type == EXPR_FUNCTION, "invalid expression type: expected EXPR_FUNCTION, got %d\n", expr->type);
    
    assertf(expr->value.function.parameters.size == 2, "invalid param size: expected %d, got %d\n", 2, expr->value.function.parameters.size);
    assertf(strcmp(expr->value.function.parameters.values[0].value, "x") == 0, "invalid parameter[0]: expected %s, got %s\n", "x", expr->value.function.parameters.values[0].value);
    assertf(strcmp(expr->value.function.parameters.values[1].value, "y") == 0, "invalid parameter[0]: expected %s, got %s\n", "x", expr->value.function.parameters.values[1].value);
    assertf(expr->value.function.body->size == 1, "invalid body size: expected %d, got %d\n", 1, expr->value.function.body->size);
    
    union expression_value left = {.string = "x"};
    enum operator op = OP_ADD;
    union expression_value right = {.string = "y"};
    test_infix_expression(expr->value.function.body->statements[0].value, left, op, right);
    free_program(program);
}

void test_call_expression_parsing() {
    char *input = "add(1, 2 * 3, 4 + 5);";
    struct lexer lexer = {input, 0};
    struct parser parser = new_parser(&lexer);
    struct program *program = parse_program(&parser);
    assert_parser_errors(&parser);
    assert_program_size(program, 1);

    struct statement stmt = program->statements[0];
    assertf(stmt.type == STMT_EXPR, "invalid statement type. expected STMT_EXPR, got %d\n", stmt.type);

    struct expression *expr = stmt.value;
    assertf(expr->type == EXPR_CALL, "invalid expression type: expected EXPR_CALL, got %d\n", expr->type);
    test_identifier_expression(expr->value.call.function, "add");
    assertf(expr->value.call.arguments.size == 3, "expected 3 arguments, got %d\n", expr->value.call.arguments.size);


    struct {
        union expression_value left;
        enum operator op;
        union expression_value right;
    } tests[] = {
        { .left = { .integer = 1 } },
        { 
            .left = { .integer = 2 },
            .op = OP_MULTIPLY,
            .right = { .integer = 3 },
        },
        { 
            .left = { .integer = 4 },
            .op = OP_ADD,
            .right = { .integer = 5 },
        },
    };

    test_integer_expression(expr->value.call.arguments.values[0], tests[0].left.integer);
    test_infix_expression(expr->value.call.arguments.values[1], tests[1].left, tests[1].op, tests[1].right);
    test_infix_expression(expr->value.call.arguments.values[2], tests[2].left, tests[2].op, tests[2].right);
    free_program(program);
}


void test_string_literal(struct expression *expr, char *expected) {
    assertf(expr->type == EXPR_STRING, "wrong expression type: expected EXPR_STRING, got %s", expr->type);
    assertf(strcmp(expr->value.string, expected) == 0, "wrong expression value: expected \"%s\", got %s", expected, expr->value.string);
}

void test_string_expression_parsing() {
    char *input = "\"hello world\";";
    struct lexer l = {input, 0};
    struct parser parser = new_parser(&l);
    struct program *program = parse_program(&parser);
    assert_parser_errors(&parser);
    assert_program_size(program, 1);

    struct statement stmt = program->statements[0];
    assertf(stmt.token.type == TOKEN_STRING, "wroken token type: expected %s, got %s", token_type_to_str(TOKEN_STRING), stmt.token.type);
    assertf (strcmp(stmt.token.literal, "hello world") == 0, "wrong token literal: expected %s, got %s", "hello world", stmt.token.literal);
    test_string_literal(stmt.value, "hello world");
    free_program(program);
}

void test_array_literal_parsing() {
    char *input = "[ 1, 2 * 2, 3 + 3, \"four\"];";
    struct lexer l = {input, 0};
    struct parser parser = new_parser(&l);
    struct program *program = parse_program(&parser);
    assert_parser_errors(&parser);
    assert_program_size(program, 1);

    struct statement stmt = program->statements[0];
    assertf(stmt.value->type == EXPR_ARRAY, "wrong expression type: expected EXPR_ARRAY, got %s", stmt.value->type);

    struct expression_list array = stmt.value->value.array;
    assertf(array.size == 4, "wrong array size: expected 4, got %d", array.size);

    test_integer_expression(array.values[0], 1);
    // TODO: Test infix expressions as well
    test_string_literal(array.values[3], "four");
    free_program(program);
}

void test_index_expression_parsing() {
    char *input = "myArray[1+2];";
    struct lexer l = {input, 0};
    struct parser parser = new_parser(&l);
    struct program *program = parse_program(&parser);
    assert_parser_errors(&parser);
    assert_program_size(program, 1);

    struct statement stmt = program->statements[0];
    assertf(stmt.value->type == EXPR_INDEX, "wrong expression type: expected EXPR_INDEX, got %s", stmt.value->type);

    struct index_expression expr = stmt.value->value.index;
    test_identifier_expression(expr.left, "myArray");

    union expression_value left = {.integer = 1};
    union expression_value right = {.integer = 2};

    test_infix_expression(expr.index, left, OP_ADD, right);
    free_program(program);
}


void test_while_expression_parsing() {
    char *input = "while (x < y) { x }";
    struct lexer lexer = {input, 0};
    struct parser parser = new_parser(&lexer);
    struct program *program = parse_program(&parser);
    assert_parser_errors(&parser);
    assert_program_size(program, 1);

    struct statement stmt = program->statements[0];
    struct expression *expr = stmt.value;
    assertf (expr->type == EXPR_WHILE, "invalid statement type: expected %d, got %d\n", EXPR_WHILE, stmt.type);

    union expression_value left = {.string = "x"};
    union expression_value right = {.string = "y"};
    test_infix_expression(expr->value.whilst.condition, left, OP_LT, right);

    struct block_statement *body = expr->value.whilst.body;
    assertf(!!body, "expected consequence block statement, got NULL\n");
    assertf(body->size == 1, "invalid consequence size: expected %d, got %d\n", 1, body->size);
    assertf(body->statements[0].type == STMT_EXPR, "statements[0] is not a statement expression, got %d\n", body->statements[0].type);
    test_identifier_expression(body->statements[0].value, "x");
    free_program(program);
}

int main() {
    test_let_statements();
    test_return_statements();
    test_program_string();
    test_identifier_expression_parsing();
    test_integer_expression_parsing();
    test_boolean_expression_parsing();
    test_prefix_expression_parsing();
    test_infix_expression_parsing();
    test_operator_precedence_parsing();
    test_if_expression_parsing();
    test_if_else_expression_parsing();
    test_function_literal_parsing();
    test_call_expression_parsing();
    test_string_expression_parsing();
    test_array_literal_parsing();
    test_index_expression_parsing();
    test_while_expression_parsing();
    printf("\x1b[32mAll parsing tests passed!\033[0m\n");
}

#ifndef OBJECT_H
#define OBJECT_H

#include <stdbool.h>
#include "env.h"
#include "parser/parser.h"

#define is_object_error(t) (t == OBJ_ERROR)
#define is_object_truthy(obj) (obj != object_null && obj != object_false)
#define make_boolean_object(value) (value ? object_true : object_false)

// TODO: Dynamically allocate this
#define OBJECT_LIST_MAX_VALUES 64

enum object_type
{
    OBJ_NULL,
    OBJ_BOOL,
    OBJ_INT,
    OBJ_ERROR,
    OBJ_FUNCTION,
    OBJ_STRING,
    OBJ_BUILTIN,
    OBJ_ARRAY,
};

struct function {
    struct identifier_list *parameters;
    struct block_statement *body;
    struct environment *env;
};

struct object_list {
    struct object *values[OBJECT_LIST_MAX_VALUES];
    unsigned int size;

    // for linking in pool
    struct object_list *next;
};

struct object
{
    enum object_type type;
    char *name;
    union {
        bool boolean;
        long integer;
        char *error;
        char *string;
        struct function function;
        struct object *(*builtin)(struct object_list *);
        struct object_list *array;
    };
    bool return_value;
    struct object *next;
};

extern struct object *object_null;
extern struct object *object_null_return;
extern struct object *object_true;
extern struct object *object_false;
extern struct object *object_true_return;
extern struct object *object_false_return;

const char *object_type_to_str(enum object_type t);
struct object *make_integer_object(long value);
struct object *make_string_object(char *str1, char *str2);
struct object *make_error_object(char *format, ...);
struct object *make_array_object(struct object_list *elements);
struct object *make_function_object(struct identifier_list *parameters, struct block_statement *body, struct environment *env);
struct object *copy_object(struct object *obj);
void free_object(struct object *obj);
void object_to_str(char *str, struct object *obj);

struct object_list *make_object_list(unsigned int cap);
void free_object_list(struct object_list *list);
void free_object_list_pool();
void free_object_pool();

#endif
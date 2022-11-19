#include <err.h> 
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h> 

#include "./object.h"
#include "./../parser/parser.h"

struct object_list *copy_object_list(struct object_list *original);

static struct object _object_null = {
    .type = OBJ_NULL,
    .return_value = false,
};
static struct object _object_null_return = {
    .type = OBJ_NULL,
    .return_value = true,
};
static struct object _object_true = {
    .type = OBJ_BOOL,
    .value.boolean = true,
    .return_value = false,
};
static struct object _object_false = {
    .type = OBJ_BOOL,
    .value.boolean = false,
    .return_value = false,
};
static struct object _object_true_return = {
    .type = OBJ_BOOL,
    .value.boolean = true,
    .return_value = true,
};
static struct object _object_false_return = {
    .type = OBJ_BOOL,
    .value.boolean = false,
    .return_value = true,
};
struct object *object_null = &_object_null;
struct object *object_null_return = &_object_null_return;
struct object *object_true = &_object_true;
struct object *object_false = &_object_false;
struct object *object_true_return = &_object_true_return;
struct object *object_false_return = &_object_false_return;
struct object *object_pool_head = NULL;
struct object_list *object_list_pool_head = NULL;

static const char *object_names[] = {
    "NULL",
    "BOOLEAN",
    "INTEGER",
    "ERROR",
    "FUNCTION",
    "STRING",
    "BUILTIN",
    "ARRAY",
};

const char *object_type_to_str(enum object_type t)
{
    return object_names[t];
}


struct object *make_object(enum object_type type) {
   struct object *obj = object_pool_head;

   if (!obj) {
       obj = malloc(sizeof (*obj));

       if (!obj) {
           err(EXIT_FAILURE, "out of memory");
       }
   } else {
       object_pool_head = obj->next;
   }

   obj->type = type;
   obj->next = NULL;
   obj->name = NULL;
   obj->return_value = false;
   return obj;
}

struct object *make_integer_object(long value)
{
    struct object *obj = make_object(OBJ_INT);
    obj->value.integer = value;
    return obj;
}

struct object *make_array_object(struct object_list *elements) {
    struct object *obj = make_object(OBJ_ARRAY);
    obj->value.array = copy_object_list(elements);
    return obj;
}

struct object *make_string_object(char *str1, char *str2)
{
    struct object *obj = make_object(OBJ_STRING);
    
    // allocate enough memory to fit both strings
    int l = strlen(str1) + (str2 ? strlen(str2) : 0) + 1;
    obj->value.string = malloc(l);
    if (!obj->value.string) {
        err(EXIT_FAILURE, "out of memory");
    }

    // piece strings together
    strcpy(obj->value.string, str1);
    if (str2) {
        strcat(obj->value.string, str2);
    }
   
    return obj;
}


struct object *make_error_object(char *format, ...) {
    va_list args;

    struct object *obj = make_object(OBJ_ERROR);

    size_t l = strlen(format);
    obj->value.error = malloc(l + 64);
    if (!obj->value.error) {
        err(EXIT_FAILURE, "out of memory");
    }

    // always return error objects
    obj->return_value = true;
    va_start(args, format);  
    vsnprintf(obj->value.error, l + 64, format, args);
    va_end(args);
    return obj;
}


struct object *make_function_object(struct identifier_list *parameters, struct block_statement *body, struct environment *env) {
    struct object *obj = make_object(OBJ_FUNCTION);
    obj->value.function.parameters = parameters;
    obj->value.function.body = body;
    obj->value.function.env = env;
    return obj;
}


struct object *copy_object(struct object *obj) {
    switch (obj->type) {
        case OBJ_BOOL:
        case OBJ_NULL:
        case OBJ_BUILTIN:
            return obj;
            break;

        case OBJ_INT:
            return make_integer_object(obj->value.integer);
            break;

        case OBJ_FUNCTION:
            return make_function_object(obj->value.function.parameters, obj->value.function.body, obj->value.function.env);
            break;

        case OBJ_ERROR: 
            return make_error_object(obj->value.error);
            break;
        
        case OBJ_STRING:
            return make_string_object(obj->value.string, NULL);
            break;

        case OBJ_ARRAY: 
            return make_array_object(obj->value.array);
            break;
    }

    // TODO: This should not be reached, but also potential problem later on
    return obj;
}


void free_object(struct object *obj)
{   
    if (obj->name) {
        return;
    }

    switch (obj->type) {
        case OBJ_NULL: 
        case OBJ_BOOL: 
        case OBJ_BUILTIN:
            // never free static objects
            return;
            break;

        case OBJ_ERROR:
            free(obj->value.error);
            obj->value.error = NULL;
            break;

        case OBJ_ARRAY: 
            free_object_list(obj->value.array);
            obj->value.array = NULL;
            break;

        case OBJ_STRING: 
            free(obj->value.string);
            obj->value.string = NULL;
            break;

        case OBJ_FUNCTION:
            // free_environment(obj->value.function.env);
            // obj->value.function.env = NULL;
            break;

       default:
           // nothing special
           break;
    }

     // add to start of free object list
    obj->next = object_pool_head;
    object_pool_head = obj;
}


struct object_list *make_object_list(unsigned int cap) {
   struct object_list *list = object_list_pool_head;

   if (!list) {
       list = malloc(cap * sizeof (*list));
       if (!list) {
           err(EXIT_FAILURE, "out of memory");
       }
   } else {
        object_list_pool_head = list->next;
        list->next = NULL;
   }
   list->size = 0;

   return list;
}

void free_object_list(struct object_list *list) {
    // reset list values and size
    for (unsigned int i=0; i < list->size; i++) {
        free_object(list->values[i]);
        list->values[i] = NULL;
    }
    list->size = 0;

    // return to pool
    list->next = object_list_pool_head;
    object_list_pool_head = list;
}


struct object_list *copy_object_list(struct object_list *original) {
    struct object_list *new = make_object_list(original->size);
    int size = original->size;
    for (int i=0; i < size; i++) {
        new->values[i] = copy_object(original->values[i]);
    }
    new->size = size;
    return new;
}

void object_to_str(char *str, struct object *obj)
{
    char tmp[64];

    switch (obj->type)
    {
    case OBJ_NULL:
        strcat(str, "NULL");
        break;
        
    case OBJ_INT:
        sprintf(tmp, "%ld", obj->value.integer);
        strcat(str, tmp);
        break;
        
    case OBJ_BOOL:
        strcat(str, (obj == object_true  || obj == object_true_return) ? "true" : "false");
        break;
        
    case OBJ_ERROR: 
        strcat(str, obj->value.error);
        break;  
         
    case OBJ_FUNCTION: 
        strcat(str, "fn(");
        identifier_list_to_str(str, obj->value.function.parameters);
        strcat(str, ") {\n");
        block_statement_to_str(str, obj->value.function.body);
        strcat(str, "\n}");
        break;

    case OBJ_STRING: 
        strcat(str, obj->value.string);
        break;

    case OBJ_BUILTIN: 
        strcat(str, "builtin function");
        break;    

    case OBJ_ARRAY: 
        strcat(str, "[");
        for (unsigned int i=0; i < obj->value.array->size; i++) {
            object_to_str(str, obj->value.array->values[i]);
            if (i < (obj->value.array->size - 1)) {
                strcat(str, ", ");
            }
        }
        strcat(str, "]");
    break;
    }
}

void free_object_pool() {
    struct object *node = object_pool_head;
    struct object *next = NULL;

    while (node) {
        next = node->next;
        free(node);
        node = next;
    }

    object_pool_head = NULL;
}

void free_object_list_pool() {
    struct object_list *node = object_list_pool_head;
    struct object_list *next = NULL;

    while (node) {
        next = node->next;
        free(node);
        node = next;
    }

    object_list_pool_head = NULL;
}
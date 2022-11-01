#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "util.h"
#include "object.h"
#include "opcode.h"
#include "vm.h"
#include "builtins.h"

#define vm_current_frame(vm) (vm->frames[vm->frame_index])
#define vm_stack_pop_ignore(vm) vm->stack_pointer -= 1;
#define vm_stack_pop(vm) (vm->stack->values[--vm->stack_pointer])
#define vm_stack_cur(vm) (vm->stack->values[vm->stack_pointer - 1])
#define vm_stack_push(vm, obj) vm->stack = insert_in_object_list(vm->stack, vm->stack_pointer++, obj);

#ifndef DEBUG 
    #define DISPATCH() goto *dispatch_table[*frame->ip];        
#else 
    #define DISPATCH()                      \
        print_debug_info(vm);               \
        goto *dispatch_table[*frame->ip];      


static void 
print_debug_info(struct vm *vm) {
    char str[BUFSIZ] = {'\0'};
    struct frame *frame = &vm_current_frame(vm);

    int ip_now = frame->ip - frame->fn->instructions.bytes;
    int ip_end = frame->fn->instructions.size - 1;
    printf("\n\nFrame: %2ld | IP: %3d/%d | opcode: %12s | operand: ", vm->frame_index, ip_now, ip_end, opcode_to_str(*frame->ip));
    struct definition def = lookup(*frame->ip);
    if (def.operands > 0) {
        if (def.operand_widths[0] > 1) {
            printf("%3d\n", read_uint16(frame->ip + 1));
        } else {
            printf("%3d\n", read_uint8(frame->ip + 1));
        }
        
    } else {
        printf("-\n");
    }

    printf("Constants: \n");
    for (int i = 0; i < vm->constants->size; i++) {
        str[0] = '\0';
        object_to_str(str, vm->constants->values[i]);
        printf("  %3d: %s = %s\n", i, object_type_to_str(vm->constants->values[i].type), str);
    }

    printf("Globals: \n");
    for (int i = 0; i < vm->globals->size; i++) {
        if (vm->globals->values[i].type == OBJ_NULL) {
            break;
        }
        str[0] = '\0';
        object_to_str(str, vm->globals->values[i]);
        printf("  %3d: %s = %s\n", i, object_type_to_str(vm->globals->values[i].type), str);
    }

    printf("Stack: \n");
    for (int i=0; i < vm->stack_pointer; i++) {
        str[0] = '\0';
        object_to_str(str, vm->stack->values[i]);
        printf("  %3d: %s = %s\n", i, object_type_to_str(vm->stack->values[i].type), str);
    }
}
#endif 

static void gc(struct vm* vm);
static struct object_list *_builtin_args_list;

struct vm *vm_new(struct bytecode *bc) {
    struct vm *vm = malloc(sizeof *vm);
    assert(vm != NULL);
    vm->stack_pointer = 0;
    vm->frame_index = 0;

    _builtin_args_list = make_object_list(16);
    vm->stack = make_object_list(64);
    vm->globals = make_object_list(64);
    vm->constants = copy_object_list(bc->constants);
    vm->heap = make_object_list(64);

    // copy instruction as we are not adding this compiled function to a constant list
    // the bytes are freed through the bytecode object
    // TODO: Simplify this
    struct instruction *ins = malloc(sizeof (struct instruction));
    assert(ins != NULL);
    memcpy(ins, bc->instructions, sizeof(struct instruction));
    struct object fn_obj = make_compiled_function_object(ins, 0);
    struct compiled_function* fn = (struct compiled_function*) fn_obj.value.ptr->value;
    vm->frames[0].ip = fn->instructions.bytes;
    vm->frames[0].fn = fn;
    vm->frames[0].base_pointer = 0;
    free(fn_obj.value.ptr);
    return vm;
}

struct vm *vm_new_with_globals(struct bytecode *bc, struct object_list *globals) {
    struct vm *vm = vm_new(bc);

    free_object_list(vm->globals);
    vm->globals = globals;

    return vm;
}

void vm_free(struct vm *vm) {
    /* free initial compiled function since it's not on the constants list */
    free(vm->frames[0].fn);

    // free args list for builtin functions
    free_object_list(_builtin_args_list);

    // free all objects on heap
    free_object_list(vm->heap);

    /* free vm itself */
    free(vm);
}


static void inline 
vm_do_binary_integer_operation(const struct vm* restrict vm, const enum opcode opcode, struct object* restrict left, const struct object* restrict right) {    
    switch (opcode) {
        case OPCODE_ADD: 
            left->value.integer += right->value.integer;
        break;
        case OPCODE_SUBTRACT: 
            left->value.integer -= right->value.integer;
        break;
        case OPCODE_MULTIPLY: 
            left->value.integer *= right->value.integer;
        break;
        case OPCODE_DIVIDE: 
            left->value.integer /= right->value.integer;
        break;
        case OPCODE_MODULO:
            left->value.integer %= right->value.integer;
        break;
        default:
            err(VM_ERR_INVALID_INT_OPERATOR, "Invalid operator for integer operation.");
        break;
    }
}

static void 
vm_do_binary_string_operation(struct vm* restrict vm, const enum opcode opcode, struct object* restrict left, const struct object* restrict right) {
    struct object o = make_string_object(left->value.ptr->value, right->value.ptr->value); 
    vm_stack_cur(vm) = o;

    // register in heap
    vm->heap->values[vm->heap->size++] = o;

    // run garbage collector
    gc(vm);
}

static void inline
vm_do_binary_boolean_operation(const struct vm* restrict vm, const enum opcode opcode, struct object* restrict left, const struct object* restrict right) {    
    switch (opcode) {
        case OPCODE_AND: 
            left->value.boolean = left->value.boolean && right->value.boolean;
        break;
        case OPCODE_OR: 
            left->value.boolean = left->value.boolean || right->value.boolean;
        break;
        default:
            err(VM_ERR_INVALID_BOOL_OPERATOR, "Invalid operator for boolean operation.");
        break;
    }
}

static void inline 
vm_do_binary_operation(struct vm* restrict vm, const enum opcode opcode) {
    const struct object* right = &vm_stack_pop(vm);
    struct object* left = &vm_stack_cur(vm);
    assert(left->type == right->type);

    switch (left->type) {
        case OBJ_INT: 
            return vm_do_binary_integer_operation(vm, opcode, left, right); 
        break;
        case OBJ_STRING: 
            return vm_do_binary_string_operation(vm, opcode, left, right); 
        break;
        case OBJ_BOOL:
            return vm_do_binary_boolean_operation(vm, opcode, left, right);
        break;
        default: 
            err(VM_ERR_INVALID_OP_TYPE, "Invalid type for binary operation.");
        break;
    }
}

static void inline 
vm_do_integer_comparison(const struct vm* restrict vm, const enum opcode opcode, struct object* restrict left, const struct object* restrict right) {   
    switch (opcode) {
         case OPCODE_EQUAL: 
            left->value.boolean = left->value.integer == right->value.integer;
            break;

        case OPCODE_NOT_EQUAL: 
            left->value.boolean = left->value.integer != right->value.integer;
            break;

        case OPCODE_GREATER_THAN: 
            left->value.boolean = left->value.integer > right->value.integer;
            break;

        case OPCODE_GREATER_THAN_OR_EQUALS: 
            left->value.boolean = left->value.integer >= right->value.integer;
            break;

        case OPCODE_LESS_THAN:
            left->value.boolean = left->value.integer < right->value.integer;
            break;

        case OPCODE_LESS_THAN_OR_EQUALS:
            left->value.boolean = left->value.integer <= right->value.integer;
            break;

        default: 
            err(VM_ERR_INVALID_OP_TYPE, "Invalid operator for integer comparison");
        break;
    }

    left->type = OBJ_BOOL;
}

static void
vm_do_bool_comparison(const struct vm* restrict vm, const enum opcode opcode, struct object* restrict left, const struct object* restrict right) {
    switch (opcode) {
        case OPCODE_EQUAL: 
            left->value.boolean = left->value.boolean == right->value.boolean;
        break;

        case OPCODE_NOT_EQUAL: 
            left->value.boolean = left->value.boolean != right->value.boolean;
        break;

        default: 
            err(VM_ERR_INVALID_OP_TYPE, "Invalid operator for boolean comparison.");
        break;
    }    
}

static void inline 
vm_do_comparision(struct vm* restrict vm, const enum opcode opcode) {
    const struct object* right = &vm_stack_pop(vm);
    struct object* left = &vm_stack_cur(vm);
    assert(left->type == right->type);

    switch (left->type) {
        case OBJ_INT:
            return vm_do_integer_comparison(vm, opcode, left, right);
        break;

        case OBJ_BOOL:
            return vm_do_bool_comparison(vm, opcode, left, right);
        break;

        default:
            err(VM_ERR_INVALID_OP_TYPE, "Invalid type for comparison.");
        break;
    }   
}

static void
vm_do_bang_operation(struct vm * restrict vm) {
    // modify item in place by leaving it on the stack
    struct object* obj = &vm_stack_cur(vm);
    obj->value.boolean = obj->type == OBJ_NULL || (obj->type == OBJ_BOOL && obj->value.boolean == false) || (obj->type == OBJ_INT && obj->value.integer <= 0);
    obj->type = OBJ_BOOL;
}

static void  
vm_do_minus_operation(struct vm* restrict vm) {
    // modify item in place by leaving it on the stack
    vm_stack_cur(vm).value.integer *= -1;
}

/* handle call to built-in function */
static void 
vm_do_call_builtin(struct vm* restrict vm, struct object (*builtin)(struct object_list *), const uint8_t num_args) {
    struct object_list *args = _builtin_args_list;
    
    for (uint32_t i = vm->stack_pointer - num_args; i < vm->stack_pointer; i++) {
        args->values[args->size++] = vm->stack->values[i];
    }  

    struct object obj = builtin(args);
    vm->stack_pointer = vm->stack_pointer - num_args - 1;
    vm_stack_push(vm, obj);
    vm_current_frame(vm).ip++;
    
    // reset args for next use
    args->size = 0;

    // register result object in heap for GC
    if(obj.type > OBJ_INT) {
        vm->heap->values[vm->heap->size++] = obj;
        gc(vm);
    }
}

/* handle call to user-defined function */
static void inline 
vm_do_call_function(struct vm* restrict vm, struct compiled_function* restrict f, const uint8_t num_args) {
    /* TODO: Validate number of arguments */
    struct frame* frame = &vm->frames[++vm->frame_index];
    frame->ip = f->instructions.bytes;
    frame->fn = f;
    frame->base_pointer = vm->stack_pointer - num_args;
    vm->stack_pointer = frame->base_pointer + f->num_locals; 
}

static void inline 
vm_do_call(struct vm* restrict vm, const uint8_t num_args) {
    const struct object callee = vm->stack->values[vm->stack_pointer - 1 - num_args];
    switch (callee.type) {
        case OBJ_COMPILED_FUNCTION:
            return vm_do_call_function(vm, callee.value.ptr->value, num_args);
        break;

        case OBJ_BUILTIN:
            return vm_do_call_builtin(vm, (struct object (*)(struct object_list *)) callee.value.ptr, num_args);
        break;

        default:
            err(VM_ERR_INVALID_FUNCTION_CALL, "Invalid function call.");
        break;
    }
}

struct object 
vm_build_array(struct vm* restrict vm, const uint16_t start_index, const uint16_t end_index) {
    struct object_list *list = make_object_list(end_index - start_index);
    for (int32_t i = start_index; i < end_index; i++) {
        list->values[list->size++] = copy_object(&vm->stack->values[i]);
    }
    struct object o = make_array_object(list);

    // register array in heap for GC 
    vm->heap->values[vm->heap->size++] = o;
    return o;
}

static void 
gc(struct vm* restrict vm) 
{
    // we want to run the garbage collector pretty much all the time when in debug mode
    // so this code gets properly exercised
    #ifndef DEBUG 
    if (vm->heap->size < 100) {
        return;
    }
    #endif 

    #ifdef DEBUG
    printf("GARBAGE COLLECTION START\n");
    printf("Heap size (before): %d\n", vm->heap->size);
    #endif

    // traverse VM constants, stack and globals and mark every object that is reachable
    for (uint32_t i=0; i < vm->stack_pointer; i++) {
        if (vm->stack->values[i].type <= OBJ_INT) { 
            continue;
        }
        vm->stack->values[i].value.ptr->marked = true;
    }
    for (uint32_t i=0; i < vm->nconstants; i++) {
        if (vm->constants->values[i].type <= OBJ_INT) { 
            continue;
        }
        vm->constants->values[i].value.ptr->marked = true;
    }
    for (uint32_t i=0; i < vm->globals->size && vm->globals->values[i].type != OBJ_NULL; i++) {
        if (vm->globals->values[i].type <= OBJ_INT) { 
            continue;
        }
        vm->globals->values[i].value.ptr->marked = true;
    }

    // traverse all objects, free all unmarked objects
    for (int32_t i = vm->heap->size - 1; i >= 0; i--) {
        if (vm->heap->values[i].value.ptr->marked) {
            // unset marked bit for next gc run
            vm->heap->values[i].value.ptr->marked = false;
            continue;
        }

        // free object
        free_object(&vm->heap->values[i]);


        // remove from heap (swap with last value)
        vm->heap[i] = vm->heap[--vm->heap->size];       
    }

    #ifdef DEBUG
    printf("Heap size (after): %d\n", vm->heap->size);
    printf("GARBAGE COLLECTION DONE\n");
    #endif
}

enum result 
vm_run(struct vm* restrict vm) {
    /* 
    The following comment is taken from CPython's source: https://github.com/python/cpython/blob/3.11/Python/ceval.c#L1243

    Computed GOTOs, or
       the-optimization-commonly-but-improperly-known-as-"threaded code"
   using gcc's labels-as-values extension
   (http://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html).

   The traditional bytecode evaluation loop uses a "switch" statement, which
   decent compilers will optimize as a single indirect branch instruction
   combined with a lookup table of jump addresses. However, since the
   indirect jump instruction is shared by all opcodes, the CPU will have a
   hard time making the right prediction for where to jump next (actually,
   it will be always wrong except in the uncommon case of a sequence of
   several identical opcodes).

   "Threaded code" in contrast, uses an explicit jump table and an explicit
   indirect jump instruction at the end of each opcode. Since the jump
   instruction is at a different address for each opcode, the CPU will make a
   separate prediction for each of these instructions, which is equivalent to
   predicting the second opcode of each opcode pair. These predictions have
   a much better chance to turn out valid, especially in small bytecode loops.

   A mispredicted branch on a modern CPU flushes the whole pipeline and
   can cost several CPU cycles (depending on the pipeline depth),
   and potentially many more instructions (depending on the pipeline width).
   A correctly predicted branch, however, is nearly free.

   At the time of this writing, the "threaded code" version is up to 15-20%
   faster than the normal "switch" version, depending on the compiler and the
   CPU architecture.

   We disable the optimization if DYNAMIC_EXECUTION_PROFILE is defined,
   because it would render the measurements invalid.

   NOTE: care must be taken that the compiler doesn't try to "optimize" the
   indirect jumps by sharing them between all opcodes. Such optimizations
   can be disabled on gcc by using the -fno-gcse flag (or possibly
   -fno-crossjumping).
*/
    const void *dispatch_table[] = {
        &&GOTO_OPCODE_CONST,
        &&GOTO_OPCODE_POP,
        &&GOTO_OPCODE_ADD,
        &&GOTO_OPCODE_SUBTRACT,
        &&GOTO_OPCODE_MULTIPLY,
        &&GOTO_OPCODE_DIVIDE,
        &&GOTO_OPCODE_MODULO,
        &&GOTO_OPCODE_TRUE,
        &&GOTO_OPCODE_FALSE,
        &&GOTO_OPCODE_EQUAL,
        &&GOTO_OPCODE_NOT_EQUAL,
        &&GOTO_OPCODE_GREATER_THAN,
        &&GOTO_OPCODE_GREATER_THAN_OR_EQUALS,
        &&GOTO_OPCODE_LESS_THAN,
        &&GOTO_OPCODE_LESS_THAN_OR_EQUALS,
        &&GOTO_OPCODE_AND,
        &&GOTO_OPCODE_OR,
        &&GOTO_OPCODE_MINUS,
        &&GOTO_OPCODE_BANG,
        &&GOTO_OPCODE_JUMP,
        &&GOTO_OPCODE_JUMP_NOT_TRUE,
        &&GOTO_OPCODE_NULL,
        &&GOTO_OPCODE_GET_GLOBAL,
        &&GOTO_OPCODE_SET_GLOBAL,
        &&GOTO_OPCODE_CALL,
        &&GOTO_OPCODE_RETURN_VALUE,
        &&GOTO_OPCODE_RETURN,
        &&GOTO_OPCODE_GET_LOCAL,
        &&GOTO_OPCODE_SET_LOCAL,
        &&GOTO_OPCODE_GET_BUILTIN,
        &&GOTO_OPCODE_ARRAY,
        &&GOTO_OPCODE_INDEX_GET,
        &&GOTO_OPCODE_INDEX_SET,
        &&GOTO_OPCODE_HALT,
    };
    struct frame *frame = &vm_current_frame(vm);

    #ifdef DEBUG
    char *instruction_str = instruction_to_str(&frame->fn->instructions);
    printf("Executing VM!\nInstructions: %s\n", instruction_str);
    free(instruction_str);
    #endif 

    // intitial dispatch
    DISPATCH();

    // pushes a constant on the stack
    GOTO_OPCODE_CONST: {
        uint16_t idx = read_uint16((frame->ip + 1));
        frame->ip += 3;
        vm_stack_push(vm, vm->constants->values[idx]); 
        DISPATCH();
    }

    // pop last value off the stack and discard it
    GOTO_OPCODE_POP: {
        vm_stack_pop_ignore(vm);
        frame->ip++;
        DISPATCH();
    }

    // call a (user-defined or built-in) function
    GOTO_OPCODE_CALL: {
        uint8_t num_args = read_uint8((++frame->ip));
        vm_do_call(vm, num_args);
        frame = &vm->frames[vm->frame_index];
        DISPATCH();
    }

    GOTO_OPCODE_JUMP: {
        uint16_t pos = read_uint16((frame->ip + 1));
        frame->ip = frame->fn->instructions.bytes + pos;
        DISPATCH();
    }

    GOTO_OPCODE_JUMP_NOT_TRUE: {
        struct object *condition = &vm_stack_pop(vm);
        if (condition->type == OBJ_NULL || (condition->type == OBJ_BOOL && condition->value.boolean == false)) {
            uint16_t pos = read_uint16((frame->ip + 1));
            frame->ip = frame->fn->instructions.bytes + pos;
        } else {
            frame->ip += 3;
        }
        DISPATCH();
    }

    GOTO_OPCODE_SET_GLOBAL: {
        uint16_t idx = read_uint16((frame->ip + 1));
        frame->ip += 3;
        vm->globals = insert_in_object_list(vm->globals, idx, vm_stack_pop(vm)); 
        DISPATCH();
    }

    GOTO_OPCODE_GET_GLOBAL: {
        uint16_t idx = read_uint16((frame->ip + 1));
        frame->ip += 3;
        vm_stack_push(vm, vm->globals->values[idx]);
        DISPATCH();
    }

    GOTO_OPCODE_RETURN_VALUE: {
        struct object obj = vm_stack_pop(vm); 
        vm->stack_pointer = frame->base_pointer - 1;
        frame = &vm->frames[--vm->frame_index];
        vm_stack_push(vm, obj);
        frame->ip++;
        DISPATCH();
    }

    GOTO_OPCODE_RETURN: {
        vm->stack_pointer = frame->base_pointer - 1;
        frame = &vm->frames[--vm->frame_index];
        vm->stack->values[vm->stack_pointer++].type = OBJ_NULL;
        frame->ip++; 
        DISPATCH();
    }

    GOTO_OPCODE_SET_LOCAL: {
        uint8_t idx = read_uint8((frame->ip + 1));
        frame->ip += 2;
        vm->stack->values[frame->base_pointer + idx] = vm_stack_pop(vm);
        DISPATCH();
    }

    GOTO_OPCODE_GET_LOCAL: {
        uint8_t idx = read_uint8((frame->ip + 1));
        frame->ip += 2;
        vm_stack_push(vm, vm->stack->values[frame->base_pointer + idx]);
        DISPATCH();
    }

    GOTO_OPCODE_AND:
    GOTO_OPCODE_OR:
    GOTO_OPCODE_ADD:
    GOTO_OPCODE_SUBTRACT:
    GOTO_OPCODE_MULTIPLY:
    GOTO_OPCODE_DIVIDE:
    GOTO_OPCODE_MODULO: {
        vm_do_binary_operation(vm, *frame->ip++);
        DISPATCH();
    }

    GOTO_OPCODE_BANG: {
        vm_do_bang_operation(vm);
        frame->ip++;
        DISPATCH();
    }

    GOTO_OPCODE_MINUS: {
        vm_do_minus_operation(vm);
        frame->ip++;
        DISPATCH();
    }

    GOTO_OPCODE_EQUAL:
    GOTO_OPCODE_NOT_EQUAL: 
    GOTO_OPCODE_GREATER_THAN: 
    GOTO_OPCODE_GREATER_THAN_OR_EQUALS:
    GOTO_OPCODE_LESS_THAN: 
    GOTO_OPCODE_LESS_THAN_OR_EQUALS: {
        vm_do_comparision(vm, *frame->ip++);
        DISPATCH();
    }

    GOTO_OPCODE_TRUE: {
        vm->stack->values[vm->stack_pointer].type = OBJ_BOOL;
        vm->stack->values[vm->stack_pointer].value.boolean = true;
        vm->stack_pointer++;
        frame->ip++;
        DISPATCH();
    }

    GOTO_OPCODE_FALSE: {
        vm->stack->values[vm->stack_pointer].type = OBJ_BOOL;
        vm->stack->values[vm->stack_pointer].value.boolean = false;
        vm->stack_pointer++;
        frame->ip++;
        DISPATCH();
    }

    GOTO_OPCODE_NULL: {
        vm->stack->values[vm->stack_pointer++].type = OBJ_NULL;
        frame->ip++;
        DISPATCH();
    }

    GOTO_OPCODE_GET_BUILTIN: {
        uint8_t idx = read_uint8((frame->ip + 1));
        frame->ip += 2;
        vm_stack_push(vm, get_builtin_by_index(idx));
        DISPATCH();
    }

    GOTO_OPCODE_ARRAY: {
        uint16_t num_elements = read_uint16((frame->ip + 1));
        frame->ip += 3;
        struct object array = vm_build_array(vm, vm->stack_pointer - num_elements, vm->stack_pointer);
        vm->stack_pointer -= num_elements;
        vm_stack_push(vm, array);
        DISPATCH();
    }

    GOTO_OPCODE_INDEX_GET: {
        struct object index = vm_stack_pop(vm);
        struct object left = vm_stack_pop(vm);
        assert(index.type == OBJ_INT);

        switch (left.type) {
            case OBJ_ARRAY: {
                struct object_list* list = (struct object_list*) left.value.ptr->value;
                if (index.value.integer < 0 || index.value.integer >= list->size) {
                    vm_stack_push(vm, (struct object) {.type = OBJ_NULL});
                } else {
                    struct object value = list->values[index.value.integer];
                    vm_stack_push(vm, value);
                }
            }
            break;

            case OBJ_STRING: {
                const char *str = ((const char*) left.value.ptr->value);
                if (index.value.integer < 0 || index.value.integer >= strlen(str)) {
                     vm_stack_push(vm, (struct object) {.type = OBJ_NULL});
                } else {
                    char buf[2];
                    buf[0] = (char) str[index.value.integer];
                    buf[1] = '\0';

                    struct object obj = make_string_object(buf, NULL);
                    vm->heap->values[vm->heap->size++] = obj;
                    vm_stack_push(vm, obj);
                }   
            }
            break;

            default:
                return VM_ERR_INVALID_INDEX_SOURCE;
            break;
        }
        
        
        frame->ip++;
        DISPATCH();
    }

    GOTO_OPCODE_INDEX_SET: {
        struct object value = vm_stack_pop(vm);
        struct object index = vm_stack_pop(vm);
        struct object array = vm_stack_pop(vm);
        assert(index.type == OBJ_INT);
        assert(array.type == OBJ_ARRAY);
        struct object_list* list = (struct object_list*) array.value.ptr->value;
        if (index.value.integer < 0 || index.value.integer >= list->size) {
            // TODO: Determine what we want to do when we assign out of bounds
        } else {
            list->values[index.value.integer] = copy_object(&value);

            // Push value on stack ???
            vm_stack_push(vm, value);
        }
        
        frame->ip++;
        DISPATCH();
    }

    GOTO_OPCODE_HALT: ;

    return VM_SUCCESS;
}

inline  
struct object vm_stack_last_popped(struct vm *restrict vm) {
    return vm->stack->values[vm->stack_pointer];
}

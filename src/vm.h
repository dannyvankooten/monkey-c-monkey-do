#pragma once

#include <bits/stdint-uintn.h>
#include <stdint.h>
#include "opcode.h"
#include "object.h"

#define FRAMES_SIZE 256

enum result {
    VM_SUCCESS = 0,
    VM_ERR_INVALID_OP_TYPE,
    VM_ERR_INVALID_INT_OPERATOR,
    VM_ERR_INVALID_BOOL_OPERATOR,
    VM_ERR_OUT_OF_BOUNDS,
    VM_ERR_STACK_OVERFLOW,
    VM_ERR_INVALID_FUNCTION_CALL,
    VM_ERR_INVALID_INDEX_SOURCE,
};

struct frame {
    uint8_t *ip;
    struct compiled_function* fn;
    uint_fast32_t base_pointer;    
};

struct vm {
    uint_fast32_t stack_pointer;
    uint_fast32_t frame_index;
    uint_fast32_t nconstants;
    
    struct frame frames[FRAMES_SIZE];
    struct object_list *constants;
    struct object_list *globals;
    struct object_list *stack;
    struct object_list *heap;
};

struct vm *vm_new(struct bytecode *bc);
struct vm *vm_new_with_globals(struct bytecode *bc, struct object_list *globals);
enum result vm_run(struct vm *vm);
struct object vm_stack_last_popped(struct vm *vm);
void vm_free(struct vm *vm);
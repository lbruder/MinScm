/* vim:et
*
* structures.h
* Data structures used by the runtime
*
* This file is part of MinScheme, an experimental compiler/interpreter/runtime
* combination for a subset of the Scheme programming language
* Copyright (c) 2013, Leif Bruder <leifbruder@gmail.com>
*
* Permission to use, copy, modify, and/or distribute this software for any
* purpose with or without fee is hereby granted, provided that the above
* copyright notice and this permission notice appear in all copies.
* 
* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

#ifndef __STRUCTURES_H__
#define __STRUCTURES_H__

#include <inttypes.h>

enum ObjectType {
        T_FIXNUM           =  0,
        T_FLONUM           =  1,
        T_SYMBOL           =  2,
        T_PAIR             =  3,
        T_STRING           =  4,
        T_TRUE             =  5,
        T_FALSE            =  6,
        T_CHAR             =  7,
        T_NULL             =  8,
        T_BUILTIN_FUNCTION =  9,
        T_CLOSURE          = 10,
        T_VECTOR           = 11,
        T_EOF              = 12,
        T_ENVIRONMENT      = 13,
        T_ENVIRONMENT_NODE = 14,
        T_TAGGED_VALUE     = 15
};

/* A position is simply an index for the heap array. By using an uint32_t, we
 * avoid any problems attacking us if sizeof(void*) != sizeof(uint32_t). Of
 * course, we are limited to a max heap size of 4GB. */
typedef uint32_t position_t;

/* General object header. The first byte is encoded as LK00TTTT, with
 * L = Live flag during GC MARK phase
 * K = Keepalive flag to prevent an object from being collected
 * T = ObjectType value
 * The target_position value is used during the GC COMPACT phase to store
 * the new position an object will be moved to */
struct Object {
        uint8_t type_and_gc_flags;
        position_t gc_target_position;
};

struct Fixnum {
        struct Object obj_data;
        int32_t value;
};

struct Flonum {
        struct Object obj_data;
        double value;
};

struct Symbol {
        struct Object obj_data;
        position_t left_tree;
        position_t right_tree;
        uint32_t name_length;
        /* uint8_t[] name; */
};

struct Pair {
        struct Object obj_data;
        uint32_t car;
        uint32_t cdr;
};

struct String {
        struct Object obj_data;
        uint32_t value_length;
        /* uint8_t[] value; */
};

struct True {
        struct Object obj_data;
};

struct False {
        struct Object obj_data;
};

struct Char {
        struct Object obj_data;
        uint8_t value;
};

struct Null {
        struct Object obj_data;
};

struct BuiltinFunction {
        struct Object obj_data;
        uint8_t opcode;
};

struct Closure {
        struct Object obj_data;
        uint8_t number_of_parameters;
        uint8_t has_rest_parameter;
        position_t symbol;
        position_t captured_environment;
        position_t body;
};

struct Vector {
        struct Object obj_data;
        uint32_t length;
        /* position_t[] values; */
};

struct Eof {
        struct Object obj_data;
};

struct Environment {
        struct Object obj_data;
        position_t outer;
        position_t root_node;
};

struct Environment_Node {
        struct Object obj_data;
        position_t symbol;
        position_t value;
        position_t left_tree;
        position_t right_tree;
};

struct Tagged_Value {
        struct Object obj_data;
        position_t symbol;
        position_t value;
};

#endif


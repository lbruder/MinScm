/* vim:et
*
* memory.h
* Memory management functions
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

#ifndef __MEMORY_H__
#define __MEMORY_H__

#include "structures.h"

void init_memory(uint32_t initial_heap_size);
void gc();
enum ObjectType get_object_type(position_t object);

position_t new_fixnum(uint32_t value);
uint32_t get_fixnum_value(position_t fixnum);

position_t new_flonum(double value);
double get_flonum_value(position_t flonum);

position_t get_symbol_from_string(uint32_t name_length, uint8_t name[]);
position_t get_string_from_symbol(position_t symbol);

position_t new_pair(position_t car, position_t cdr);
position_t get_car(position_t pair);
position_t get_cdr(position_t pair);
void set_car(position_t pair, position_t new_car);
void set_cdr(position_t pair, position_t new_cdr);

position_t new_string(uint32_t value_length);
uint32_t get_string_length(position_t string);
uint8_t get_string_char(position_t string, uint32_t index);
void set_string_char(position_t string, uint32_t index, uint8_t new_char);

position_t get_true();

position_t get_false();

position_t new_char(uint8_t value);

position_t get_null();

position_t get_builtin_function(uint8_t opcode);
uint8_t get_builtin_function_opcode(position_t builtin_function);

position_t new_closure(uint8_t number_of_parameters,
                       uint8_t has_rest_parameter,
                       position_t symbol,
                       position_t captured_environment,
                       position_t body);
uint8_t get_closure_number_of_parameters(position_t closure);                       
uint8_t get_closure_has_rest_parameter(position_t closure);
position_t get_closure_symbol(position_t closure);
position_t get_closure_environment(position_t closure);
position_t get_closure_body(position_t closure);

position_t new_vector(uint32_t length);
uint32_t get_vector_length(position_t vector);
position_t get_vector_value(position_t vector, uint32_t index);
void set_vector_value(position_t vector, uint32_t index, position_t new_value);

position_t get_eof();

position_t new_environment(position_t outer);
void environment_define(position_t env, position_t symbol, position_t value);
void environment_set(position_t env, position_t symbol, position_t value);
position_t environment_get(position_t env, position_t symbol);

position_t new_tagged_value(position_t value);
position_t get_tagged_value(position_t tagged_value);

#endif


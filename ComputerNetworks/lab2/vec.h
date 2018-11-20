#pragma once

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#define VEC_MINIMAL_SIZE 8

typedef struct {
    uint32_t length;
    uint32_t allocated;
    void ** array;
} vec_t;

#define VEC_FOREACH(V, IDX, E) \
    for( (IDX) = 0, (E) = (V) -> array[ 0 ]; (IDX) < (V) -> length; (E) = (V) -> array[ ++ (IDX) ] )

#define VEC_CREATE(NAME) \
    vec_t * NAME = malloc( sizeof( vec_t ) )

int vec_init( vec_t * vec );
void vec_destroy( vec_t * vec );
void vec_push( vec_t * vec, void * item );
void * vec_pop( vec_t * vec );
void vec_remove( vec_t * vec, uint32_t index, bool should_free );

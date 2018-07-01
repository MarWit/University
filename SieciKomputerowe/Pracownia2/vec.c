#include "vec.h"

int
vec_init( vec_t * vec ) {
    vec -> array        = calloc( VEC_MINIMAL_SIZE, sizeof( void * ) );

    if( vec -> array == NULL ) {
        return -1;
    }

    vec -> length       = 0;
    vec -> allocated    = VEC_MINIMAL_SIZE;

    return 0;
}

void
vec_destroy( vec_t * vec ) {
    free( vec -> array );

    vec -> length       = 0;
    vec -> allocated    = 0;
}

void
vec_reallocate( vec_t * vec, uint32_t new_size ) {
    void *addr = realloc( vec -> array, new_size * sizeof( void * ) );
    vec -> array = addr;
    vec -> allocated = new_size;
}

void
vec_push( vec_t * vec, void * item ) {
    if( vec -> length + 1 == vec -> allocated ) {
        vec_reallocate( vec, vec -> allocated + VEC_MINIMAL_SIZE );
    }

    vec -> array[ vec -> length ++ ] = item;
}

void *
vec_pop( vec_t * vec ) {
    if( vec -> length == 0 ) {
        return NULL;
    }

    return vec -> array[ -- vec -> length ];
}

void
vec_remove( vec_t * vec, uint32_t index, bool should_free ) {
    if( index >= vec -> length || ! vec -> length ) {
        return;
    }

    if( should_free ) {
        free( vec -> array[ index ] );
    }

    for( uint32_t i = index + 1; i < vec -> length; ++ i ) {
        vec -> array[ i - 1 ] = vec -> array[ i ];
    }

    vec -> length --;
}

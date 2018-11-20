#include <stdio.h>
#include <assert.h>
#include <pthread.h>

#include "sem.h"

#define T 50
#define N 20000

volatile unsigned long n = 0;
sem_t mutex;

void *
remote( void __attribute__((unused)) *arg ) {
    for( int i = 0; i < N; ++ i ) {
        sem_wait( & mutex );
        n ++;
        sem_post( & mutex );
    }

    return 0;
}

int
main( void ) {
    int i;
    pthread_t threads[ T ];
    sem_init( & mutex, 1 );

    for( i = 0; i < T; ++i ) {
        pthread_create( & threads[ i ], NULL, remote, NULL );
    }

    for( i = 0; i < T; ++i ) {
        pthread_join( threads[ i ], NULL );
    }

    printf( "n = %lu, expected = %d\n", n, T * N );
    assert( n == T * N );
    return 0;
}

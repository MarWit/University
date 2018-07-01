#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <unistd.h>
#include <wait.h>

#include "barrier.h"

#define N 10
#define H 20
#define K 5

barrier_t * bar;

void
horse( int idx ) {
    int k = K;

    while( k -- ) {
        printf( "Horse #%d is preparing to start\n", idx );
        bar_wait( bar );
        printf( "Horse #%d ended the loop\n", idx );
    }

    printf( "Horse #%d completed %d races\n", idx, K );
}

int
main( void ) {
    bar = bar_init( N );

    int i;
    pid_t pid;

    for( i = 0; i < H; ++ i ) {
        if( (pid = fork()) < 0) {
            fprintf( stderr, "Could not fork (i = %d, line: %d)\n", i, __LINE__ );
        } else if( pid == 0 ) {
            printf( "Create horse #%d\n", i );
            horse( i );
            return 0;
        }
    }

    while( wait( NULL ) ) {
        if( errno == ECHILD ) {
            break;
        }
    }

    bar_destroy( bar );
}

#include <stdio.h>
#include <stdlib.h>

#include <errno.h>
#include <pthread.h>
#include <semaphore.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>
#include <wait.h>

typedef struct {
    sem_t critsec;
    sem_t is_empty;
    sem_t is_full;
    long soup;
} shared_t;

// #define DEBUG
#define N 10
#define M 6

shared_t *shared;

void
savage( int __attribute__((unused)) i ) {
    for( ;; ) {
        sem_wait( & shared -> critsec );

        if( shared -> soup == 0 ) {
            sem_post( & shared -> is_empty );
            sem_wait( & shared -> is_full );
        }

        shared -> soup --;
#ifdef DEBUG
        printf( "Savage #%d ate (bowl: %ld/%d)\n", i, shared -> soup, M );
#endif

        sem_post( & shared -> critsec );

        usleep( rand() % 1234 );
    }
}

void
cook( ) {
    for( ;; ) {
        sem_wait( & shared -> is_empty );
#ifdef DEBUG
        printf( "Cook was called!\n" );
#endif
        shared -> soup = M;
        sem_post( & shared -> is_full );
    }
}

int
main( void ) {
    shared = (shared_t *) mmap( NULL,
                                sizeof( shared_t ),
                                PROT_READ | PROT_WRITE,
                                MAP_SHARED | MAP_ANONYMOUS,
                                0, 0 );

    sem_init( & shared -> critsec, 1, 1 );
    sem_init( & shared -> is_empty, 1, 0 );
    sem_init( & shared -> is_full, 1, 0 );
    shared -> soup = M;

#ifndef DEBUG
    srand( time( NULL ) );
#endif

    pid_t pid;

    for( int i = 0; i < N; ++ i ) {
        if( ( pid = fork() ) == -1 ) {
            // TODO: Fork error
        } else if( pid == 0 ) {
            savage( i );
            return 0;
        }
    }

    if( ( pid = fork() ) == -1 ) {
        // TODO: Error
    } else if( pid == 0 ) {
        cook( );
        return 0;
    }

    int status;
    for( ; waitpid( -1, & status, 0 ); ) {
        if( errno == ECHILD ) break;
    }

    return 0;
}

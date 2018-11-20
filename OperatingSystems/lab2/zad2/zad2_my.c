#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include "../zad1/sem.h"

#define N 6
// #define DEBUG

sem_t forks[ N ];
pthread_t threads[ N ];

void
think( void ) {
    usleep( rand() % 200 );
}

void
eat( void ) {
    usleep( rand() % 200 );
}

void
take_forks( long i ) {
    if( i ) {
        sem_wait( & forks[ i ] );
        sem_wait( & forks[ (i + 1) % N ] );
    } else {
        sem_wait( & forks[ (i + 1) % N ] );
        sem_wait( & forks[ i ] );
    }
}

void
put_forks( long i ) {
    if( i ) {
        sem_post( & forks[ (i + 1) % N ] );
        sem_post( & forks[ i ] );
    } else {
        sem_post( & forks[ i ] );
        sem_post( & forks[ (i + 1) % N ] );
    }
}

void *
philosopher( void *arg ) {
    long i = (long) arg;
    for( ;; ) {
        think( );
        take_forks( i );
        eat( );
        put_forks( i );
    }
}

void
parent_handler( int signum ) {
    if( signum == SIGINT ) {
        for( int i = 0; i < N; i ++ ) {
            pthread_cancel( threads[ i ] );
        }
    }
}

#ifdef DEBUG
void *
printer( void __attribute__((unused)) *arg ) {
    int i, value;
    for( ;; ) {
        for( i = 0; i < N; i ++ ) {
            sem_getvalue( & forks[ i ], &value );
            printf( "%d ", value );
        }

        printf( "\n" );
        usleep( 500 );
    }
}
#endif

int
main( void ) {
    printf( "My semaphores\n" );

#ifdef DEBUG
    pthread_t tprinter;
#endif

    long i = 0;

    for( ; i < N; ++ i ) {
        sem_init( & forks[ i ], 1 );
    }

    for( i = 0 ; i < N; ++ i ) {
        if( pthread_create( & threads[ i ], NULL, philosopher, (long *)i ) ) {
            fprintf( stderr, "Could not create thread\n" );
            return -1;
        }
    }

#ifdef DEBUG
    pthread_create( & tprinter, NULL, printer, NULL );
#endif

    for( i = 0; i < N; ++ i ) {
        if( pthread_join( threads[ i ], NULL ) ) {
            fprintf( stderr, "Could not join thread\n" );
            continue;
        }
    }

#ifdef DEBUG
    pthread_cancel( tprinter );
#endif

    return 0;
}

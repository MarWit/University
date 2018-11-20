#include <fcntl.h>
#include <pthread.h>
#include <semaphore.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/errno.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#define N 6
// #define DEBUG

#ifdef DEBUG
pid_t pprinter;
#endif

pid_t pids[ N ];
sem_t * forks[ N ];

void
think( void ) {
    usleep( rand() % 200 );
}

void
eat( void ) {
    usleep( rand() % 200 );
}

void
take_forks( int i ) {
    if( i ) {
        sem_wait( forks[ i ] );
        sem_wait( forks[ (i + 1) % N ] );
    } else {
        sem_wait( forks[ (i + 1) % N ] );
        sem_wait( forks[ i ] );
    }
}

void
put_forks( int i ) {
    if( i ) {
        sem_post( forks[ (i + 1) % N ] );
        sem_post( forks[ i ] );
    } else {
        sem_post( forks[ i ] );
        sem_post( forks[ (i + 1) % N ] );
    }
}

void
philosopher( int i ) {
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
            kill( pids[ i ], SIGINT );
        }

#ifdef DEBUG
        kill( pprinter, SIGINT );
#endif
    }
}

void
child_handler( int signum ) {
    if( signum == SIGINT ) {
        for( int i = 0; i < N; ++ i ) {
            sem_close( forks[ i ] );
        }

        exit( 0 );
    }
}

#ifdef DEBUG
void
printer( void ) {
    int i;
    int value;
    for( ;; ) {
        for( i = 0; i < N; ++ i ) {
            sem_getvalue( forks[ i ], & value );
            printf( "%d ", value );
        }

        printf( "\n" );
    }
}
#endif

int
main( void ) {
    printf( "Named semaphores\n" );
    int i = 0;

    char buffer[ 32 ];
    for( i = 0; i < N; ++ i ) {
        sprintf( buffer, "/fork%d", i );
        forks[ i ] = sem_open( buffer, O_CREAT | O_EXCL, 0664, 1 );
    }

    for( i = 0; i < N; ++ i ) {
        pids[ i ] = fork();

        if( pids[ i ] < 0 ) {
            fprintf( stderr, "Error: fork returned value < 0 (line: %d)\n", __LINE__ );
        } else if( pids[ i ] == 0 ) {
            struct sigaction sig;
            sig.sa_handler = child_handler;
            sigemptyset( &sig.sa_mask );
            sigaction( SIGINT, & sig, NULL );

            philosopher( i );
            return 0;
        }
    }

#ifdef DEBUG
    if( (pprinter = fork()) < 0 ) {
            fprintf( stderr, "Error: fork returned value < 0 (line: %d)\n", __LINE__ );
    } else if( pprinter == 0 ) {
        struct sigaction sig;
        sig.sa_handler = child_handler;
        sigemptyset( &sig.sa_mask );
        sigaction( SIGINT, & sig, NULL );

        printer( );
        return 0;
    }
#endif

    struct sigaction sig;
    sig.sa_handler = parent_handler;
    sigemptyset( &sig.sa_mask );
    sigaction( SIGINT, & sig, NULL );

    while( wait( NULL ) ) {
        if( errno == ECHILD ) {
            break;
        }
    }

    for( i = 0; i < N; ++ i ) {
        sem_close( forks[ i ] );
        sprintf( buffer, "/fork%d", i );
        sem_unlink( buffer );
    }

    return 0;
}

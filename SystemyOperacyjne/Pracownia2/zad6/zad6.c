#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include "../zad1/sem.h"

#define DEBUG


#ifdef DEBUG
#include <stdio.h>
#define LOG(A) puts(A)
#else
#define LOG(A) {}
#endif

sem_t smoking,
      tobacco,
      paper,
      matches,
      critsec,
      tob_smoker,
      pap_smoker,
      mat_smoker;

bool tobacco_available,
     paper_available,
     matches_available;

void *
agent( void __attribute__((unused)) *arg ) {
    for( ;; ) {
        sem_post( & smoking );

        switch( rand() % 3 ) {
            case 0:
                sem_post( & tobacco );
                sem_post( & paper );
                break;

            case 1:
                sem_post( & tobacco );
                sem_post( & matches );
                break;

            case 2:
                sem_post( & paper );
                sem_post( & matches );
                break;
        }
    }
}

void
make_cigarrete( ) {
    usleep( rand() % 300 );
}

void
smoke( ) {
    usleep( rand( ) % 1500 );
}

void *
tobacco_smoker( void __attribute__((unused)) *arg ) {
    for( ;; ) {
        sem_wait( & tob_smoker );
        LOG( "Smoker with tobacco's makin' cigarrete" );
        make_cigarrete( );
        sem_post( & smoking );
        LOG( "Smoker with tobacco's smoking" );
        smoke( );
    }
}

void *
paper_smoker( void __attribute__((unused)) *arg ) {
    for( ;; ) {
        sem_wait( & pap_smoker );
        LOG( "Smoker with paper's makin' cigarrete" );
        make_cigarrete( );
        sem_post( & smoking );
        LOG( "Smoker with paper's smoking" );
        smoke( );
    }
}

void *
matches_smoker( void __attribute__((unused)) *arg ) {
    for( ;; ) {
        sem_wait( & mat_smoker );
        LOG( "Smoker with matches's makin' cigarrete" );
        make_cigarrete( );
        sem_post( & smoking );
        LOG( "Smoker with matches's smoking" );
        smoke( );
    }
}

void *
tobacco_collector( void __attribute__((unused)) *arg ) {
    for( ;; ) {
        sem_wait( & tobacco );
        sem_wait( & critsec );

        if( matches_available ) {
            matches_available = false;
            sem_post( & pap_smoker );
        } else if( paper_available ) {
            paper_available = false;
            sem_post( & mat_smoker );
        } else {
            tobacco_available = true;
        }

        sem_post( & critsec );
    }
}

void *
paper_collector( void __attribute__((unused)) *arg ) {
    for( ;; ) {
        sem_wait( & paper );
        sem_wait( & critsec );

        if( matches_available ) {
            matches_available = false;
            sem_post( & tob_smoker );
        } else if( tobacco_available ) {
            tobacco_available = false;
            sem_post( & mat_smoker );
        } else {
            paper_available = true;
        }

        sem_post( & critsec );
    }
}

void *
matches_collector( void __attribute__((unused)) *arg ) {
    for( ;; ) {
        sem_wait( & matches );
        sem_wait( & critsec );

        if( tobacco_available ) {
            tobacco_available = false;
            sem_post( & pap_smoker );
        } else if( paper_available ) {
            paper_available = false;
            sem_post( & tob_smoker );
        } else {
            matches_available = true;
        }

        sem_post( & critsec );
    }
}

int
main( void ) {
    // Base
    sem_init( & smoking, 1 );
    sem_init( & tobacco, 0 );
    sem_init( & paper, 0 );
    sem_init( & matches, 0 );

    // Solution
    sem_init( & critsec, 1 );
    sem_init( & tob_smoker, 0 );
    sem_init( & pap_smoker, 0 );
    sem_init( & mat_smoker, 0 );

    tobacco_available = false;
    paper_available = false;
    matches_available = false;

    pthread_t tagent;
    pthread_t tsmokers[ 3 ];
    pthread_t tcollectors[ 3 ];

    pthread_create( & tagent, NULL, agent, NULL );
    pthread_create( & tsmokers[ 0 ], NULL, tobacco_smoker, NULL );
    pthread_create( & tsmokers[ 1 ], NULL, paper_smoker, NULL );
    pthread_create( & tsmokers[ 2 ], NULL, matches_smoker, NULL );
    pthread_create( & tcollectors[ 0 ], NULL, tobacco_collector, NULL );
    pthread_create( & tcollectors[ 1 ], NULL, paper_collector, NULL );
    pthread_create( & tcollectors[ 2 ], NULL, matches_collector, NULL );

    int i;

    pthread_join( tagent, NULL );
    for( i = 0; i < 3; ++ i ) pthread_join( tsmokers[ i ], NULL );
    for( i = 0; i < 3; ++ i ) pthread_join( tcollectors[ i ], NULL );

    return 0;
}

#include "barrier.h"
#include <pthread.h>
#include <sys/mman.h>

/*
 * 1) Wpuść  N - 1
 * 2) Wpuść N, zamknij A, otwórz B
 * 3) Wywal N - 1
 * 4) Otwórz A, zamknij B
 *
 *   \               /
 *    \_____________/
 *   A|  N miejsc   |B
 *    |_____________|
 *    /             \
 *   /               \
 */


/*
 * Zaimplementowałem dwie wersje bariery, jednak
 * żadna z nich zdaje się nie działać, mimo że
 * nie byłem w stanie znaleźć do nich kontprzykładu.
 * ( w szczególności zakomentowana wersja zdaje się być
 * dość naturalną implementacją )
 */

barrier_t *
bar_init( int num ) {
    barrier_t * b = mmap( NULL,
                          sizeof( barrier_t ),
                          PROT_READ | PROT_WRITE,
                          MAP_SHARED | MAP_ANONYMOUS,
                          0, 0 );

    b -> num = num;
    b -> counter = 0;

    sem_init( & b -> critsec, 1, 1 );
    sem_init( & b -> entry, 1, num );
    // sem_init( & b -> entry, 1, 1 );
    sem_init( & b -> exit, 1, 0 );

    return b;
}

void
bar_wait( barrier_t * b ) {
    sem_wait( & b -> entry );
    sem_wait( & b -> critsec );

    b -> counter ++;
    if( b -> counter == b -> num ) {
        for( int i = 0; i < b -> num; ++ i ) {
            sem_post( & b -> exit );
        }
    }

    sem_post( & b -> critsec );

    sem_wait( & b -> exit );
    sem_wait( & b -> critsec );

    b -> counter --;
    if( b -> counter == 0 ) {
        for( int i = 0; i < b -> num; ++ i ) {
            sem_post( & b -> entry );
        }
    }

    sem_post( & b -> critsec );
}

//// Ta werja wymaga dodatkowo odkomentowania sem_init dla entry
// void
// bar_wait( barrier_t * b ) {
//     sem_wait( & b -> entry );
//     sem_wait( & b -> critsec );
//
//     b -> counter ++;
//     if( b -> counter == b -> num ) {
//         sem_post( & b -> exit );
//     } else {
//         sem_post( & b -> entry );
//     }
//
//     sem_post( & b -> critsec );
//
//     sem_wait( & b -> exit );
//     sem_wait( & b -> critsec );
//
//     b -> counter --;
//     if( b -> counter == 0 ) {
//         sem_post( & b -> entry );
//     } else {
//         sem_post( & b -> exit );
//     }
//
//     sem_post( & b -> critsec );
// }

void
bar_destroy( barrier_t * b ) {
    sem_destroy( & b -> entry );
    sem_destroy( & b -> exit );
    sem_destroy( & b -> critsec );

    munmap( b, sizeof( barrier_t ) );
}

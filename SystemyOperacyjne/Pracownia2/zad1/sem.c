#include "sem.h"

void sem_init( sem_t *sem, unsigned value ) {
    sem -> value = value;

    pthread_mutexattr_t attrs;
    pthread_mutexattr_init( & attrs );
    pthread_mutexattr_settype( & attrs, PTHREAD_MUTEX_ERRORCHECK );

    pthread_mutex_init( & sem -> critsec, & attrs );
    pthread_cond_init( & sem -> cond, NULL );
}

void
sem_wait( sem_t *sem ) {
    pthread_mutex_lock( & sem -> critsec );

    while( sem -> value == 0 ) {
        pthread_cond_wait( & sem -> cond, & sem -> critsec );
    }

    sem -> value --;
    pthread_mutex_unlock( & sem -> critsec );
}

void
sem_post( sem_t *sem ) {
    pthread_mutex_lock( & sem -> critsec );
    sem -> value ++;
    pthread_cond_broadcast( & sem -> cond );
    pthread_mutex_unlock( & sem -> critsec );
}

void
sem_getvalue( sem_t *sem, int *sval ) {
    *sval = sem -> value;
}

#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#define DEBUG

#define N 100000
#define WRITERS 5
#define READERS 50

#ifdef DEBUG
extern int printf (const char *__restrict __format, ...);
#endif

typedef struct _list {
    int value;
    struct _list * next;
    struct _list * prev;
} list_t;

list_t *list = NULL;

pthread_mutex_t append_crit;
pthread_mutex_t search_crit;
pthread_mutex_t remove_crit;
pthread_cond_t no_searchers;
int searchers = 0;

void
list_append( list_t ** l, int num ) {
    if( *l == NULL ) {
        list_t * new = malloc( sizeof( list_t ) );
        new -> prev = new -> next = NULL;
        new -> value = num;
        *l = new;
    } else {
        list_t * query = *l;
        while( query -> next != NULL )
            query = query -> next;

        list_t * new = malloc( sizeof( list_t ) );
        new -> prev = query;
        new -> next = NULL;
        new -> value = num;

        query -> next = new;
    }
}

void
list_remove( list_t ** l, int num ) {
    list_t * query = *l;

    while( query != NULL && query -> value != num )
        query = query -> next;

    if( query != NULL ) {
        if( query -> prev != NULL ) {
            if( query -> next != NULL ) {
                (query -> next) -> prev = query -> prev;
            }

            (query -> prev) -> next = query -> next;
        } else {
            *l = query -> next;

            if( *l != NULL )
                (*l) -> prev = NULL;
        }

        free( query );
    }
}

int
list_pop( list_t ** l ) {
    if( *l != NULL ) {
        int v = (*l) -> value;
        list_t * old = *l;
        *l = old -> next;

        if( *l != NULL )
            (*l) -> prev = NULL;

        free( old );

        return v;
    }

    return -1;
}

bool
search( int num ) {
    pthread_mutex_lock( & search_crit );
    searchers ++;
    pthread_mutex_unlock( & search_crit );

    list_t * query = list;
    bool ret = false;

    while( query != NULL ) {
        if( query -> value == num ) {
            ret = true;
            break;
        }

        query = query -> next;
    }

    pthread_mutex_lock( & search_crit );
    searchers --;
    if( searchers == 0 ) {
        pthread_cond_broadcast( & no_searchers );
    }
    pthread_mutex_unlock( & search_crit );

    return ret;
}

void
append( int num ) {
    pthread_mutex_lock( & append_crit );
    list_append( & list, num );
    pthread_mutex_unlock( & append_crit );
}

void
remove( int num ) {
    pthread_mutex_lock( & remove_crit );

    while( searchers > 0 ) {
        pthread_cond_wait( & no_searchers, & remove_crit );
    }

    pthread_mutex_lock( & search_crit );
    pthread_mutex_lock( & append_crit );

    list_remove( & list, num );

    pthread_mutex_unlock( & search_crit );
    pthread_mutex_unlock( & append_crit );
    pthread_mutex_unlock( & remove_crit );
}

void *
reader( void __attribute__((unused)) *arg ) {
    for( ;; ) {
        usleep( rand() % 500 );
#ifndef DEBUG
        search( rand() % N );
#else
        int x = rand() % N;
        printf( "Searched for: %d = %d\n", x, search( x ) );
#endif
    }
}

void *
writer( void __attribute__((unused)) *arg ) {
    int x;

    list_t * local = NULL;

    for( ;; ) {
        usleep( rand() % 1000 );
        if( (rand() % 3) == 0 ) {
            x = rand() % N;
            list_append( & local, x );
            append( x );
#ifdef DEBUG
            printf( "Appended %d\n", x );
#endif
        } else {
            x = list_pop( & local );
            remove( x );
#ifdef DEBUG
            printf( "Removed %d\n", x );
#endif
        }
    }
}

int
main( void ) {
    pthread_mutex_init( & append_crit, NULL );
    pthread_mutex_init( & search_crit, NULL );
    pthread_mutex_init( & remove_crit, NULL );
    pthread_cond_init( & no_searchers, NULL );

    searchers = 0;

    pthread_t writers[ WRITERS ];
    pthread_t readers[ READERS ];

#ifndef DEBUG
    srand( time( NULL ) );
#endif

    int i;

    for( i = 0; i < WRITERS; ++ i ) {
        pthread_create( & writers[ i ], NULL, writer, NULL );
    }

    for( i = 0; i < READERS; ++ i ) {
        pthread_create( & readers[ i ], NULL, reader, NULL );
    }

    for( i = 0; i < WRITERS; ++ i ) {
        pthread_join( writers[ i ], NULL );
    }

    for( i = 0; i < READERS; ++ i ) {
        pthread_join( readers[ i ], NULL );
    }

    return 0;
}

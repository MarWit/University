#pragma once

#include <semaphore.h>

typedef struct {
    sem_t critsec;
    sem_t entry;
    sem_t exit;
    int num;
    int counter;
} barrier_t;

barrier_t * bar_init( int num );
void bar_wait( barrier_t * b );
void bar_destroy( barrier_t * b );

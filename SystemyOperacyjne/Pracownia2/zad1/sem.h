#pragma once

#include <pthread.h>

typedef struct {
    int value;
    pthread_cond_t cond;
    pthread_mutex_t critsec;
} sem_t;


void sem_init(sem_t *sem, unsigned value);
void sem_wait(sem_t *sem);
void sem_post(sem_t *sem);
void sem_getvalue(sem_t *sem, int *sval);

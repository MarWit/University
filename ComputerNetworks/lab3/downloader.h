#pragma once

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>

#define PACKET_SIZE 1000
#define PACKET_LIFETIME 100000
#define WINDOW_SIZE 1000
#define BUFFER_SIZE (PACKET_SIZE * WINDOW_SIZE)
#define SERVER_ADDRESS "156.17.4.30"

#define MIN(A,B) ((A) > (B) ? (B) : (A))

#define THROW_CUSTOM_ERROR(FUNC,DESC) \
    fprintf( stderr, "%s: %s (file %s, line: %d)\n", (FUNC), (DESC), __FILE__, __LINE__ )

#define THROW_ERROR(FUNC) THROW_CUSTOM_ERROR(FUNC, strerror( errno ))

typedef struct {
    long lifetime;
    bool valid;
} packet_desc_t;

typedef struct {
    packet_desc_t packet_info[ WINDOW_SIZE ];
    char data[ BUFFER_SIZE ];
    int start;
    int end;
} window_t;

int download( FILE *file, long port, long bytes );

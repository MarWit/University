#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "downloader.h"

int
read_long( char *input, long *output ) {
    errno = 0;
    char *end;

    *output = strtol( input, & end, 10 );

    return errno != 0 || *end != '\0';
}

int
main( int argc, char **argv ) {
    if( argc != 4 ) {
        printf( "Usage: %s <port> <out> <bytes>\n", argv[ 0 ] );
        return 0;
    }

    long port;
    if( read_long( argv[ 1 ], & port ) != 0 ) {
        THROW_CUSTOM_ERROR( "read_long", "invalid port" );
        return 1;
    }

    long bytes;
    if( read_long( argv[ 3 ], & bytes ) != 0 ) {
        THROW_CUSTOM_ERROR( "read_long", "invalid length" );
        return 1;
    }

    FILE * file = fopen( argv[ 2 ], "w" );
    if( file == NULL ) {
        THROW_ERROR( "fopen" );
        return 1;
    }

    int ret = download( file, port, bytes );
    fclose( file );

    return ret;
}

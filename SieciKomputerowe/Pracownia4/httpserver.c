#define _DEFAULT_SOURCE

#include <errno.h>
#include <netinet/in.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

#include "worker.h"

#define THROW_CUSTOM_ERROR(FUNC,DESC) \
    fprintf( stderr, "%s: %s (file %s, line: %d)\n", (FUNC), (DESC), __FILE__, __LINE__ )

#define THROW_ERROR(FUNC) THROW_CUSTOM_ERROR(FUNC, strerror( errno ))

bool
read_short( char * input, short * ret ) {
    errno = 0;
    *ret = strtoul( input, NULL, 0 );

    return errno != 0;
}

int
main( int argc, char **argv ) {
    if( argc != 3 ) {
        printf( "Usage: %s <port> <root>\n", argv[ 0 ] );
        printf( "\t<port> - http server's port\n" );
        printf( "\t<root> - root directory containing websites to serve\n" );

        return 0;
    }

    short port;
    if( read_short( argv[ 1 ], & port ) ) {
        THROW_CUSTOM_ERROR( "read_short", "given port is invalid\n" );
        return 1;
    }

    char * rootpath = realpath( argv[ 2 ], NULL );
    if( rootpath == NULL ) {
        THROW_CUSTOM_ERROR( "realpath", "given path is invalid\n" );
        return 1;
    }

    int sockfd = socket( AF_INET, SOCK_STREAM, 0 );
    if( sockfd < 0 ) {
        THROW_ERROR( "socket" );
        return 1;
    }

    struct sockaddr_in server;
    memset( &server, 0, sizeof( server ) );

    server.sin_family = AF_INET;
    server.sin_port = htons( port );
    server.sin_addr.s_addr = INADDR_ANY;

    if( bind( sockfd, (struct sockaddr *) & server, sizeof( server ) ) < 0 ) {
        THROW_ERROR( "bind" );
        return 1;
    }

    if( listen( sockfd, 0 ) < 0 ) {
        THROW_ERROR( "listen" );
        return 1;
    }

    serve( sockfd, port, rootpath );

    return 0;
}

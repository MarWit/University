#define _DEFAULT_SOURCE

#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/sendfile.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include "worker.h"
#include "errors.h"

char *
trim_whitespaces( char * input ) {
    int len = strlen( input );

    while( len -- ) {
        if( *input < 33 || *input > 126 ) {
            input ++;
        } else {
            break;
        }
    }

    do {
        if( input[ len ] < 33 || input[ len ] > 126 ) {
            input[ len ] = '\0';
        } else {
            break;
        }
    } while( len -- );

    return input;
}

#include <errno.h>

enum HTTPError
parse_request(
    int sockfd,
    char *rootpath,
    char **request,
    char **site,
    char **requested_file,
    bool *autoclose
) {
    char buffer[ 8192 ];

    int len = recv( sockfd, buffer, 8191, 0 );
    if( len <= 0 ) {
        return -1;
    }

    char *saveptr;
    char * req = strtok_r( buffer, "\r\n", & saveptr );

    char * method = strtok( req, " " );
    char * object = strtok( NULL, " " );

    if( strcmp( "GET", method ) != 0 ) {
        return NotImplemented;
    }

    char *query, *host, *connection;
    while( (query = strtok_r( NULL, "\r\n", & saveptr )) != NULL ) {
        if( strstr( query, "Host" ) != NULL ) {
            host = query;
        } else if( strstr( query, "Connection" ) != NULL ) {
            connection = query;
        }
    }

    if( host == NULL ) {
        return NotFound;
    }

    // Don't look
    strtok( host, ":" );
    host = strtok( NULL, ":" );
    host = trim_whitespaces( host );

    printf( "GET %s ", object );

    *autoclose = false;
    if( connection != NULL ) {
        if( strstr( connection, "close" ) != NULL ) {
            *autoclose = true;
        }
    }

    char pathbuf[ 512 ];
    sprintf( pathbuf, "%s/%s", rootpath, host );
    char * sitedirectory = realpath( pathbuf, NULL );

    if( sitedirectory == NULL ) {
        return NotFound;
    }

    if( strstr( sitedirectory, rootpath ) == NULL ) {
        return Forbidden;
    }

    char * filepath = strtok( object, "?" );
    sprintf( pathbuf, "%s/%s", sitedirectory, filepath );

    *requested_file = realpath( pathbuf, NULL );
    if( *requested_file == NULL ) {
        return NotFound;
    }

    if( strstr( *requested_file, sitedirectory ) == NULL ) {
        return Forbidden;
    }

    *site = strdup( host );
    *request = strdup( filepath );

    return OK;
}

void
handle_error( int sockfd, enum HTTPError err, bool closed ) {
    char buffer[ 512 ];
    char * error = full_http_error( err );

    int size = sprintf(
        buffer,
        "HTTP/1.1 %s\r\n"
        "Connection: %s\r\n"
        "Content-Type: text/html\r\n"
        "Content-Length: %lu\r\n"
        "\r\n"
        "<h1>%s</h1>",
        error,
        closed ? "close" : "keep-alive",
        strlen( error ) + 9,
        error
    );

    send( sockfd, buffer, size, 0 );
}

void
handle_redirect( int sockfd, char * req, short port, char * host, bool closed ) {
    char buffer[ 512 ];
    char * error = full_http_error( MovedPermanently );

    int size = sprintf(
        buffer,
        "HTTP/1.1 %s\r\n"
        "Connection: %s\r\n"
        "Location: http://%s:%d%s%s\r\n"
        "\r\n",
        error,
        closed ? "close" : "keep-alive",
        host, port, req,
        req[ strlen( req ) - 1 ] == '/' ? "index.html" : "/index.html"
    );

    send( sockfd, buffer, size, 0 );
}

void
handle_static( int sockfd, char *filepath, int fsize, bool closed ) {
    char buffer[ 512 ];
    char * error = full_http_error( OK );
    char * mime = get_mime_from_path( filepath );

    int size = sprintf(
        buffer,
        "HTTP/1.1 %s\r\n"
        "Connection: %s\r\n"
        "Content-Type: %s\r\n"
        "Content-Length: %d\r\n"
        "\r\n",
        error,
        closed ? "close" : "keep-alive",
        mime,
        fsize
    );

    send( sockfd, buffer, size, 0 );

    int fd = open( filepath, O_RDONLY );
    sendfile( sockfd, fd, NULL, fsize );
    close( fd );
}

void
handle_request( int sockfd, short port, char *rootpath ) {
    char *request, *requested_file, *host;
    bool autoclose = false;

    fd_set descriptors;
    struct timeval timeout;

    while( !autoclose ) {
        enum HTTPError e = parse_request( sockfd, rootpath, & request, & host, & requested_file, & autoclose );
        if( e == -1 ) {
            break;
        }

        if( e != OK ) {
            handle_error( sockfd, e, autoclose );
            printf( "%s\n", full_http_error( e ) );
        } else {
            struct stat statbuf;
            stat( requested_file, & statbuf );

            if( S_ISDIR( statbuf.st_mode ) ) {
                handle_redirect( sockfd, request, port, host, autoclose );
                printf( "%s\n", full_http_error( MovedPermanently ) );
            } else {
                handle_static( sockfd, requested_file, statbuf.st_size, autoclose );
                printf( "%s\n", full_http_error( OK ) );
            }

            free( request );
            free( host );
            free( requested_file );

            if( autoclose ) {
                break;
            }
        }

        FD_ZERO( & descriptors );
        FD_SET( sockfd, & descriptors );
        timeout.tv_sec = 1; timeout.tv_usec = 0;

        int ret = select( sockfd + 1, & descriptors, NULL, NULL, & timeout );
        if( ret <= 0 ) {
            break;
        }
    }

    close( sockfd );
}

void
serve( int sockfd, short port, char *rootpath ) {
    for( ;; ) {
        int clientfd = accept( sockfd, NULL, NULL );

        // printf( "Accepted!\n" );

        handle_request( clientfd, port, rootpath );
        // printf( "Handled!\n" );
    }
}

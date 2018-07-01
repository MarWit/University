#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <netinet/ip.h>
#include <errno.h>
#include <string.h>

#include "consts.h"
#include "receiver.h"
#include "sender.h"

#define THROW_ERROR(NAME) \
    fprintf( stderr, NAME " error: %s (file: %s, line: %d)\n", strerror( errno ), __FILE__, __LINE__ )

int
main( int argc, char **argv ) {
    if( argc != 2 ) {
        printf( "Usage: %s <ip-address>\n", argv[ 0 ] );
        return EXIT_SUCCESS;
    }

    int sockfd = socket( AF_INET, SOCK_RAW, IPPROTO_ICMP );

    if( sockfd < 0 ) {
        THROW_ERROR( "socket" );
        return EXIT_FAILURE;
    }

    struct sockaddr_in recipient;
    bzero( &recipient, sizeof( recipient ) );
    recipient.sin_family = AF_INET;

    if( inet_pton( AF_INET, argv[ 1 ], & recipient.sin_addr ) != 1 ) {
        THROW_ERROR( "inet_pton" );
        return EXIT_FAILURE;
    }

    icmp_reply packets[ PACKETS_NUM ];

    for( int i = 1; i <= MAX_TTL; ++i ) {
        if( send_ping( sockfd, &recipient, i ) < 0 ) {
            THROW_ERROR( "send_ping" );
            return EXIT_FAILURE;
        }

        int num = receive_packets( sockfd, i, packets );
        if( num < 0 ) {
            THROW_ERROR( "receive_packets" );
            return EXIT_FAILURE;
        }

        if( ! num ) {
            printf( "%2d. *\n", i );
            continue;
        }

        long avg = 0;
        printf( "%2d. ", i );

        for( int j = 0; j < num; ++ j ) {
            avg += packets[ j ].delay;
            if( ! packets[ j ].dup  ) {
                printf( "%s ", packets[ j ].ip_addr );
            }
        }

        if( num == PACKETS_NUM )
            printf( "%ldms\n", avg / PACKETS_NUM );
        else
            printf( "???\n" );

        if( ! strcmp( packets[ 0 ].ip_addr, argv[ 1 ] ) )
            break;
    }

    return EXIT_SUCCESS;
}

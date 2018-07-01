#include <arpa/inet.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <sys/socket.h>

#include "packets.h"
#include "table.h"
#include "vec.h"

#define THROW_ERROR(FUNC, DESC) \
    fprintf( stderr, "%s: %s (file: %s, line: %d)\n", FUNC, DESC, __FILE__, __LINE__ )

table_t
parse_input( uint32_t * num ) {
    scanf( "%d", num );

    table_t table = table_create();

    char cidr[ 20 ];
    char *ip_address, *mask;
    uint8_t m;
    uint32_t dist;

    for( uint32_t i = 0; i < *num; ++ i ) {
        scanf( "%s distance %u", cidr, &dist);

        ip_address = strtok( cidr, "/" );
        mask = strtok( NULL, "/" );
        m = (uint8_t) strtol( mask, NULL, 10 );

        table_add_entry( table, ip_address, m, dist, NULL );
    }

    return table;
}

void
print_table( table_t table ) {
    table_entry_t *entry; uint32_t i;
    VEC_FOREACH( table, i, entry ) {
        printf( "%s/%d ", entry -> subnet, entry -> mask );

        if( entry -> bdistance == INF )
            printf( "unreachable " );
        else
            printf( "distance %d ", entry -> distance );

        if( IS_DIRECT( entry ) )
            printf( "connected directly\n" );
        else
            printf( "via %s\n", entry -> via );
    }

    printf( "\n" );
}

int
receive_data( int sockfd, table_t table ) {
    char ip_address[ 15 ];
    char subnet[ 15 ];
    uint8_t mask;
    uint32_t distance;

    int retval;

    for( ;; ) {
        retval = receive_packet(
            sockfd,
            ip_address,
            subnet,
            & mask,
            & distance
        );

        if( retval == RECEIVED_ALL )
            return 0;
        else if( retval == RECEIVED_JUNK )
            continue;

        table_add_or_update( table, ip_address, subnet, mask, distance );
    }
}

void
broadcast_table( int sockfd, table_t table ) {
    table_entry_t *recipient; uint32_t i;
    table_entry_t *entry; uint32_t j;

    int retval;

    VEC_FOREACH( table, i, recipient ) {
        if( ! IS_DIRECT( recipient ) ) break;

        VEC_FOREACH( table, j, entry ) {
            // if( i == j ) continue;
            if( entry -> decaying < -DECAY_RESET ) continue;

            retval = send_packet(
                sockfd,
                recipient -> broadcast,
                entry -> subnet,
                entry -> mask,
                entry -> bdistance
            );

            if( retval < 0 ) {
                recipient -> bdistance = INF;
                break;
            }
        }
    }
}

int
main( void ) {
    uint32_t num;
    table_t table = parse_input( &num );

    int sockfd = socket( AF_INET, SOCK_DGRAM, 0 );
    if( sockfd < 0 ) {
        THROW_ERROR( "socket", strerror( errno ) );
        return EXIT_FAILURE;
    }

    int broadcast = 1;
    int retval = setsockopt( sockfd, SOL_SOCKET, SO_BROADCAST, & broadcast, sizeof( int ) );

    if( retval < 0 ) {
        THROW_ERROR( "setsockopt", strerror( errno ) );
        return EXIT_FAILURE;
    }

    struct sockaddr_in server;
    memset( & server, 0, sizeof( server ) );
    server.sin_family = AF_INET;
    server.sin_port = htons( 54321 );
    server.sin_addr.s_addr = htonl( INADDR_ANY );

    retval = bind( sockfd, (struct sockaddr*) &server, sizeof( server ) );
    if( retval < 0 ) {
        THROW_ERROR( "bind", strerror( errno ) );
        return EXIT_FAILURE;
    }

    fd_set descriptors;
    struct timeval tv;

    for( ;; ) {
        print_table( table );
        broadcast_table( sockfd, table );

        tv.tv_sec = 15; tv.tv_usec = 0;     // TODO(marwit): Change this to some global constant
        while( tv.tv_sec > 0 || tv.tv_usec > 0 ) {
            FD_ZERO( & descriptors );
            FD_SET( sockfd, & descriptors );

            retval = select( sockfd + 1, & descriptors, NULL, NULL, & tv );

            if( retval < 0 ) {
                THROW_ERROR( "select", strerror( errno ) );
                return EXIT_FAILURE;
            }

            if( retval > 0 ) {
                if( receive_data( sockfd, table ) < 0 ) {
                    THROW_ERROR( "receive_data", strerror( errno ) );
                    return EXIT_FAILURE;
                }
            }
        }

        table_decay( table );
    }

    return EXIT_SUCCESS;
}

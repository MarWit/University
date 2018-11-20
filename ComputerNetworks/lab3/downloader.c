#include <arpa/inet.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <sys/socket.h>

#include "downloader.h"

int
send_request(
    int sockfd,
    struct sockaddr_in *recipient,
    long length,
    long offset
) {
    static char req_string[ 20 ];

    int len = snprintf( req_string, 20, "GET %ld %ld\n", offset, length );
    int ret = sendto(
            sockfd,
            req_string, len,
            0, (struct sockaddr*) recipient, sizeof( struct sockaddr_in )
    );

    if( ret < 0 ) {
        THROW_ERROR( "sendto" );
        return -1;
    }

    return 0;
}

void
update_timeouts( window_t *window, long timeout ) {
    if( timeout <= 0 ) return;

    for( int i = 0; i < WINDOW_SIZE; ++ i ) {
        int index = (i + window -> start) % WINDOW_SIZE;

        if( ! window -> packet_info[ index ].valid &&
            window -> packet_info[ index ].lifetime > 0 ) {
            window -> packet_info[ index ].lifetime -= timeout;
        }
    }
}

int
request_remaining(
    int sockfd,
    struct sockaddr_in *recipient,
    long window_offset,
    window_t *window,
    long data_length
) {
    for( int i = 0; i < WINDOW_SIZE; ++ i ) {
        int index = (i + window -> start) % WINDOW_SIZE;

        if( (i + window_offset) * PACKET_SIZE > data_length )
            break;

        if( window -> packet_info[ index ].valid ||
            window -> packet_info[ index ].lifetime > 0 ) {
            continue;
        }

        window -> packet_info[ index ].lifetime = PACKET_LIFETIME;

        long size = ((window_offset + i + 1) * PACKET_SIZE) > data_length ?
            (data_length % PACKET_SIZE) : PACKET_SIZE;

        long offset = (size < PACKET_SIZE) ?
            (data_length - size) : (window_offset + i) * PACKET_SIZE;

        if( send_request( sockfd, recipient, size, offset ) < 0 ) {
            return -1;
        }
    }

    return 0;
}

int
receive_data(
    int sockfd,
    window_t *window,
    long window_offset,
    struct sockaddr_in *recipient
) {
    static char packet[ PACKET_SIZE + 20 ];
    struct sockaddr_in from;

    for( ;; ) {
        socklen_t from_len = sizeof( from );
        int ret = recvfrom(
            sockfd,
            packet, sizeof( packet ),
            MSG_DONTWAIT,
            (struct sockaddr*) & from, & from_len
        );

        if( ret < 0 && errno == EWOULDBLOCK ) {
            break;
        }

        if( ret < 0 ) {
            THROW_ERROR( "recvfrom" );
            return -1;
        }

        if( from.sin_port != recipient -> sin_port ||
            from.sin_addr.s_addr != recipient -> sin_addr.s_addr )
        {
            continue;
        }

        char * header = strtok( packet, "\n" );
        long offset, size;
        sscanf( header, "DATA %ld %ld", & offset, & size );

        if( offset < PACKET_SIZE * window_offset )
            continue;

        offset = (offset - PACKET_SIZE * window_offset) / PACKET_SIZE;
        offset = (offset + window -> start) % WINDOW_SIZE;

        if( window -> packet_info[ offset ].valid )
            continue;

        int header_len = strlen( header ) + 1;
        memcpy( window -> data + PACKET_SIZE * offset, packet + header_len, size );
        window -> packet_info[ offset ].valid = true;
    }

    return 0;
}

int
write_file(
    FILE * file,
    window_t *window,
    long *window_offset,
    long bytes
) {
    int i = 0;

    for( i = 0; i < WINDOW_SIZE; ++ i ) {
        int index = (i + window -> start) % WINDOW_SIZE;

        if( ! i && ! window -> packet_info[ index ].valid )
            return 0;

        if( ! window -> packet_info[ index ].valid ||
            index < window -> start
        ) break;

        window -> packet_info[ index ].valid = false;
        window -> packet_info[ index ].lifetime = 0;
    }

    int bytes_to_write = MIN( i * PACKET_SIZE, bytes - *window_offset * PACKET_SIZE);
    fwrite( window -> data + window -> start * PACKET_SIZE, bytes_to_write, 1, file );

    *window_offset += i;
    window -> start = (window -> start + i) % WINDOW_SIZE;

    printf( "%0.2f%%\n", 100.0 * MIN( 1.0, (float)(*window_offset * PACKET_SIZE) / (float) bytes) );

    return 1;
}


int
download( FILE * file, long port, long bytes ) {
    int sockfd = socket( AF_INET, SOCK_DGRAM, 0 );
    if( sockfd < 0 ) {
        THROW_ERROR( "socket" );
        return 1;
    }

    long window_offset = 0;
    static window_t window;

    struct sockaddr_in recipient;
    inet_pton( AF_INET, SERVER_ADDRESS, & recipient.sin_addr );
    recipient.sin_port = htons( port );
    recipient.sin_family = AF_INET;

    fd_set descriptors;
    struct timeval tv;

    int packets_num = (bytes / PACKET_SIZE) + 1;
    while( window_offset < packets_num ) {
        int ret = request_remaining( sockfd, &recipient, window_offset, & window, bytes );

        if( ret < 0 ) {
            return -1;
        }

        tv.tv_sec = 0; tv.tv_usec = 100000;
        long time_start = tv.tv_usec;

        while( tv.tv_sec > 0 || tv.tv_usec > 0 ) {
            FD_ZERO( & descriptors );
            FD_SET( sockfd, & descriptors );

            int ret = select( sockfd + 1, & descriptors, NULL, NULL, & tv );

            if( ret < 0 ) {
                THROW_ERROR( "select" );
                return -1;
            }

            if( ret > 0 ) {
                receive_data( sockfd, & window, window_offset, & recipient );
            }

            update_timeouts( & window, time_start - tv.tv_usec );

            time_start = tv.tv_usec;
        }

        ret = 1;
        while( ret ) {
            ret = write_file( file, & window, & window_offset, bytes );
        }
    }

    return 0;
}

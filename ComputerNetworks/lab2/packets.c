#include <arpa/inet.h>
#include <errno.h>
#include <string.h>
#include <sys/socket.h>

#include "packets.h"

int
send_packet(
    int sockfd,
    char *ip_addr,
    char *subnet,
    uint8_t mask,
    uint32_t distance )
{
    struct sockaddr_in addr;
    memset( & addr, 0, sizeof( addr ) );

    addr.sin_family = AF_INET;
    addr.sin_port = htons( 54321 );
    inet_pton( AF_INET, ip_addr, & addr.sin_addr );

    packet_t payload;
    inet_pton( AF_INET, subnet, & payload.ip_address );
    payload.mask = mask;
    payload.distance = htonl( distance );

    return sendto( sockfd, & payload, 9, 0, (struct sockaddr *) & addr, sizeof( addr ) );
}

int
receive_packet(
    int sockfd,
    char *ip_addr,
    char *subnet,
    uint8_t *mask,
    uint32_t *distance )
{
    packet_t payload;
    struct sockaddr_in from;
    socklen_t from_len;

    int retval = recvfrom(
            sockfd,
            & payload, 9,
            MSG_DONTWAIT,
            (struct sockaddr *) & from, & from_len
    );

    if( retval < 0 ) {
        return RECEIVED_ALL;
    }

    if( retval != 9 ) {
        return RECEIVED_JUNK;
    }

    inet_ntop( AF_INET, & from.sin_addr, ip_addr, 15 );
    inet_ntop( AF_INET, & payload.ip_address, subnet, 15 );
    *mask = payload.mask;
    *distance = ntohl( payload.distance );

    return RECEIVED_SOME;
}

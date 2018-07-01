#include "sender.h"
#include "consts.h"

u_int16_t
compute_icmp_checksum( const void *buff, int length ) {
    u_int32_t sum;
    const u_int16_t* ptr = buff;

    assert( length % 2 == 0 );

    for( sum = 0; length > 0; length -= 2 )
        sum += *ptr ++;

    sum = (sum >> 16) + (sum & 0xffff);

    return (u_int16_t)(~(sum + (sum >> 16)));
}

int
send_ping( int sockfd, struct sockaddr_in *recipient, int ttl ) {
    struct icmphdr icmp_header;

    icmp_header . type = ICMP_ECHO;
    icmp_header . code = 0;
    icmp_header . un . echo . id = getpid();

    if( setsockopt( sockfd, IPPROTO_IP, IP_TTL, & ttl, sizeof( int ) ) < 0 ) {
        return -1;
    }

    for( int i = 0; i < PACKETS_NUM; ++ i ) {
        icmp_header . un . echo . sequence = PACKETS_NUM * ttl + i;
        icmp_header . checksum = 0;
        icmp_header . checksum = compute_icmp_checksum(
                (u_int16_t * )& icmp_header,
                sizeof( icmp_header )
        );

        ssize_t bytes_send = sendto(
            sockfd,
            & icmp_header,
            sizeof( icmp_header ),
            0,
            (struct sockaddr*) recipient,
            sizeof( struct sockaddr_in )
        );

        if( bytes_send < 0 ) {
            return -1;
        }
    }

    return 0;
}

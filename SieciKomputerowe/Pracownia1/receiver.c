#include "consts.h"
#include "receiver.h"

int
receive_packets( int sockfd, int ttl, icmp_reply *output ) {
    fd_set descriptors;

    struct timeval tv;
    tv.tv_sec = 1; tv.tv_usec = 0;

    uint8_t buffer[ IP_MAXPACKET ];
    struct sockaddr_in sender;
    socklen_t sender_len = sizeof( sender );

    int received = 0;

    while( tv.tv_sec > 0 || tv.tv_usec > 0 ) {
        FD_ZERO( &descriptors );
        FD_SET( sockfd, &descriptors );
        int status = select( sockfd + 1, & descriptors, NULL, NULL, & tv );

        if( status < 0 ) {
            return -1;
        } else if( status > 0 ) {
            for( ;; ) {
                ssize_t len = recvfrom(
                        sockfd,
                        buffer,
                        IP_MAXPACKET,
                        MSG_DONTWAIT,
                        (struct sockaddr*) &sender,
                        & sender_len
                );

                if( len < 0 && errno == EWOULDBLOCK )
                    break;

                struct iphdr *ip_header = (struct iphdr*) buffer;
                uint8_t *icmp_packet = buffer + 4 * ip_header -> ihl;
                struct icmphdr *icmp_header = (struct icmphdr*) icmp_packet;

                if( icmp_header -> type == 11 ) {
                    struct iphdr *ip_header_rep = (struct iphdr*) (buffer + 4 * ip_header -> ihl + sizeof( struct icmphdr ));
                    icmp_packet = icmp_packet + sizeof( struct icmphdr ) + 4 * ip_header_rep -> ihl;
                    icmp_header = (struct icmphdr*) icmp_packet;
                }

                if( icmp_header -> un . echo . id != getpid() )
                    continue;

                if( icmp_header -> un . echo . sequence / PACKETS_NUM != ttl )
                    continue;

                inet_ntop( AF_INET, & sender.sin_addr, output[ received ].ip_addr, 20 );

                output[ received ].delay = 1000 - tv.tv_usec / 1000;
                output[ received ].dup = 0;

                for( int i = 0; i < received; ++ i ) {
                    if( strcmp( output[ received ].ip_addr, output[ i ].ip_addr ) == 0 ) {
                        output[ received ].dup = 1;
                        break;
                    }
                }

                if( ++ received == PACKETS_NUM )
                    return received;
            }
        } else break;
    }

    return received;
}

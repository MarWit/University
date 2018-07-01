#pragma once

#include <arpa/inet.h>
#include <errno.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <unistd.h>

typedef struct {
    char ip_addr[ 20 ];
    long delay;
    int dup;
} icmp_reply;

int receive_packets( int sockfd, int ttl, icmp_reply *output );

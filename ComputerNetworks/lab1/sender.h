#pragma once

#include <netinet/ip_icmp.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <assert.h>

int send_ping( int sockfd, struct sockaddr_in *recipient, int ttl );

#pragma once

#include <stdint.h>

#define RECEIVED_SOME 1
#define RECEIVED_ALL 0
#define RECEIVED_ERROR -1
#define RECEIVED_JUNK -2

#pragma pack(push, 1)
typedef struct {
    uint32_t ip_address;
    uint8_t mask;
    uint32_t distance;
} packet_t;
#pragma pack(pop)


int send_packet( int sockfd, char *ip_addr, char *subnet, uint8_t mask, uint32_t distance );
int receive_packet( int sockfd, char *ip_addr, char *subnet, uint8_t *mask, uint32_t *distance );

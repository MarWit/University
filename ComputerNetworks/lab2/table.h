#pragma once

#include <stdint.h>
#include <stdbool.h>

#include "vec.h"

#define DECAY_RESET 5
#define INF 0xFFFFFFFF

#define CREATE_IPADDRESS(A,B,C,D) \
    ((A << 24) | (B << 16) | (C << 8) | D)
#define IS_DIRECT(ENTRY) ((ENTRY) -> via == NULL)

typedef vec_t* table_t;
typedef struct table_entry_s {
    uint32_t ip_address;
    char subnet[ 16 ];
    uint32_t subnet_numeric;
    char broadcast[ 16 ];
    uint8_t  mask;
    uint32_t distance;
    uint32_t bdistance;
    int32_t decaying;
    char *   via;
} table_entry_t;

table_t table_create( void );
void table_add_entry( table_t table, char * ip_address, uint8_t mask_bits, uint32_t distance, char * via );
void table_add_or_update( table_t table, char * ip_address, char * subnet, uint8_t mask_bits, uint32_t distance );
void table_decay( table_t table );

#include <stdio.h>
#include <string.h>

#include "table.h"

table_t
table_create( void ) {
    VEC_CREATE( table );
    vec_init( table );

    return table;
}

void
ip_to_str( uint32_t ip_address, char * output ) {
    snprintf( output, 16, "%hhu.%hhu.%hhu.%hhu",
        (uint8_t) ((ip_address & 0xFF000000) >> 24),
        (uint8_t) ((ip_address & 0x00FF0000) >> 16),
        (uint8_t) ((ip_address & 0x0000FF00) >>  8),
        (uint8_t) ((ip_address & 0x000000FF))
    );
}

uint32_t
str_to_ip( char * ip_address ) {
    uint8_t a, b, c, d;
    sscanf( ip_address, "%hhu.%hhu.%hhu.%hhu", &a, &b, &c, &d );

    return CREATE_IPADDRESS(a, b, c, d);
}

void
table_add_entry(
    table_t table,
    char * ip_address,
    uint8_t mask_bits,
    uint32_t distance,
    char * via )
{
    table_entry_t * entry = calloc( 1, sizeof( table_entry_t ) );

    uint32_t ip = str_to_ip( ip_address );
    uint32_t mask = (0xFFFFFFFF << (32 - mask_bits)) & 0xFFFFFFFF;
    uint32_t subnet = ip & mask;
    uint32_t broadcast = subnet | (0xFFFFFFFF & ~mask);

    ip_to_str( subnet, entry -> subnet );
    ip_to_str( broadcast, entry -> broadcast );

    entry -> ip_address         = ip;
    entry -> subnet_numeric     = subnet;
    entry -> mask               = mask_bits;
    entry -> decaying           = DECAY_RESET;
    entry -> distance           = distance;
    entry -> bdistance          = distance;
    entry -> via                = via;

    vec_push( table, entry );
}

table_entry_t *
table_find( table_t table, char * ip_address ) {
    bool found = false;
    uint32_t ip  = str_to_ip( ip_address );

    table_entry_t * entry; uint32_t i;
    VEC_FOREACH( table, i, entry ) {
        uint32_t mask = (0xFFFFFFFF << (32 - entry -> mask));
        if( (ip & mask) == entry -> subnet_numeric && ip != entry -> ip_address ) {
            found = true;
            break;
        }
    }

    return found ? entry : NULL;
}

void
table_add_or_update(
    table_t table,
    char * ip_address,
    char * subnet,
    uint8_t mask_bits,
    uint32_t distance )
{
    if( distance == INF ) return;

    table_entry_t * from = table_find( table, ip_address );

    if( from == NULL || ! IS_DIRECT( from ) )
        return;

    from -> bdistance = from -> distance;

    distance += from -> distance;

    table_entry_t * entry; uint32_t i;
    bool found = false;

    uint32_t subnet_numeric = str_to_ip( subnet );

    VEC_FOREACH( table, i, entry ) {
        if( subnet_numeric == entry -> subnet_numeric ) {
            found = true;
            break;
        }
    }

    char * via = calloc( 15, 1 );
    memcpy( via, ip_address, 15 );

    if( found ) {
        if( distance > entry -> distance )
            return;

        if( IS_DIRECT( entry ) )
            return;

        entry -> decaying = DECAY_RESET;
        entry -> distance = distance;
        entry -> bdistance = distance;

        free( entry -> via );
        entry -> via = via;
    } else {
        table_add_entry( table, subnet, mask_bits, distance, via );
    }
}

void
table_decay( table_t table ) {
    bool done = false;
    table_entry_t * entry; uint32_t i;

    // Decay
    VEC_FOREACH( table, i, entry ) {
        if( IS_DIRECT( entry ) ) continue;

        entry -> decaying --;
        if( entry -> decaying <= 0 )
            entry -> bdistance = INF;
    }

    // Remove
    while( ! done ) {
        done = true;

        VEC_FOREACH( table, i, entry ) {
            if( IS_DIRECT( entry ) ) continue;
            if( entry -> decaying < -DECAY_RESET ) {
                done = false;
                break;
            }
        }

        if( ! done ) {
            vec_remove( table, i, true );
        }
    }
}

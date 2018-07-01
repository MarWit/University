#include <stdint.h>

uint32_t
ext2_hash_legacy( const char *string, uint8_t length ) {
    uint32_t hash = 0x12a3fe2d;
    uint32_t previous = 0x37abe8f9;

    for( ; length > 0; -- length, ++ string ) {
        uint32_t next = previous + (hash ^ (*string * 0x6d22f5));

        if( (next & 0x80000000) != 0)
            next -= 0x7fffffff;

        previous = hash;
        hash = next;
    }

    return hash << 1;
}

#define F(x, y, z) ((z) ^ ((x) & ((y) ^ (z))))
#define G(x, y, z) (((x) & (y)) + (((x) ^ (y)) & (z)))
#define H(x, y, z) ((x) ^ (y) ^ (z))

#define ROUND(f, a, b, c, d, x, s)	\
	(a += f(b, c, d) + x, a = (a << s) | (a >> (32 - s)))
#define K1 0
#define K2 013240474631UL
#define K3 015666365641UL

uint32_t
half_md4_transform( uint32_t buf[4], uint32_t const in[8] ) {
    uint32_t a = buf[0], b = buf[1], c = buf[2], d = buf[3];

    /* Round 1 */
    ROUND(F, a, b, c, d, in[0] + K1,  3);
    ROUND(F, d, a, b, c, in[1] + K1,  7);
    ROUND(F, c, d, a, b, in[2] + K1, 11);
    ROUND(F, b, c, d, a, in[3] + K1, 19);
    ROUND(F, a, b, c, d, in[4] + K1,  3);
    ROUND(F, d, a, b, c, in[5] + K1,  7);
    ROUND(F, c, d, a, b, in[6] + K1, 11);
    ROUND(F, b, c, d, a, in[7] + K1, 19);

    /* Round 2 */
    ROUND(G, a, b, c, d, in[1] + K2,  3);
    ROUND(G, d, a, b, c, in[3] + K2,  5);
    ROUND(G, c, d, a, b, in[5] + K2,  9);
    ROUND(G, b, c, d, a, in[7] + K2, 13);
    ROUND(G, a, b, c, d, in[0] + K2,  3);
    ROUND(G, d, a, b, c, in[2] + K2,  5);
    ROUND(G, c, d, a, b, in[4] + K2,  9);
    ROUND(G, b, c, d, a, in[6] + K2, 13);

    /* Round 3 */
    ROUND(H, a, b, c, d, in[3] + K3,  3);
    ROUND(H, d, a, b, c, in[7] + K3,  9);
    ROUND(H, c, d, a, b, in[2] + K3, 11);
    ROUND(H, b, c, d, a, in[6] + K3, 15);
    ROUND(H, a, b, c, d, in[1] + K3,  3);
    ROUND(H, d, a, b, c, in[5] + K3,  9);
    ROUND(H, c, d, a, b, in[0] + K3, 11);
    ROUND(H, b, c, d, a, in[4] + K3, 15);

    buf[0] += a;
    buf[1] += b;
    buf[2] += c;
    buf[3] += d;

    return buf[1];
}

void
create_padding( const char *string, uint32_t length, uint32_t *blocks, uint32_t blocks_num ) {
    uint32_t padding = length;
    padding |= padding << 8;
    padding |= padding << 16;

    uint32_t size = blocks_num * 4;
    if( length > size ) {
        length = size;
    }

    uint32_t iters = length / 4;

    for( uint32_t i = 0; i < iters; ++ i ) {
        blocks[ i ] = (padding << 8) + *(string++);
        blocks[ i ] = (blocks[ i ] << 8) + *(string++);
        blocks[ i ] = (blocks[ i ] << 8) + *(string++);
        blocks[ i ] = (blocks[ i ] << 8) + *(string++);
    }

    if( iters < size ) {
        blocks[ iters ] = padding;

        for( uint32_t i = 0, remaining = length % 4; i < remaining; ++i ) {
            blocks[ iters ] = (blocks[ iters ] << 8) + *(string++);
        }

        for( uint32_t i = iters + 1; i < blocks_num; ++ i ) {
            blocks[ i ] = padding;
        }
    }
}

uint32_t
ext2_hash_half_md4( const char *string, uint8_t length ) {
    uint32_t buffer[ 4 ] = { 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476 };

    for( int i = length; i > 0; i -= 32) {
        uint32_t blocks[ 8 ];
        create_padding( string, (uint32_t) i, blocks, 8 );
        half_md4_transform( buffer, blocks );

        string += 32;
    }

    return buffer[ 1 ];
}

void
tea_transform( uint32_t buffer[ 4 ], uint32_t blocks[ 4 ] ) {
    uint32_t x = buffer[ 0 ],
             y = buffer[ 1 ];

    uint32_t sum = 0;
    for( int i = 16; i > 0; -- i ) {
        sum += 0x9E3779B9;
        x += ((y << 4) + blocks[ 0 ]) ^ (y + sum) ^ ((y >> 5) + blocks[ 1 ]);
        y += ((x << 4) + blocks[ 2 ]) ^ (x + sum) ^ ((x >> 5) + blocks[ 3 ]);
    }

    buffer[ 0 ] += x;
    buffer[ 1 ] += y;
}

uint32_t
ext2_hash_tea( const char *string, uint8_t length ) {
    uint32_t buffer[ 4 ] = { 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476 };

    for( int i = length; i > 0; i -= 16) {
        uint32_t blocks[ 4 ];
        create_padding( string, (uint32_t) i, blocks, 4 );
        tea_transform( buffer, blocks );

        string += 16;
    }

    return buffer[ 1 ];
}

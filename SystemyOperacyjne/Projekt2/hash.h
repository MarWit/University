#pragma once

#include <stdint.h>

#define DX_HASH_LEGACY 0
#define DX_HASH_HALF_MD4 1
#define DX_HASH_TEA 2

uint32_t ext2_hash_legacy( const char *string, uint8_t length );
uint32_t ext2_hash_half_md4( const char *string, uint8_t length );
uint32_t ext2_hash_tea( const char *string, uint8_t length );

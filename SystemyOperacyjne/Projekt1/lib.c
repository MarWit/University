#include <sys/queue.h>
#include <sys/mman.h>
#include <stdint.h>
#include <stdbool.h>
#include <pthread.h>
#include <unistd.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "lib.h"

#if defined(__i386__)
#define ALIGN(X) (((((X)-1)>>2)<<2) + 4) // Align to 4
#elif defined(__x86_64__)
#define ALIGN(X) (((((X)-1)>>3)<<3) + 8) // Align to 8
#endif

#define ALIGN_TO(X,T) (((X)/T)*T + T)

#define MIN_BLOCK_SIZE 16
#define CHUNK_SIZE 4
#define UNUSED_TRESHOLD 4

typedef struct mem_block {
    int64_t mb_size;                    /* mb_size > 0 => free, mb_size < 0 => allocated */
    union {
        LIST_ENTRY(mem_block) mb_node;  /* node on free block list, valid if block is free */
        uint64_t mb_data[0];            /* user data pointer, valid if block is allocated */
    };
} mem_block_t;

#define ABS(N) ((N) >= 0 ? (N) : -(N))
#define BOUNDARY_TAG(B) *(int64_t *)((char *)(&(B)->mb_data[0]) + ABS((B)->mb_size) - 8)

typedef struct mem_chunk {
    LIST_ENTRY(mem_chunk) ma_node;      /* node on list of all chunks */
    LIST_HEAD(, mem_block) ma_freeblks; /* list of all free blocks in the chunk */
    int32_t size;                       /* chunk size minus sizeof(mem_chunk_t) */
    int32_t free;
    mem_block_t ma_first;               /* first block in the chunk */
} mem_chunk_t;

typedef struct mem_manage {
    LIST_HEAD(, mem_chunk) chunk_list;
    size_t page_size;
    size_t free_total;
    bool debug;
    bool initialized;
    pthread_mutex_t critsec;
} mem_manage_t;

mem_manage_t manager = { .critsec = PTHREAD_MUTEX_INITIALIZER, .initialized = false };
#define MANAGE_INITIALIZED (manager.initialized)
#define DEBUG (manager.debug)

static inline void
manage_init( ) {
    LIST_INIT( & manager.chunk_list );
    manager.page_size = getpagesize( );
    manager.free_total = 0;
    manager.initialized = true;

    manager.debug = (getenv( "MALLOC_DEBUG" ) != NULL);
}

static inline size_t
align_to_pagesize( size_t size ) {
    if( (size % manager.page_size) != 0 ) {
        return manager.page_size * ( (size / manager.page_size) + 1);
    }

    return size;
}

static inline mem_chunk_t *
create_chunk( size_t size ) {
    mem_chunk_t * chunk;
    chunk = mmap( NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0 );

    LIST_INIT( & chunk -> ma_freeblks );
    chunk -> size = size - sizeof( mem_chunk_t );
    chunk -> ma_first . mb_size = chunk -> size;
    BOUNDARY_TAG(&chunk -> ma_first) = chunk -> ma_first . mb_size;
    chunk -> free = chunk -> ma_first . mb_size;

    LIST_INSERT_HEAD( & manager.chunk_list, chunk, ma_node );
    LIST_INSERT_HEAD( & chunk -> ma_freeblks, & chunk -> ma_first, mb_node );

    if( DEBUG )
        fprintf( stderr, "create_chunk( %ld ) = %p\n", size, chunk );

    return chunk;
}

static inline mem_block_t *
split_block( mem_block_t * block, mem_chunk_t * chunk, size_t size ) {
    if( (size_t) block -> mb_size < size + sizeof( mem_block_t ) + MIN_BLOCK_SIZE + 8 )
        return block;

    if( DEBUG )
        fprintf( stderr, "split_block( %p, NULL, %ld )\n", block, size );

    int64_t old_size = block -> mb_size;

    BOUNDARY_TAG( block ) = 0;
    block -> mb_size = size;
    BOUNDARY_TAG( block ) = block -> mb_size;

    mem_block_t * new = (mem_block_t *)((char *)&block -> mb_data[ 0 ] + size );
    new -> mb_size = old_size - size - sizeof( mem_block_t );
    BOUNDARY_TAG( new ) = new -> mb_size;

    LIST_INSERT_AFTER( block, new, mb_node );

    manager.free_total -= sizeof( mem_block_t );
    chunk -> free -= sizeof( mem_block_t );

    return block;
}

static inline mem_block_t *
find_block( size_t size, mem_chunk_t ** chunk ) {
    mem_chunk_t * ch;
    mem_block_t * bl;

    LIST_FOREACH( ch, &manager.chunk_list, ma_node ) {
        if( (size_t) ch -> free < size ) continue;

        LIST_FOREACH( bl, & ch -> ma_freeblks, mb_node ) {
            if( (size_t) bl -> mb_size >= size ) {
                if( DEBUG )
                    fprintf( stderr, "find_block( %ld ) = %p\n", size, bl );

                *chunk = ch;
                return bl;
            }
        }
    }

    if( DEBUG )
        fprintf( stderr, "find_block( %ld ) = NULL\n", size );

    *chunk = NULL;
    return NULL;
}

void *
foo_malloc( size_t size ) {
    if( size < MIN_BLOCK_SIZE ) {
        size = MIN_BLOCK_SIZE + 8;
    } else {
        size = ALIGN(size + 8);
    }

    pthread_mutex_lock( & manager.critsec );

    mem_block_t * block = NULL, * p = NULL;
    mem_chunk_t * chunk = NULL;

    if( ! MANAGE_INITIALIZED ) {
        manage_init( );
    }

    if( size < manager . free_total ) {
        if( DEBUG )
            fprintf( stderr, "malloc( %ld ): Looking for block.. (line: %d)\n", size, __LINE__ );

        block = find_block( size, & chunk );
    }

    if( block == NULL && size > CHUNK_SIZE * manager . page_size ) {
        if( DEBUG )
            fprintf( stderr, "malloc( %ld ): Creating oversized chunk (line: %d)\n", size, __LINE__ );

        size_t chunk_size = align_to_pagesize( size );
        chunk = create_chunk( chunk_size );
        block = & chunk -> ma_first;
    }

    if( block == NULL ) {
        if( DEBUG )
            fprintf( stderr, "malloc( %ld ): Creating new chunk (line: %d)\n", size, __LINE__ );

        chunk = create_chunk( CHUNK_SIZE * manager . page_size );
        block = & chunk -> ma_first;
    }

    p = split_block( block, chunk, size );
    LIST_REMOVE( p, mb_node );

    p -> mb_size = -p -> mb_size;
    BOUNDARY_TAG( p ) = p -> mb_size;

    chunk -> free -= size;
    manager.free_total -= size;

    pthread_mutex_unlock( & manager.critsec );

    if( DEBUG )
        fprintf( stderr, "malloc( %ld ) = %p\n", size, &p -> mb_data[ 0 ] );

    return & p -> mb_data[ 0 ];
}

void *
foo_calloc( size_t count, size_t size ) {
    char * ptr = foo_malloc( size * count );
    for( size_t i = 0; i < size; *(ptr+(i++)) = 0 ) {}

    if( DEBUG )
        fprintf( stderr, "calloc( %ld ) = %p\n", size, ptr );

    return (void *) ptr;
}

static inline mem_block_t *
get_block_from_ptr( void *ptr, mem_chunk_t **chunk ) {
    mem_chunk_t * c;

    LIST_FOREACH( c, & manager.chunk_list, ma_node ) {
        if( ptr > (void *)(& c -> ma_first) && ptr < (void *)((char *)& c -> ma_first + c -> size) ) {
            if( (char *)ptr - 8 >= (char *)(& c -> ma_first) ) {
                *chunk = c;

                if( DEBUG )
                    fprintf( stderr, "get_block_from_ptr( %p ) = %p\n", ptr, (mem_block_t *)((char *)ptr - 8) );

                return (mem_block_t *)((char *)ptr - 8);
            } else {
                if( DEBUG )
                    fprintf( stderr, "get_block_from_ptr( %p ) = NULL (line %d)\n", ptr, __LINE__ );

                return NULL;
            }
        }
    }

    if( DEBUG )
        fprintf( stderr, "get_block_from_ptr( %p ) = NULL (line %d)\n", ptr, __LINE__ );

    return NULL;
}

void *
foo_realloc( void *ptr, size_t size ) {
    pthread_mutex_lock( & manager.critsec );

    mem_block_t * block = NULL, * tmp = NULL;
    mem_chunk_t * chunk = NULL;

    block = get_block_from_ptr( ptr, & chunk );

    if( block == NULL ) {
        pthread_mutex_unlock( & manager.critsec );
        return NULL;
    }

    size_t unaligned_size = size;
    if( size < MIN_BLOCK_SIZE ) {
        size = MIN_BLOCK_SIZE + 8;
    } else {
        size = ALIGN(size + 8);
    }

    assert( block -> mb_size < 0 );
    int64_t block_size = ABS( block -> mb_size );

    if( size == (size_t) block_size ) {
        if( DEBUG )
            fprintf( stderr, "realloc( %p, %ld ): Reallocating to same size\n", ptr, unaligned_size );

        pthread_mutex_unlock( & manager.critsec );
        return ptr;
    }

    if( size < (size_t) block_size ) {
        if( DEBUG )
            fprintf( stderr, "realloc( %p, %ld ): Shinking block %ld -> %ld\n", ptr, unaligned_size, block_size, size );

        int64_t tmp_size = block_size - size - sizeof( mem_block_t );

        if( tmp_size <= 0 ) {
            pthread_mutex_unlock( & manager.critsec );
            return ptr;
        }

        block -> mb_size = -size;
        BOUNDARY_TAG( block ) = block -> mb_size;

        tmp = (mem_block_t *)((char *)&block -> mb_data[ 0 ] + size);

        if( (char *)&tmp -> mb_data[ 0 ] + tmp_size < (char *)chunk + chunk -> size ) {
            mem_block_t *next = (mem_block_t *)((char *)&block -> mb_data[ 0 ] + block_size);
            if( next -> mb_size > 0 ) {
                if( DEBUG )
                    fprintf( stderr, "realloc( %p, %ld ): Found free block next to residual, merging.\n", ptr, unaligned_size  );

                tmp -> mb_size = tmp_size + next -> mb_size + sizeof( mem_block_t );
                BOUNDARY_TAG( tmp ) = tmp -> mb_size;

                LIST_INSERT_BEFORE( next, tmp, mb_node );
                LIST_REMOVE( next, mb_node );

                chunk -> free += sizeof( mem_block_t ) + tmp_size;
                manager.free_total += sizeof( mem_block_t ) + tmp_size;

                pthread_mutex_unlock( & manager.critsec );

                if( DEBUG )
                    fprintf( stderr, "realloc( %p, %ld ) = %p\n", ptr, unaligned_size, & block -> mb_data[ 0 ] );

                return & block -> mb_data[ 0 ];
            }
        }

        if( DEBUG )
            fprintf( stderr, "realloc( %p, %ld ): Creating new free block from residual.\n", ptr, unaligned_size  );

        tmp -> mb_size = tmp_size;
        BOUNDARY_TAG( tmp ) = tmp -> mb_size;

        chunk -> free += tmp_size;
        manager.free_total += tmp_size;

        mem_block_t * next;

        LIST_FOREACH( next, & chunk -> ma_freeblks , mb_node ) {
            if( tmp > next ) break;
        }

        if( next != NULL ) {
            LIST_INSERT_AFTER( next, tmp, mb_node );
        } else {
            LIST_INSERT_HEAD( & chunk -> ma_freeblks, tmp, mb_node );
        }

        if( DEBUG )
            fprintf( stderr, "realloc( %p, %ld ) = %p\n", ptr, unaligned_size, & block -> mb_data[ 0 ] );

        pthread_mutex_unlock( & manager.critsec );
        return & block -> mb_data[ 0 ];
    }

    if( (char *)&block -> mb_data[ 0 ] + block_size < (char *)chunk + chunk -> size ) {
        tmp = (mem_block_t *)((char *)&block -> mb_data[ 0 ] + block_size);

        if(
            tmp -> mb_size > 0 &&
            block -> mb_size + tmp -> mb_size + sizeof( mem_block_t ) >= size
        ) {

            if( DEBUG )
                fprintf( stderr, "realloc( %p, %ld ): Extending block size %ld -> %ld\n", ptr, unaligned_size, block_size, size );

            mem_block_t *next = (mem_block_t *)((char *)&block -> mb_data[ 0 ] + size);
            next -> mb_size = tmp -> mb_size - size;
            BOUNDARY_TAG( next ) = next -> mb_size;

            LIST_INSERT_AFTER( next, tmp, mb_node );
            LIST_REMOVE( tmp, mb_node );

            chunk -> free -= size - block_size;
            manager.free_total -= size - block_size;

            block -> mb_size = -size;
            BOUNDARY_TAG( block ) = block -> mb_size;

            pthread_mutex_unlock( & manager.critsec );
            return & block -> mb_data[ 0 ];
        }
    }

    pthread_mutex_unlock( & manager.critsec );

    if( DEBUG )
        fprintf( stderr, "realloc( %p, %ld ): Block couldn't be extended. We need to allocate new and move data.\n", ptr, unaligned_size );

    void *new = foo_malloc( unaligned_size );
    for( int i = 0; i < block -> mb_size - 8; ++ i ) {
        *((char *)new + i) = *((char *)&block -> mb_data[ 0 ] + i);
    }

    foo_free( ptr );
    return new;
}

void
clean_check( mem_chunk_t *chunk ) {
    mem_block_t *b = LIST_FIRST( & chunk -> ma_freeblks );

    // Its one-element list
    if( LIST_NEXT(b, mb_node) == NULL ) {
        if(
            b -> mb_size == chunk -> size &&
            manager.free_total - chunk -> size > UNUSED_TRESHOLD * manager.page_size
        ) {
            LIST_REMOVE( chunk, ma_node );
            manager.free_total -= chunk -> size;

            munmap( chunk, chunk -> size + sizeof( mem_chunk_t ) );
        }
    }
}

void
foo_free( void *ptr ) {
    pthread_mutex_lock( & manager.critsec );

    mem_block_t * block = NULL, * tmp = NULL;
    mem_chunk_t * chunk = NULL;

    block = get_block_from_ptr( ptr, & chunk );

    if( block == NULL ) {
        pthread_mutex_unlock( & manager.critsec );
        return;
    }

    assert( block -> mb_size < 0 );
    block -> mb_size = - block -> mb_size;
    BOUNDARY_TAG( block ) = block -> mb_size;
    bool indexed = false;

    manager.free_total += block -> mb_size;
    chunk -> free += block -> mb_size;

    if( DEBUG )
        fprintf( stderr, "free( %p ): Freeing %ld bytes\n", ptr, block -> mb_size );


    if( block != & chunk -> ma_first ) {
        int64_t prev_size = *(int64_t *)((char *)block - 8);
        if( prev_size > 0 ) {
            tmp = (mem_block_t *)((char *)block - prev_size - 8);

            if( DEBUG )
                fprintf( stderr, "free( %p ): Merging to block (%p) on the left\n", ptr, tmp );

            tmp -> mb_size += sizeof( mem_block_t ) + block -> mb_size - 16;
            BOUNDARY_TAG( tmp ) = tmp -> mb_size;

            manager.free_total += sizeof( mem_block_t );
            chunk -> free += sizeof( mem_block_t );

            block = tmp;
            indexed = true;
        }
    }

    if( (char *)chunk + chunk -> size > (char *)block + block -> mb_size ) {
        tmp = (mem_block_t *)((char *)&block -> mb_data[ 0 ] + block -> mb_size);

        if( tmp -> mb_size > 0 ) {
            if( DEBUG )
                fprintf( stderr, "free( %p ): Merging with block (%p) on the right\n", ptr, tmp );

            LIST_REMOVE( tmp, mb_node );
            block -> mb_size += tmp -> mb_size + sizeof( mem_block_t );
            BOUNDARY_TAG( block ) = block -> mb_size;

            manager.free_total += sizeof( mem_block_t );
            chunk -> free += sizeof( mem_block_t );
        }
    }

    if( ! indexed ) {
        tmp = NULL;
        LIST_FOREACH( tmp, & chunk -> ma_freeblks , mb_node ) {
            if( tmp < block ) break;
        }

        if( tmp != NULL ) {
            LIST_INSERT_AFTER( tmp, block, mb_node );
        } else {
            LIST_INSERT_HEAD( & chunk -> ma_freeblks, block, mb_node );
        }
    }

    clean_check( chunk );
    pthread_mutex_unlock( & manager.critsec );
}

void
foo_mdump( void ) {
    if( ! MANAGE_INITIALIZED ) {
        fprintf( stderr, "Manager is not initialized yet!\n" );
        return;
    }

    mem_chunk_t *chunk;
    mem_block_t *block;

    int i = 0, j = 0;

    LIST_FOREACH( chunk, & manager.chunk_list, ma_node ) {
        fprintf( stderr, "Chunk #%d: %p-%p, %d/%d free.\n", i, chunk, (char *)chunk + chunk -> size, chunk -> free, chunk -> size );
        block = &chunk -> ma_first;

        for( j = 0 ;; ++ j ) {
            if( (char *)block >= ((char*)chunk + chunk -> size) ) break;

            fprintf( stderr, "\tBlock #%d: %p-%p, size: %ld, used: %d\n",
                    j,
                    & block -> mb_data[ 0 ], (char *) &block -> mb_data[ 0 ] + ABS(block -> mb_size) - 1,
                    ABS(block -> mb_size), block -> mb_size < 0
            );

            block = (mem_block_t *)((char *)(&block -> mb_data[ 0 ]) + ABS(block -> mb_size) );
        }

        ++ i;
    }
}

void
foo_mdump2( void ) {
    mem_chunk_t *chunk;
    mem_block_t *block;

    int i = 0, j = 0;

    LIST_FOREACH( chunk, & manager.chunk_list, ma_node ) {
        fprintf( stderr, "Chunk #%d: %p-%p, %d/%d free.\n", i, chunk, (char *)chunk + chunk -> size, chunk -> free, chunk -> size );
        j = 0;
        LIST_FOREACH( block, & chunk -> ma_freeblks, mb_node ) {
            fprintf( stderr, "\tBlock #%d: %p-%p, size: %ld\n",
                    j,
                    & block -> mb_data[ 0 ], (char *) &block -> mb_data[ 0 ] + block -> mb_size - 1,
                    block -> mb_size
            );

            j ++;
        }

        i ++;
    }

}

mem_manage_t *
get_manager( void ) {
    return &manager;
}

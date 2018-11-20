/*
 * This file is based on example source code 'hello_ll.c'
 *
 * FUSE: Filesystem in Userspace
 * Copyright (C) 2001-2007  Miklos Szeredi <miklos@szeredi.hu>
 *
 * This program can be distributed under the terms of the GNU GPL.
 */

/* NOTE:
 * Program przyjmuje dokładnie takie argumenty jakie zostały
 * wyspecyfikowane w pdf'ie (czyli ext2ro <image> <moutpoint> [rest])
 * mimo tego że "--help" wyświetla inaczej.
 */

#define FUSE_USE_VERSION 30

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <fuse_lowlevel.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>

#include "ext2.h"
#include "blkio.h"
#include "hash.h"


#define UNUSED __attribute__((unused))

typedef struct fuse_file_info fuse_file_info_t;
typedef struct ext2fs_dinode dinode_t;
typedef struct ext2fs_direct_2 dirent_t;

static struct m_ext2fs ext2fs_meta;
static char * image = NULL;

#define BLOCKNO_SUPERBLOCK 0
#define BLOCKNO_GROUPDESCRIPTOR 1
#define BLOCKNO_BLOCKBITMAP 2
#define BLOCKNO_INODEBITMAP 3
#define BLOCKNO_INODETABLE 4

#define blockno(SB, B) ((SB)->e2fs_first_dblock + B)
#define bitmap_get(BM, N) ((BM[N/8] & (1<<(N%8))) != 0)
#define bitmap_set(BM, N) (BM[N/8] |= (1<<(N%8)))
#define bitmap_del(BM, N) (BM[N/8] &= ~(1<<(N%8)))

static void ext2_init(void *userdata UNUSED, struct fuse_conn_info *conn UNUSED) {
    // Open filesystem
    blk_open( image );
    blk_setsize( 0x400 );

    // Read superblock
    ext2fs_meta.e2fs = malloc( SBSIZE );
    blk_read( ext2fs_meta.e2fs, 1 );

    // Is it really ext2?
    assert( ext2fs_meta.e2fs -> e2fs_magic == E2FS_MAGIC );

    // Update block size
    ext2fs_meta.e2fs_bsize = 0x400 << ext2fs_meta.e2fs -> e2fs_log_bsize;
    blk_setsize( ext2fs_meta.e2fs_bsize );

    // Fill some data
    ext2fs_meta.e2fs_bpg = ext2fs_meta.e2fs -> e2fs_bpg;
    ext2fs_meta.e2fs_ipg = ext2fs_meta.e2fs -> e2fs_ipg;
    ext2fs_meta.e2fs_isize = ext2fs_meta.e2fs -> e2fs_inode_size;
    if( ext2fs_meta.e2fs -> e2fs_log_fsize > 0 )
        ext2fs_meta.e2fs_fsize = 0x400 << ext2fs_meta.e2fs -> e2fs_log_fsize;
    else
        ext2fs_meta.e2fs_fsize = 0x400 >> -ext2fs_meta.e2fs -> e2fs_log_fsize;
    ext2fs_meta.e2fs_ipb = ext2fs_meta.e2fs_bsize / ext2fs_meta.e2fs_isize;
    ext2fs_meta.e2fs_gdbcount = ext2fs_meta.e2fs -> e2fs_bcount / ext2fs_meta.e2fs_bpg;

    // Create buffer for data
    char * buffer = malloc( ext2fs_meta.e2fs_bsize * ext2fs_meta.e2fs_gdbcount );

    // Load Group Descriptors
    ext2fs_meta.e2fs_gd = calloc( ext2fs_meta.e2fs_gdbcount, sizeof( struct ext2_gd ) );
    blk_read_n( buffer, blockno(ext2fs_meta.e2fs, BLOCKNO_GROUPDESCRIPTOR), ext2fs_meta.e2fs_gdbcount );
    memcpy( ext2fs_meta.e2fs_gd, buffer, sizeof( struct ext2_gd ) * ext2fs_meta.e2fs_gdbcount );

    // Clean up
    free( buffer );

    // Now we're ready to go
}

static dinode_t * ext2_read_inode(fuse_ino_t ino) {
    if( ino == FUSE_ROOT_ID ) {
        ino = EXT2_ROOTINO;
    }

    ino --;

    int inode_group = ino / ext2fs_meta.e2fs_ipg;
    ino %= ext2fs_meta.e2fs_ipg;

    char *buffer = malloc( ext2fs_meta.e2fs_bsize );
    blk_read( buffer, ext2fs_meta.e2fs_gd[ inode_group ].ext2bgd_i_bitmap );

    if( ! bitmap_get( buffer, ino ) ) {
        free( buffer );
        return NULL;
    }

    int inode_block = ino / ext2fs_meta.e2fs_ipb;
    ino %= ext2fs_meta.e2fs_ipb;

    blk_read( buffer, ext2fs_meta.e2fs_gd[ inode_group ].ext2bgd_i_tables + inode_block );

    dinode_t *di = malloc( ext2fs_meta.e2fs_isize );
    memcpy( di, buffer + ino * ext2fs_meta.e2fs_isize, ext2fs_meta.e2fs_isize );
    free( buffer );

    return di;
}

static void ext2_overwrite_inode(fuse_ino_t ino, dinode_t *di) {
    if( ino == FUSE_ROOT_ID ) {
        ino = EXT2_ROOTINO;
    }

    ino --;

    int inode_group = ino / ext2fs_meta.e2fs_ipg;
    ino %= ext2fs_meta.e2fs_ipg;

    int inode_block = ino / ext2fs_meta.e2fs_ipb;
    ino %= ext2fs_meta.e2fs_ipb;

    char *buffer = malloc( ext2fs_meta.e2fs_bsize );
    blk_read( buffer, ext2fs_meta.e2fs_gd[ inode_group ].ext2bgd_i_tables + inode_block );

    memcpy( buffer + ino * ext2fs_meta.e2fs_isize, di, ext2fs_meta.e2fs_isize );
    blk_write( buffer, ext2fs_meta.e2fs_gd[ inode_group ].ext2bgd_i_tables + inode_block );

    free( buffer );
}

static void ext2_destroy(void *userdata UNUSED) {
    // Clean up..
    blk_close();

    free( image );
    free( ext2fs_meta.e2fs );
    free( ext2fs_meta.e2fs_gd );
}

static inline void ext2_fill_stat(fuse_ino_t ino, dinode_t *di,
        struct stat *stbuf) {
    stbuf -> st_ino   = ino;
    stbuf -> st_mode  = di -> e2di_mode;
    stbuf -> st_uid   = di -> e2di_uid;
    stbuf -> st_gid   = di -> e2di_gid;
    stbuf -> st_size  = di -> e2di_size;
    stbuf -> st_atime = di -> e2di_atime;
    stbuf -> st_ctime = di -> e2di_ctime;
    stbuf -> st_mtime = di -> e2di_mtime;
    stbuf -> st_nlink = di -> e2di_nlink;
}

static int ext2_stat(fuse_ino_t ino, struct stat *stbuf) {
    dinode_t * di = ext2_read_inode( ino );

    if( di == NULL ) {
        return -1;
    }

    ext2_fill_stat( ino, di, stbuf );
    free( di );
    return 0;
}

#define min(x, y) ((x) < (y) ? (x) : (y))

static void ext2_getattr(fuse_req_t req, fuse_ino_t ino,
        fuse_file_info_t *fi UNUSED) {
    struct stat stbuf;

    memset(&stbuf, 0, sizeof(stbuf));
    if (ext2_stat(ino, &stbuf) == -1) {
        fuse_reply_err(req, ENOENT);
    } else {
        fuse_reply_attr(req, &stbuf, 1.0);
    }
}

static void ext2_setattr(fuse_req_t req, fuse_ino_t ino, struct stat *attr,
        int to_set, fuse_file_info_t *fi UNUSED) {

    if( ext2fs_meta.e2fs_ronly ) {
        fuse_reply_err( req, EACCES );
        return;
    }

    dinode_t *di = ext2_read_inode( ino );
    if( di == NULL ) {
        fuse_reply_err( req, ENOENT );
        return;
    }

    if( (to_set & FUSE_SET_ATTR_MODE) != 0 )
        di -> e2di_mode = attr -> st_mode;

    if( (to_set & FUSE_SET_ATTR_UID) != 0 )
        di -> e2di_uid = attr -> st_uid;

    if( (to_set & FUSE_SET_ATTR_GID) != 0 )
        di -> e2di_gid = attr -> st_gid;

    if( (to_set & FUSE_SET_ATTR_SIZE) != 0 )
        di -> e2di_size = attr -> st_size;

    if( (to_set & FUSE_SET_ATTR_ATIME) != 0 )
        di -> e2di_atime = attr -> st_atime;

    if( (to_set & FUSE_SET_ATTR_MTIME) != 0 )
        di -> e2di_mtime = attr -> st_mtime;

    ext2_overwrite_inode( ino, di );
    ext2_fill_stat( ino, di, attr );
    free( di );

    fuse_reply_attr(req, attr, 1.0);
}

static inline void ext2_read_block(char *output, dinode_t *di, uint32_t block) {
    uint32_t indirect_size = ext2fs_meta.e2fs_bsize / 4;
    uint32_t indirect_size2 = indirect_size * indirect_size;

    if( block < EXT2_NDIR_BLOCKS ) {
        blk_read( output, di -> e2di_blocks[ block ] );
    } else {
        uint32_t * buffer = malloc( ext2fs_meta.e2fs_bsize );

        if( (block -= EXT2_NDIR_BLOCKS) < indirect_size ) {
            blk_read( buffer, di -> e2di_blocks[ EXT2_NDIR_BLOCKS ] );
            blk_read( output, buffer[ block ] );
        } else if( (block -= indirect_size) < indirect_size2 ) {
            blk_read( buffer, di -> e2di_blocks[ EXT2_DIND_BLOCK ] );
            blk_read( buffer, buffer[ block / indirect_size ] );
            blk_read( output, buffer[ block % indirect_size ] );
        } else {
            block -= indirect_size2;

            blk_read( buffer, di -> e2di_blocks[ EXT2_TIND_BLOCK ] );
            blk_read( buffer, buffer[ block / indirect_size2 ] );
            blk_read( buffer, buffer[ (block / indirect_size) % indirect_size ] );
            blk_read( output, buffer[ block % indirect_size ] );
        }

        free( buffer );
    }
}

static inline void ext2_read_block_ex(char *output, dinode_t *di, uint32_t amount, uint32_t offset) {
    uint32_t block = offset / ext2fs_meta.e2fs_bsize;
    offset %= ext2fs_meta.e2fs_bsize;

    char * buffer = malloc( ext2fs_meta.e2fs_bsize );
    for( ;; ) {
        ext2_read_block( buffer, di, block );
        memcpy( output, buffer + offset, min( ext2fs_meta.e2fs_bsize, amount ) );

        if( ext2fs_meta.e2fs_bsize - offset >= amount ) {
            break;
        } else {
            amount -= (ext2fs_meta.e2fs_bsize - offset);
            output += (ext2fs_meta.e2fs_bsize - offset);
            block ++;
            offset = 0;
        }
    }

    free( buffer );
}

static void ext2_lookup(fuse_req_t req, fuse_ino_t parent, const char *name) {
    dinode_t * iparent = ext2_read_inode( parent );
    assert( iparent != NULL );
    assert( S_ISDIR( iparent -> e2di_mode ) != 0 );

    char * entry = malloc( ext2fs_meta.e2fs_bsize );
    dirent_t * query = NULL;
    uint32_t namelen = strlen( name );
    bool found = false;

    for( uint32_t i = 0; i < iparent -> e2di_nblock; ++ i ) {
        ext2_read_block( entry, iparent, i );
        query = (dirent_t *) entry;

        while( (char *) query < entry + ext2fs_meta.e2fs_bsize && query -> e2d_ino ) {
            if( query -> e2d_namlen == namelen &&
                memcmp( query -> e2d_name, name, namelen ) == 0
            ) {
                found = true;
                break;
            }

            query = (void *)query + query -> e2d_reclen;
        }

        if( found ) break;
    }

    free( iparent );

    if( found ) {
        struct fuse_entry_param e;
        memset(&e, 0, sizeof(e));

        e.ino = query -> e2d_ino;
        e.attr_timeout = 1.0;
        e.entry_timeout = 1.0;

        ext2_stat( e.ino, & e.attr );
        fuse_reply_entry( req, &e );
    } else {
        fuse_reply_err(req, ENOENT);
    }

    free( entry );
}

/*
static uint32_t ext2_generate_hash(const char *input, size_t len, uint8_t hash_version) {
    switch( hash_version ) {
        case DX_HASH_LEGACY:
            return ext2_hash_legacy( input, len ) & ~1;

        case DX_HASH_HALF_MD4:
            return ext2_hash_half_md4( input, len ) & ~1;

        case DX_HASH_TEA:
            return ext2_hash_tea( input, len ) & ~1;

        default: assert( 1 == 0 ); // ???
    }
}

static void ext2_lookup_htree(fuse_req_t req, fuse_ino_t parent, const char *name) {
    dinode_t * iparent = ext2_read_inode( parent );
    assert( iparent != NULL );
    assert( S_ISDIR( iparent -> e2di_mode ) != 0 );

    if( (iparent -> e2di_flags & EXT4_INDEX) == 0 ) {
        free( iparent );
        printf( "HTree > Legacy!\n" );
        ext2_lookup( req, parent, name );
        return;
    }

    printf( "HTree !\n" );

    struct ext2fs_htree_root * root = malloc( ext2fs_meta.e2fs_bsize );
    ext2_read_block( (char *) root, iparent, 0 );

    printf( "Hash version: %d\n", root -> h_info.h_hash_version );
    printf( "Info len: %d\n", root -> h_info.h_info_len );
    printf( "Ind levels: %d\n", root -> h_info.h_ind_levels );

    struct ext2fs_htree_count * htree_cnt = (struct ext2fs_htree_count *) & root -> h_entries[ 0 ];

    printf( "Entries max: %d\n", htree_cnt -> h_entries_max );
    printf( "Entries num: %d\n", htree_cnt -> h_entries_num );

    uint32_t hash = ext2_generate_hash( name, strlen( name ), root -> h_info.h_hash_version );
    printf( "Hash: %x\n", hash );

    uint32_t l = 1, r = htree_cnt -> h_entries_num, m UNUSED;
    struct ext2fs_htree_node *node = malloc( ext2fs_meta.e2fs_bsize );

    for( ; l < r; ++ l ) {
        printf( "Hash #%d: %x -> %d\n", l, root -> h_entries[ l ].h_hash, root -> h_entries[ l ].h_blk );
        ext2_read_block( (char *) node, iparent, root -> h_entries[ l ].h_blk );
        printf( "Entry 0: Hash=%x, blk=%d\n", node -> h_entries[ 0 ].h_hash, node -> h_entries[ 0 ].h_blk );
        printf( "Entry 1: Hash=%x, blk=%d\n", node -> h_entries[ 1 ].h_hash, node -> h_entries[ 1 ].h_blk );
    }

    free( node );
    free( root );
    free( iparent );
    fuse_reply_err(req, ENOENT);
} */

typedef struct {
    char *p;
    size_t size;
} dirbuf_t;

typedef struct {
    dinode_t * di;
    dirbuf_t dirbuf;
} dir_t;

static void dirbuf_add(fuse_req_t req, dirbuf_t *b, const char *name,
        fuse_ino_t ino) {
    struct stat stbuf;
    size_t oldsize = b->size;
    b->size += fuse_add_direntry(req, NULL, 0, name, NULL, 0);
    b->p = (char *)realloc(b->p, b->size);
    memset(&stbuf, 0, sizeof(stbuf));
    stbuf.st_ino = ino;
    fuse_add_direntry(req, b->p + oldsize, b->size - oldsize, name, &stbuf,
            b->size);
}

static int reply_buf_limited(fuse_req_t req, const char *buf, size_t bufsize,
        off_t off, size_t maxsize) {
    if (off < (ssize_t)bufsize) {
        return fuse_reply_buf(req, buf + off, min(bufsize - off, maxsize));
    } else {
        return fuse_reply_buf(req, NULL, 0);
    }
}

static void ext2_opendir(fuse_req_t req, fuse_ino_t ino,
        fuse_file_info_t *fi) {

    fi -> fh = 0;
    dinode_t * di = ext2_read_inode( ino );

    if( di == NULL ) {
        fuse_reply_err(req, ENOENT);
        return;
    }

    if( ! S_ISDIR( di -> e2di_mode ) ) {
        free( di );
        fuse_reply_err(req, ENOTDIR);
        return;
    }

    dir_t * dir = calloc( 1, sizeof( dir_t ) );
    dir -> di = di;

    fi -> fh = (uint64_t) dir;
    fuse_reply_open(req, fi);
}

static void ext2_readdir(fuse_req_t req, fuse_ino_t ino UNUSED, size_t size,
        off_t off, fuse_file_info_t *fi) {

    dir_t * dir = (dir_t *) fi -> fh;

    if( dir -> dirbuf.p == NULL ) {
        char * entry = malloc( ext2fs_meta.e2fs_bsize );
        dirent_t * query = NULL;

        char name_buffer[ EXT2FS_MAXNAMLEN ];

        for( uint32_t i = 0; i < dir -> di -> e2di_nblock; ++ i ) {
            ext2_read_block( entry, dir -> di, i );
            query = (dirent_t *) entry;

            while( (char *)query < entry + ext2fs_meta.e2fs_bsize && query -> e2d_ino ) {
                memcpy( name_buffer, query -> e2d_name, query -> e2d_namlen );
                name_buffer[ query -> e2d_namlen ] = 0;

                dirbuf_add( req, & dir -> dirbuf, name_buffer, query -> e2d_ino );
                query = (void *)query + query -> e2d_reclen;
            }
        }

        free( entry );
    }

    reply_buf_limited(req, dir -> dirbuf.p, dir -> dirbuf.size, off, size);

    if( off + size >= dir -> dirbuf.size ) {
        free( dir -> dirbuf.p );
        memset( & dir -> dirbuf, 0, sizeof( dirbuf_t ) );
    }
}

static void ext2_releasedir(fuse_req_t req, fuse_ino_t ino UNUSED,
        fuse_file_info_t *fi) {

    if( fi -> fh != 0 ) {
        dir_t * dir = (dir_t *) fi -> fh;

        free( dir -> di );
        free( dir );

        fi -> fh = 0;
    }

    fuse_reply_err(req, ENOENT);
}

static void ext2_open(fuse_req_t req, fuse_ino_t ino, fuse_file_info_t *fi) {
    fi -> fh = 0;

    if( (fi -> flags & 3) != O_RDONLY ) {
        fuse_reply_err(req, EACCES);
        return;
    }

    dinode_t *di = ext2_read_inode( ino );

    if( di == NULL ) {
        fuse_reply_err(req, ENOENT);
        return;
    }

    if( S_ISDIR( di -> e2di_mode ) ) {
        free( di );
        fuse_reply_err(req, EISDIR);
        return;
    }

    fi -> fh = (uint64_t) di;

    fuse_reply_open(req, fi);
}

static void ext2_read(fuse_req_t req, fuse_ino_t ino UNUSED, size_t size, off_t off,
        fuse_file_info_t *fi) {

    dinode_t *di = (dinode_t *) fi -> fh;

    size_t bufsize = min(size, (size_t) di -> e2di_size - off);

    char * buffer = malloc( bufsize );
    ext2_read_block_ex( buffer, di, bufsize, off );
    fuse_reply_buf( req, buffer, bufsize );

    free( buffer );
}

static void ext2_release(fuse_req_t req, fuse_ino_t ino UNUSED,
        fuse_file_info_t *fi) {

    if( fi -> fh != 0 ) {
        free( (dinode_t *) fi -> fh );
        fi -> fh = 0;
    }

    fuse_reply_err(req, ENOENT);
}

static void ext2_readlink(fuse_req_t req, fuse_ino_t ino) {
    dinode_t *di = ext2_read_inode( ino );

    if( di == NULL ) {
        fuse_reply_err(req, ENOENT);
        return;
    }

    if( ! S_ISLNK( di -> e2di_mode ) ) {
        free( di );
        fuse_reply_err(req, ENOLINK);
        return;
    }

    char *symlink = malloc( di -> e2di_size + 1 );

    if( di -> e2di_size <= sizeof( di -> e2di_blocks ) ) {
        memcpy( symlink, di -> e2di_blocks, di -> e2di_size );
    } else {
        ext2_read_block_ex( symlink, di, di -> e2di_size, 0 );
    }

    symlink[ di -> e2di_size ] = '\0';

    fuse_reply_readlink( req, symlink );

    free( symlink );
    free( di );
}

static void ext2_statfs(fuse_req_t req, fuse_ino_t ino UNUSED) {
    struct statvfs statfs = {
        .f_bsize   = ext2fs_meta.e2fs_bsize,
        .f_frsize  = ext2fs_meta.e2fs_fsize,
        .f_blocks  = ext2fs_meta.e2fs -> e2fs_bcount,
        .f_bfree   = ext2fs_meta.e2fs -> e2fs_fbcount,
        .f_bavail  = ext2fs_meta.e2fs -> e2fs_fbcount,
        .f_files   = ext2fs_meta.e2fs -> e2fs_icount,
        .f_ffree   = ext2fs_meta.e2fs -> e2fs_ficount,
        .f_favail  = ext2fs_meta.e2fs -> e2fs_ficount,
        .f_fsid    = ext2fs_meta.e2fs -> e2fs_magic,
        .f_flag    = 0,
        .f_namemax = EXT2FS_MAXNAMLEN
    };

    fuse_reply_statfs(req, &statfs);
}

static struct fuse_lowlevel_ops ext2_oper = {
    .init       = ext2_init,
    .destroy    = ext2_destroy,
    .lookup     = ext2_lookup,
    // .lookup     = ext2_lookup_htree,
    .getattr    = ext2_getattr,
    .setattr    = ext2_setattr,
    .opendir    = ext2_opendir,
    .readdir    = ext2_readdir,
    .releasedir = ext2_releasedir,
    .open       = ext2_open,
    .read       = ext2_read,
    .release    = ext2_release,
    .readlink   = ext2_readlink,
    .statfs     = ext2_statfs
};

static int ext2_fuse_optproc( void * data UNUSED, const char *arg,
        int key, struct fuse_args *outargs UNUSED ) {
    if( key == FUSE_OPT_KEY_NONOPT && image == NULL ) {
        image = strdup( arg );
        return 0;
    }

    return 1;
}

int main(int argc, char *argv[]) {
    struct fuse_args args = FUSE_ARGS_INIT(argc, argv);
    struct fuse_chan *ch;
    char *mountpoint;
    int err = -1;

    fuse_opt_parse( &args, NULL, NULL, ext2_fuse_optproc );

    if (fuse_parse_cmdline(&args, &mountpoint, NULL, NULL) != -1 &&
            (ch = fuse_mount(mountpoint, &args)) != NULL) {
        struct fuse_session *se;

        se = fuse_lowlevel_new(&args, &ext2_oper, sizeof(ext2_oper), NULL);
        if (se != NULL) {
            if (fuse_set_signal_handlers(se) != -1) {
                fuse_session_add_chan(se, ch);
                err = fuse_session_loop(se);
                fuse_remove_signal_handlers(se);
                fuse_session_remove_chan(ch);
            }
            fuse_session_destroy(se);
        }
        fuse_unmount(mountpoint, ch);
    }
    fuse_opt_free_args(&args);

    return err ? 1 : 0;
}

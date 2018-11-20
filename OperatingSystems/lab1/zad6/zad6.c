#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <syslog.h>
#include <unistd.h>

/*
 * Założyłem, że "Działanie demona należy zakończyć opuszczając procedurę main"
 * należy interpretować tak, że powinniśmy opuścić ją używając exit() wtw
 * nie było żadnych komplikacji przy demonizowaniu programu. ( czyli dla SIGTERM,
 * SIGKILL, etc. jak już daemon wystartował )
 */

void
demonize( const char *cmd ) {
    int fd0, fd1, fd2;
    unsigned int i;
    pid_t pid;
    struct rlimit rl;
    struct sigaction sa;

    umask( 0 );
    if( getrlimit( RLIMIT_NOFILE, & rl ) < 0 ) {
        printf( "getrlimit failed\n" );
        exit( 1 );
    }

    if( (pid = fork()) < 0 ) {
        printf( "fork failed\n" );
        exit( 1 );
    } else if( pid != 0 )
        exit( 0 );

    setsid( );

    sa.sa_handler = SIG_IGN;
    sigemptyset( & sa.sa_mask );
    sa.sa_flags = 0;

    if( sigaction( SIGHUP, &sa, NULL ) < 0 ) {
        printf( "sigaction failed (demonize)\n" );
        exit( 1 );
    }
    if( (pid = fork()) < 0 ) {
        printf( "fork failed\n" );
        exit( 1 );
    } else if( pid != 0 )
        exit( 0 );

    if( chdir( "/" ) < 0 ) {}
    if( rl.rlim_max == RLIM_INFINITY )
        rl.rlim_max = 1024;

    for( i = 0; i < rl.rlim_max; i ++ )
        close( i );

    fd0 = open( "/dev/null", O_RDWR );
    fd1 = dup( 0 );
    fd2 = dup( 0 );

    openlog( cmd, LOG_CONS, LOG_DAEMON );

    if( fd0 != 0 || fd1 != 1 || fd2 != 2 ) {
        syslog(
                LOG_ERR,
                "unexpected file descriptors %d %d %d",
                fd0, fd1, fd2
        );
        exit( 1 );
    }
}

int
already_running( void ) {
    const char run[] = "/tmp/zad6.pid";
    char pid[ 16 ] = { 0 };

    int fd = open( run, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH );
    if( fd < 0 ) {
        syslog( LOG_ERR, "couldn't create lock file" );
        exit( 1 );
    }

    if( lockf( fd, F_TLOCK, 0 ) < 0 ) {
        if( errno == EACCES || errno == EAGAIN ) {
            close( fd );
            return 1;
        }

        exit( 1 );
    }

    ftruncate( fd, 0 );
    sprintf( pid, "%ld", (long) getpid() );
    write( fd, pid, 15 );

    return 0;
}

int
main( int argc __attribute__((unused)), char **argv ) {
    char *cmd;

    if( ( cmd = strrchr( argv[ 0 ], '/' )) == NULL )
        cmd = argv[ 0 ];
    else
        cmd ++;

    demonize( cmd );

    if( already_running()) {
        syslog( LOG_ERR, "daemon already running" );
        exit( 1 );
    }

    struct sigaction sa;
    sa.sa_handler = SIG_DFL;
    sigemptyset( &sa.sa_mask );
    sa.sa_flags = 0;

    if( sigaction( SIGHUP, &sa, NULL ) < 0 ) {
        syslog( LOG_ERR, "sigaction failed" );
        exit( 1 );
    }

    sigset_t mask;
    sigfillset( &mask );

    if( sigprocmask( SIG_BLOCK, &mask, NULL ) != 0 ) {
        syslog( LOG_ERR, "sigprocmask failed" );
        exit( 1 );
    }

    int err, signo;
    int counter = 0;

    for( ;; ) {
        err = sigwait( &mask, &signo );
        if( err != 0 ) {
            syslog( LOG_ERR, "sigwait failed" );
            exit( 1 );
        }

        switch( signo ) {
            case SIGHUP: {
                counter = 0;
                syslog( LOG_INFO, "SIGHUP - counter: %d", counter );
                break;
            }

            case SIGTERM: {
                syslog( LOG_INFO, "SIGTERM - counter: %d", counter );
                exit( 0 );
            }

            case SIGUSR1: {
                counter ++;
                syslog( LOG_INFO, "SIGUSR1 - counter: %d", counter );
                break;
            }
        }
    }

    exit( 0 );
}

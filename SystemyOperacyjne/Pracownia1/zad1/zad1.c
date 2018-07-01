#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <signal.h>
#include <wait.h>
#include <sys/prctl.h>

#define IS_CHILD(P) (P==0)

void
entry_point_a( int8_t option ) {
    if( option ) {
        struct sigaction new;
        new.sa_handler = SIG_IGN;
        sigemptyset( &new.sa_mask );
        new.sa_flags = 0;

        sigaction( SIGCHLD, &new, NULL );
        printf( "SIGCHLD is now ignored!\n" );
    }

    char *const params[] = { "/usr/bin/ps", "au" };
    char *const env[] = { NULL };

    pid_t pid = fork();

    if( IS_CHILD( pid ) ) {
        execve( "/usr/bin/ps", params, env );
    } else {
        printf( "Now you can check for zombie process.. Press ENTER to continue\n" );
        getchar();
    }
}

void
entry_point_b( int8_t option ) {
    if( ! option ) {
        prctl( PR_SET_CHILD_SUBREAPER, 1 );
        printf( "Process is now set as subreaper!\n" );
    }

    pid_t pid1 = fork();

    if( IS_CHILD( pid1 ) ) {
        pid_t pid2 = fork();

        if( ! IS_CHILD( pid2 ) ) {
            exit( 0 );
        }
    } else {
        int s;
        waitpid( pid1, &s, 0 );
        printf( "Now you can check for descendants.. Press ENTER to continue\n" );
        getchar();
    }
}

void
usage( char * prog ) {
    printf( "Usage: %s [--a 0/1] [--b 0/1]\n", prog );
    printf( "\t --a 0 - a without sigaction\n" );
    printf( "\t --a 1 - a with sigaction\n" );
    printf( "\t --b 0 - b and program will be reaper\n" );
    printf( "\t --b 1 - b and program won't be reaper\n" );
}

int
main( int argc, char ** argv ) {
    if( argc < 3 ) {
        usage( argv[ 0 ] );
        return 0;
    }

    int8_t variant = argv[ 2 ][ 0 ] - '0';
    if( variant < 0 || variant > 1 ) {
        usage( argv[ 0 ] );
        return 0;
    }

    if( ! strcmp( "--a", argv[ 1 ] ) ) {
        entry_point_a( variant );
    } else if( ! strcmp( "--b", argv[ 1 ] ) ) {
        entry_point_b( variant );
    } else {
        usage( argv[ 0 ] );
    }

    return 0;
}

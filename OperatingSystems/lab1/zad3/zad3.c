#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <wait.h>

int
reader( int fd_out ) {
    int words = 0;
    char word[ 64 ];

    while( ! feof( stdin ) ) {
        if( fscanf( stdin, "%s ", word ) > 0 ) {
            words ++;
            write( fd_out, word, 63 );
        }
    }

    close( fd_out );

    fflush( stdout );
    printf( "Words: %d\n", words );

    return 0;
}

int
transformer( int fd_in, int fd_out ) {
    char word[ 64 ];
    char clean_word[ 64 ];
    unsigned int i, j;

    while( read( fd_in, word, 63 ) != 0 ) {
        for( i = 0, j = 0; i < strlen( word ); i ++ ) {
            if( isalnum( word[ i ] ) )
                clean_word[ j ++ ] = word[ i ];
        }

        clean_word[ j ] = '\0';
        write( fd_out, clean_word, 63 );
    }

    close( fd_in );
    close( fd_out );

    return 0;
}

int
writer( int fd_in ) {
    char word[ 64 ];
    int chars = 0;

    while( read( fd_in, word, 63 ) != 0 ) {
        chars += strlen( word );
        printf( "%s ", word );
    }

    close( fd_in );
    printf( "\nChars: %d\n", chars );

    return 0;
}

void
translate_status( const char *func, int *status ) {
    if( WIFEXITED( *status ) ) {
        fprintf( stderr, "%s exited with status = %d.\n", func, WEXITSTATUS( *status ) );
    } else if( WIFSIGNALED( *status ) ) {
        fprintf( stderr, "%s was terminated by signal with id: %d\n", func, WTERMSIG( *status ) );
    }
}

int
main( void ) {
    int read_conv[ 2 ];
    pipe( read_conv );

    pid_t reader_p = fork();

    if( reader_p == 0 )
        return reader( read_conv[ 1 ] );

    close( read_conv[ 1 ] );

    int conv_show[ 2 ];
    pipe( conv_show );

    pid_t transf_p = fork();

    if( transf_p == 0 )
        return transformer( read_conv[ 0 ], conv_show[ 1 ] );

    close( read_conv[ 0 ] );
    close( conv_show[ 1 ] );

    pid_t writer_p = fork();

    if( writer_p == 0 )
        return writer( conv_show[ 0 ] );

    close( conv_show[ 0 ] );

    int status;

    waitpid( reader_p, & status, 0 );
    translate_status( "reader()", & status );

    waitpid( transf_p, & status, 0 );
    translate_status( "transformer()", & status );

    waitpid( writer_p, & status, 0 );
    translate_status( "writer()", & status );

    return 0;
}

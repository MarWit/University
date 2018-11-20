#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <ucontext.h>

int words = 0;
int chars = 0;

char word[ 64 ];
char clean_word[ 64 ];
char stdin_closed = 0;

static ucontext_t ucxt_main, ucxt_reader,
                  ucxt_transformer, ucxt_writer;
int
throw_error( const char* reason ) {
    printf( "[Error]: %s\n", reason );
    return -1;
}


void
reader() {
    for( ;; ) {
        if( ! feof( stdin ) ) {
            if( fscanf( stdin, "%s ", word ) > 0 ) {
                words ++;
            }

            if( swapcontext( &ucxt_reader, &ucxt_transformer ) == -1 ) {
                throw_error( "Couldn't swap context: reader -> transformer." );
                break;
            }
        } else {
            fflush( stdout );
            printf( "\nWords: %d\n", words );
            stdin_closed = 1;
            break;
        }
    }
}

void
transformer( ) {
    for( ;; ) {
        unsigned int i = 0, j = 0;
        for( ; i < strlen( word ); i ++ ) {
            if( isalnum( word[ i ] ) )
                clean_word[ j ++ ] = word[ i ];
        }

        clean_word[ j ] = '\0';

        if( ! stdin_closed ) {
            if( swapcontext( &ucxt_transformer, &ucxt_writer ) == -1 ) {
                throw_error( "Couldn't swap context: transformer -> writer." );
                break;
            }
        } else break;
    }
}

void
writer() {
    for( ;; ) {
        if( stdin_closed ) {
            printf( "Chars: %d\n", chars );
            break;
        } else {
            chars += strlen( clean_word );
            printf( "%s ", clean_word );

            if( swapcontext( &ucxt_writer, &ucxt_reader ) == -1 ) {
                throw_error( "Couldn't swap context: writer -> reader." );
                break;
            }
        }
    }
}

int
main( void ) {
    char stacks[ 3 ][ 1 << 14 ];

    if( getcontext( & ucxt_reader ) == -1 ) {
        return throw_error( "Couldn't create context for reader." );
    }

    ucxt_reader.uc_stack.ss_sp = stacks[ 0 ];
    ucxt_reader.uc_stack.ss_size = (1 << 14);
    ucxt_reader.uc_link = &ucxt_transformer;

    makecontext( &ucxt_reader, reader, 0 );

    if( getcontext( & ucxt_transformer ) == -1 ) {
        return throw_error( "Couldn't create context for transformer." );
    }

    ucxt_transformer.uc_stack.ss_sp = stacks[ 1 ];
    ucxt_transformer.uc_stack.ss_size = (1 << 14);
    ucxt_transformer.uc_link = &ucxt_writer;

    makecontext( &ucxt_transformer, transformer, 0 );

    if( getcontext( & ucxt_writer ) == -1 ) {
        return throw_error( "Couldn't create context for writer." );
    }

    ucxt_writer.uc_stack.ss_sp = stacks[ 2 ];
    ucxt_writer.uc_stack.ss_size = (1 << 14);
    ucxt_writer.uc_link = &ucxt_main;

    makecontext( &ucxt_writer, writer, 0 );

    if( swapcontext( &ucxt_main, &ucxt_reader ) == -1 ) {
        return throw_error( "Couldn't swap context with reader." );
    }

    return 0;
}

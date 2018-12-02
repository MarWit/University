#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>
#include <util/delay.h>

// #include "../common/uart.h"
#include "../common/i2c.h"

#define MIN(A,B) ((A) < (B) ? (A) : (B))

#define BAUD (9600)

int
uart_getc( FILE *s ) {
    while( !(UCSR0A & _BV(RXC0) ) );

    return UDR0;
}

int
uart_putc( char c, FILE *s ) {
    while( ! ( UCSR0A & _BV(UDRE0) ) );
    UDR0 = c;

    return 0;
}

static inline void
uart_init( void ) {
    UBRR0 = (F_CPU / 16 / BAUD - 1);
    UCSR0A = 0;
    UCSR0B = _BV(RXEN0) | _BV(TXEN0);
    UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);

    static FILE fd;
    fdev_setup_stream( & fd, uart_putc, uart_getc, _FDEV_SETUP_RW );
    stdin = stdout = stderr = & fd;
}

void
read_line( char *buffer, uint16_t len ) {
    for( uint16_t i = 0; i < len; ++i ) {
        buffer[ i ] = getchar();
        putchar( buffer[ i ] );

        if( buffer[ i ] == '\r' ) {
            buffer[ i ] = '\0';
            putchar( '\n' );
            break;
        }
    }
}

typedef void (*func_t)(void);
typedef struct {
    char *name;
    func_t func;
} handle_t;


void
simple_read( char *addr_s ) {
    uint16_t addr = strtol( addr_s, NULL, 0 );

    i2cStart();
    i2cSend( 0xa0 | (addr & 0x100) >> 7 );
    i2cSend( addr & 0xff );

    i2cStart();
    i2cSend( 0xa1 | (addr & 0x100) >> 7 );
    uint8_t byte = i2cReadNoAck();
    i2cStop();

    printf( "0x%.4X -> 0x%.2X\r\n", addr, byte );
}

void
simple_write( char *addr_s, char *value_s ) {
    uint16_t addr = strtol( addr_s, NULL, 0 );
    uint8_t byte = strtol( value_s, NULL, 0 );

    i2cStart();
    i2cSend( 0xa0 | (addr & 0x100) >> 7 );
    i2cSend( addr & 0xff );
    i2cSend( byte );
    i2cStop();

    printf( "0x%.4X <- 0x%.2X\r\n", addr, byte );
}

void
handle_read( void ) {
    char *addr_s = strtok( NULL, " " );
    char *length_s = strtok( NULL, " " );

    if( length_s == NULL ) {
        simple_read( addr_s );
        return;
    }

    uint16_t addr = strtol( addr_s, NULL, 0 );
    uint16_t length = strtol( length_s, NULL, 0 );

    i2cStart();
    i2cSend( 0xa0 | (addr & 0x100) >> 7 );
    i2cSend( addr & 0xff );

    i2cStart();
    i2cSend( 0xa1 | (addr & 0x100) >> 7 );

    uint8_t k = 0;
    uint8_t check = 0;
    uint8_t byte = 0;

    for( uint16_t i = 0; i < length; ++ i ) {
        if( k == 0 ) {
            uint16_t l = length - i;
            if( l >= 0x20 ) l = 0x20;

            printf( ":%.2X%.4X00", l, addr );
            check = l + (uint8_t)addr + (uint8_t)(addr >> 4);
        }

        if( i == length - 1 )
            byte = i2cReadNoAck();
        else
            byte = i2cReadAck();

        printf( "%.2X", byte );
        check += byte;

        if( ++k == 0x20 ) {
            k = 0;
            printf( "%.2X\r\n", (~check + 1) & 0xff );
            addr += 0x20;
        }
    }

    i2cStop();

    if( k != 0 ) {
        printf( "%.2X\r\n", (~check + 1) & 0xff );
    }

    printf( ":00000001FF\r\n" );
}

void
handle_write( void ) {
    char buffer[ 0x100 ];

    char *addr_s = strtok( NULL, " " );
    char *value_s = strtok( NULL, " " );

    if( addr_s != NULL && value_s != NULL ) {
        simple_write( addr_s, value_s );
        return;
    }

    for( ;; ) {
        printf( "> " );
        read_line( buffer, 0x100 );
        if( buffer[ 0 ] != ':' ) break;

        uint16_t len;
        sscanf( buffer + 1, "%2x", & len );

        if( strlen( buffer ) != (1 + 2 + 4 + 2 + len * 2 + 2) )
            continue;

        uint8_t mode;
        sscanf( buffer + 7, "%2hhx", & mode );
        if( mode ) break;

        uint16_t addr;
        sscanf( buffer + 3, "%4x", & addr );

        uint8_t byte;

        i2cStart();
        i2cSend( 0xa0 | (addr & 0x100) >> 7 );
        i2cSend( addr & 0xff );

        uint8_t page_offset = MIN( len, 0x10 - (len & 0xf) );

        for( uint8_t i = 0; i < page_offset; ++ i ) {
            sscanf( buffer + 9 + 2 * i, "%2hhx", & byte );
            printf( "%.4X <- %.2X\r\n", addr + i, byte );

            i2cSend( byte );
        }

        i2cStop();

        len -= page_offset;
        addr = (addr & ~0xf) + 0x10;
        uint8_t pages_num = len / 0x10;

        while( pages_num -- ) {
            _delay_ms( 3 );

            i2cStart();
            i2cSend( 0xa0 | (addr & 0x100) >> 7 );
            i2cSend( addr & 0xff );

            for( uint8_t i = 0; i < 0x10; ++ i, ++ addr ) {
                sscanf( buffer + 9 + 2 * (i + page_offset), "%2hhx", & byte );
                printf( "%.4X <- %.2X\r\n", addr, byte );

                i2cSend( byte );
            }

            i2cStop();

            len -= 0x10;
            page_offset += 0x10;
        }
    }
}

handle_t handlers[] = {
    { .name = "read",  .func = handle_read  },
    { .name = "write", .func = handle_write },
};

int
compare_with( const void *a, const void *b ) {
    char *str_a = (char *) a;
    char *str_b = ((handle_t *) b) -> name;

    return strcmp( str_a, str_b );
}

int compare( const void *a, const void *b ) {
    return compare_with( ((handle_t *)a) -> name, b );
}

void
setup( void ) {
    uart_init();
    i2cInit();

    qsort(
        handlers,
        sizeof( handlers ) / sizeof( handle_t ),
        sizeof( handle_t ),
        compare
    );
}

int
main( void ) {
    char buffer[ 0x100 ];
    handle_t *handler;

    for( setup() ;; ) {
        read_line( buffer, 0x100 );
        char *cmd = strtok( buffer, " " );

        handler = bsearch(
            cmd,
            handlers,
            sizeof( handlers ) / sizeof( handle_t ),
            sizeof( handle_t ),
            compare_with
        );

        if( handler != NULL ) {
            (handler -> func)();
        }
    }
}

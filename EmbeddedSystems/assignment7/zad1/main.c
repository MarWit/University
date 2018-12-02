#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>

// #include "../common/uart.h"
#include "../common/i2c.h"

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

typedef void (*func_t)(void);
typedef struct {
    char *name;
    func_t func;
} handle_t;

void
handle_read( void ) {
    char *addr_s = strtok( NULL, " " );

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
handle_write( void ) {
    char *addr_s = strtok( NULL, " " );
    char *value_s = strtok( NULL, " " );

    uint16_t addr = strtol( addr_s, NULL, 0 );
    uint8_t byte = strtol( value_s, NULL, 0 );

    i2cStart();
    i2cSend( 0xa0 | (addr & 0x100) >> 7 );
    i2cSend( addr & 0xff );
    i2cSend( byte );
    i2cStop();

    printf( "0x%.4X <- 0x%.2X\r\n", addr, byte );
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
    char buffer[ 256 ];
    handle_t *handler;

    for( setup() ;; ) {
        for( uint16_t i = 0; i < 0x100; ++i ) {
            buffer[ i ] = getchar();
            putchar( buffer[ i ] );

            if( buffer[ i ] == '\r' ) {
                buffer[ i ] = '\0';
                putchar( '\n' );
                break;
            }
        }

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>

// #include "../common/uart.h"
#include "../common/i2c.h"

#define BAUD (9600)
#define BASE_YEAR (1970)

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
handle_date( void ) {
    i2cStart();
    i2cSend( 0xd0 );
    i2cSend( 0x4 );

    i2cStart();
    i2cSend( 0xd1 );

    uint16_t day = i2cReadAck();
    uint16_t month = i2cReadAck();
    uint16_t year = i2cReadNoAck();

    i2cStop();

    year |= (month & 0x80);
    day = (day & 0xf) + 10 * ((day & 0x30) >> 4);
    month = (month & 0xf) + 10 * ((month & 0x10) >> 4);
    year = (year & 0xf) + 10 * ((year & 0x1f0) >> 4);
    year += BASE_YEAR;

    printf( "%.2d-%.2d-%.4d\r\n", day, month, year );
}

void
handle_time( void ) {
    i2cStart();
    i2cSend( 0xd0 );
    i2cSend( 0x0 );

    i2cStart();
    i2cSend( 0xd1 );

    uint16_t seconds = i2cReadAck();
    uint16_t minutes = i2cReadAck();
    uint16_t hours = i2cReadNoAck();

    i2cStop();

    seconds = (seconds & 0xf) + 10 * (seconds >> 4);
    minutes = (minutes & 0xf) + 10 * (minutes >> 4);
    hours = (hours & 0xf) + 10 * ((hours & 0x30) >> 4);

    printf( "%.2d:%.2d:%.2d\r\n", hours, minutes, seconds );
}

void
handle_set_date( void ) {
    char *date = strtok( NULL, " " );

    uint16_t day, month, year;
    sscanf( date, "%2d-%2d-%4d", & day, & month, & year );

    day = (day % 10) | (day / 10) << 4;
    month = (month % 10) | (month / 10) << 4;
    year -= BASE_YEAR;
    year = (year % 10) | (year / 10) << 4;
    month |= (year & 0x100) >> 1;

    i2cStart();
    i2cSend( 0xd0 );
    i2cSend( 0x4 );

    i2cSend( day );
    i2cSend( month );
    i2cSend( year );

    i2cStop();
}

void
handle_set_time( void ) {
    char *time = strtok( NULL, " " );

    uint16_t hours, minutes, seconds;
    sscanf( time, "%2d:%2d:%2d", & hours, & minutes, & seconds );

    hours = (hours % 10) | (hours / 10) << 4;
    minutes = (minutes % 10) | (minutes / 10) << 4;
    seconds = (seconds % 10) | (seconds / 10) << 4;

    i2cStart();
    i2cSend( 0xd0 );
    i2cSend( 0x0 );

    i2cSend( seconds );
    i2cSend( minutes );
    i2cSend( hours );

    i2cStop();
}

handle_t handlers[] = {
    { .name = "date",       .func = handle_date },
    { .name = "time",       .func = handle_time },
    { .name = "set_date",   .func = handle_set_date  },
    { .name = "set_time",   .func = handle_set_time  },
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

#include <avr/interrupt.h>
#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

volatile float TEMP = 30.0;
volatile float TEMP_NOW = 0.0;

ISR(ADC_vect) {
    TEMP_NOW = (1.1 * ((ADC) / 1024.) - 0.5) * 100.;

    if( TEMP_NOW > TEMP ) {
        PORTB = 0;
    }

    if( TEMP_NOW < TEMP - 0.5 ) {
        PORTB = _BV( 5 );
    }
}

void
handle_temp( void ) {
    printf( "Current temp: %f\r\n", TEMP_NOW );
    printf( "Temperature set: %f\r\n", TEMP );
}

void
handle_set_temp( void ) {
    char *temp_s = strtok( NULL, " " );

    float new_temp = strtod( temp_s, NULL );
    printf( "Set temperature to %f\r\n", new_temp );

    TEMP = new_temp;
}

handle_t handlers[] = {
    { .name = "set_temp", .func = handle_set_temp },
    { .name = "temp",  .func = handle_temp  },
};

int
compare_with( const void *a, const void *b ) {
    char *str_a = (char *) a;
    char *str_b = ((handle_t *) b) -> name;

    return strcmp( str_a, str_b );
}

void
setup( void ) {
    uart_init();

    ADMUX = _BV( REFS0 ) | _BV( REFS1 );
    ADCSRA = _BV( ADEN ) | _BV( ADPS2 ) | _BV( ADPS1 ) | _BV( ADPS0 ) | _BV( ADIE ) | _BV( ADATE );
    ADCSRB = 0;

    DDRB = _BV( 5 );

    sei();

    ADCSRA |= _BV( ADSC );
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

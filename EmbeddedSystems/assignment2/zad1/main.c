#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

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

int
main( void ) {
    uart_init();

    // ADMUX = _BV( REFS0 ) | _BV( REFS1 );
    // ADCSRA = _BV( ADEN ) | _BV( ADSC ) | _BV( ADPS0 ) | _BV( ADPS1 ) | _BV( ADPS2 );

    DDRB = _BV(5);
    DDRC = 0;

    uint8_t byte = 0;
    uint8_t cyclic[ 0xff ];

    static uint32_t seconds = 1;

    for( ;; ) {
        PORTB = cyclic[ byte ] ? _BV( 5 ) : 0;
        cyclic[ byte ] = !!( PINC & _BV( PC0 ) );

        if( ++ byte > 0xfe ) {
            byte = 0;
        }

        _delay_ms( 4 * seconds );
    }
}

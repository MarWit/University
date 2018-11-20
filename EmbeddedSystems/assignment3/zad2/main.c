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

void
setup( void ) {
    uart_init();

    ADMUX = _BV( REFS0 ) | _BV( MUX3 ) | _BV( MUX2 ) | _BV( MUX1 );
    ADCSRA = _BV( ADEN ) | _BV( ADPS0 ) | _BV( ADPS1 ) | _BV( ADPS2 );

    DDRB = _BV( 5 );
}

int
main( void ) {
    setup();

    for( ;; ) {
        ADCSRA |= _BV( ADSC );
        while( !!(ADCSRA & _BV( ADSC )) );

        uint8_t led = !!PORTB;

        printf( "%s %0.2fV\r\n", led ? "ON" : "OFF", (1.1 * 1024.) / ADC );

        PORTB = led ? 0 : _BV( 5 );
        _delay_ms( 500 );
    }

    return 0;
}

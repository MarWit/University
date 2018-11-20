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

    ADMUX = _BV( REFS0 );
    ADCSRA = _BV( ADEN ) | _BV( ADPS0 ) | _BV( ADPS1 ) | _BV( ADPS2 );

    DDRB = _BV( 5 );
}

int
main( void ) {
    setup();

    uint8_t scale[] = {
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 11, 11, 11, 11, 12, 12, 12, 12, 13, 13, 13, 13, 14, 14, 14, 15, 15, 15, 16, 16, 16, 17, 17, 17, 18, 18, 19, 19, 19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24, 25, 25, 26, 26, 27, 27, 28, 28, 29, 30, 30, 31, 32, 32, 33, 34, 34, 35, 36, 37, 37, 38, 39, 40, 41, 42, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 61, 62, 63, 65, 66, 67, 69, 70, 72, 73, 75, 76, 78, 80, 81, 83, 85, 87, 88, 90, 92, 94, 96, 98, 100, 102, 105, 107, 109, 111, 114, 116, 118, 121, 124, 126, 129, 131, 134, 137, 140, 143, 146, 149, 152, 155, 159, 162, 165, 169, 172, 176, 180, 184, 187, 191, 195, 200
    };

    for( ;; ) {
        ADCSRA |= _BV( ADSC );
        while( !!(ADCSRA & _BV( ADSC )) );

        uint8_t T = scale[ ADC >> 2 ];

        PORTB = _BV( 5 );
        for( uint32_t i = 0; i < T; ++i ) _delay_us( 5 );
        PORTB = 0;
        for( uint32_t i = 0; i < (scale[ 0xff ] - T); ++i ) _delay_us( 5 );
    }

    return 0;
}

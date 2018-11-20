#include <avr/io.h>
#include <stdio.h>
#include <stdlib.h>
#include <util/delay.h>

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
    // uart_init();

    DDRB = _BV( 5 ) | _BV( 1 );
    DDRD = _BV( 6 );

    TCCR1A = _BV( COM1A0 );
    TCCR1B = _BV( CS10 ) | _BV( WGM12 );

    OCR1A = 210;

}

int
main( void ) {
    setup();

    uint16_t counter = 0xff;
    uint8_t flag = 0;

    for( ;; counter -- ) {
        // printf( "%d\r\n", !!(PIND & _BV( 2 )) );

        if( !(PINB & _BV( 0 ) ) ) {
            flag = 2;
        }

        if( ! counter ) {
            OCR1A = OCR1A ? 0 : 210;
            PORTB = flag ? _BV( 5 ) : 0;
            PORTD = flag ? _BV( 6 ) : 0;

            counter = OCR1A ? 0xff : 0xffa;

            if( flag > 0 ) flag --;
        }
    }
}

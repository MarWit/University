#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <math.h>

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

    ADMUX = _BV( REFS0 ) | _BV( MUX1 );
    ADCSRA = _BV( ADEN ) | _BV( ADPS0 ) | _BV( ADPS1 ) | _BV( ADPS2 );
}

int
main( void ) {
    setup();

    float T0 = 298.15; // 25C

    // R = 5760, T = 295.15K = 22C

    float coeff = -0.7591;
    float beta = 5965;

    for( ;; ) {
        ADCSRA |= _BV( ADSC );
        while( !!(ADCSRA & _BV( ADSC )) );


        float temp = beta / (coeff + log((1024. - ADC) / ADC) + beta / T0) - 273.15;
        printf( "%.2f*C\r\n", temp );

        _delay_ms( 1000 );
    }

    return 0;
}

#include <avr/interrupt.h>
#include <avr/io.h>
#include <math.h>
#include <stdio.h>
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
    uart_init();

    ADMUX = _BV( REFS0 ) | _BV( MUX0 );
    ADCSRA = _BV( ADEN ) | _BV( ADPS0 ) | _BV( ADPS1 ) | _BV( ADPS2 );
    ADCSRA |= _BV( ADATE );
    ADCSRB = _BV( ADTS0 ) | _BV( ADTS1 );

    TCCR0A = _BV( WGM01 );
    TCCR0B = _BV( CS01 );
    TIMSK0 = _BV( OCIE0A );
    OCR0A = 249;

    sei();
}

float buffer[ 256 ];
volatile float squares = 0;
uint8_t iter;

ISR( TIMER0_COMPA_vect ) {
    squares -= buffer[ iter ];
    buffer[ iter ] = ((ADC) - 511.5) / 511.5;
    buffer[ iter ] *= buffer[ iter ];
    squares += buffer[ iter ++ ];
}

int
main( void ) {
    setup();

    for( ;; ) {
        float mean = sqrt( squares ) / 256.;
        float dbm = 20. * log10( mean / 0.055 );

        printf( "%f dBFS\r\n", dbm );
        _delay_ms( 50 );
    }

    return 0;
}

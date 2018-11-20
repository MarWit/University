#include <avr/interrupt.h>
#include <avr/io.h>
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

    EICRA = _BV( ISC01 ) | _BV( ISC00 );
    EIMSK = _BV( INT0 );

    ADMUX = _BV( REFS0 );
    ADCSRA = _BV( ADEN ) | _BV( ADPS0 ) | _BV( ADPS1 ) | _BV( ADPS2 ) | _BV( ADATE ) | _BV( ADIE );

    ADCSRB = _BV( ADTS1 );

    sei();
}

volatile float resistance = 0;

ISR( INT0_vect ) {}
ISR( ADC_vect ) {
    float v = 5.0 * (ADC) / 1024.;
    resistance = 10000. * (5.0 - v) / v;
}

int
main( void ) {
    setup();

    for( ;; ) {
        cli();
        printf( "%f\r\n", resistance );

        while( !(UCSR0A & _BV(TXC0) ) );

        sei();
        _delay_ms( 1000 );
    }

    return 0;
}
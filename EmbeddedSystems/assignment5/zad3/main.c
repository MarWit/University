#include <avr/io.h>
#include <avr/sleep.h>
#include <avr/interrupt.h>
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

    ADMUX  = _BV( REFS0 );
    ADMUX |= _BV( MUX3 ) | _BV( MUX2 ) | _BV( MUX1 );

    ADCSRA = _BV( ADEN ) | _BV( ADPS2 ) | _BV( ADPS1 ) | _BV( ADPS0 );
}

ISR( ADC_vect ) {
    printf( "%f\n", 1.1 * 1024. / (ADC) );

    while( !(UCSR0A & _BV(TXC0) ) );
    _delay_ms( 5 );
}

int
main( void ) {
    setup();

    uint8_t N = 250;

    for( uint8_t i = 0; i < N; ++ i ) {
        ADCSRA |= _BV( ADSC );
        while( !!(ADCSRA & _BV( ADSC )) );

        printf( "%f\n", 1.1 * 1024.0 / (ADC) );
    }

    printf( "\n" );
    while( !(UCSR0A & _BV(TXC0) ) );

    ADCSRA |= _BV( ADIE );
    set_sleep_mode( SLEEP_MODE_ADC );
    sei();

    for( uint8_t i = 0; i < N; ++ i ) {
        sleep_mode();
    }

    cli();

    printf( "\n" );
}

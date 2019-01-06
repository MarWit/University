#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>
#include <stdio.h>
#include <util/delay.h>
#include <util/atomic.h>

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

    TCCR0A = _BV( WGM01 );
    TCCR0B = _BV( CS02 );
    TIMSK0 = _BV( OCIE0A );

    OCR0A = 50;

    TCCR1A = _BV( COM1A1 );
    TCCR1B = _BV( WGM13 ) | _BV( CS11 ) | _BV( CS10 );
    TIMSK1 = _BV( OCIE1A ) | _BV( TOIE1 );

    DDRB = _BV( 1 );

    ICR1 = 0x400;
    OCR1A = 0x300;

    sei();
}

volatile double overflow, compare;

ISR( TIMER0_COMPA_vect ) {
    ADMUX &= ~_BV( MUX0 );
    ADCSRA |= _BV( ADSC );
    while( !!(ADCSRA & _BV( ADSC )) );

    OCR1A = ADC;

    ADMUX |= _BV( MUX0 );
}

ISR( TIMER1_OVF_vect ) {
    ADCSRA |= _BV( ADSC );
    while( !!(ADCSRA & _BV( ADSC )) );

    overflow = 5000. * (ADC) / 1024;
}

ISR( TIMER1_COMPA_vect ) {
    ADCSRA |= _BV( ADSC );
    while( !!(ADCSRA & _BV( ADSC )) );

    compare = 5000. * (ADC) / 1024;
}

int
main( void ) {
    for( setup();; ) {
        printf( "Compare: %fmV\r\n", compare );
        printf( "Overflow: %fmV\r\n", overflow );

        _delay_ms( 200 );
    }

    return 0;
}

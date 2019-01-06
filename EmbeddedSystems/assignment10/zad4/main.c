#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>
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

    ADMUX = _BV( REFS0 ) | _BV( ADLAR );
    ADCSRA = _BV( ADEN ) | _BV( ADPS0 ) | _BV( ADPS1 ) | _BV( ADPS2 );
    ADCSRA |= _BV( ADATE ) | _BV( ADIE ) | _BV( ADSC );

    TCCR1A = _BV( COM1B1 ) | _BV( WGM11 );
    TCCR1B = _BV( WGM12 ) | _BV( WGM13 ) | _BV( CS12 );

    // set_sleep_mode( SLEEP_MODE_IDLE );

    DDRB = _BV( 2 );
    ICR1 = 1249;
    OCR1B = 90;

    sei();
}

ISR( ADC_vect ) {
    // MIN=35
    // MAX=150
    // OCR1A = ADCH;
    OCR1B = 115. * (double)(0xff - ADCH) / 256. + 35.;
}

int
main( void ) {
    for( setup();; ) {
        char c = getchar();

        if( c == '+' ) {
            TCNT1 = 0;
            OCR1B ++;
            printf( "%d\r\n", OCR1B );
        } else if( c == '-' ) {
            TCNT1 = 0;
            OCR1B --;
            printf( "%d\r\n", OCR1B );
        }
    }

    return 0;
}

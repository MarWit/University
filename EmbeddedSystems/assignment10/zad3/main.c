#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>
#include <stdio.h>
#include <util/delay.h>

void
setup( void ) {
    ADMUX = _BV( REFS0 );
    ADCSRA = _BV( ADEN ) | _BV( ADPS0 ) | _BV( ADPS1 ) | _BV( ADPS2 );
    ADCSRA |= _BV( ADATE ) | _BV( ADIE ) | _BV( ADSC );

    TCCR0A = _BV( COM0A1 ) | _BV( COM0B1 ) | _BV( WGM01 ) | _BV( WGM00 );
    TCCR0B = _BV( CS01 ) | _BV( CS00 );

    set_sleep_mode( SLEEP_MODE_IDLE );

    DDRD = _BV( 5 ) | _BV( 6 );
    OCR0A = OCR0B = 0;

    sei();
}

ISR( ADC_vect ) {
    int16_t val = ADC - 511;

    if( val <= 0 ) {
        OCR0A = (-val) / 2;
        OCR0B = 0;
    } else {
        OCR0A = 0;
        OCR0B = (val - 1) / 2;
    }
}

int
main( void ) {
    for( setup();; ) sleep_mode();

    return 0;
}

#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>
#include <stdio.h>
#include <util/delay.h>

void
setup( void ) {
    ADMUX = _BV( REFS0 ) | _BV( ADLAR );
    ADCSRA = _BV( ADEN ) | _BV( ADPS0 ) | _BV( ADPS1 ) | _BV( ADPS2 );
    ADCSRA |= _BV( ADATE ) | _BV( ADIE ) | _BV( ADSC );

    TCCR1A = _BV( COM1A1 ) | _BV( WGM10 );
    TCCR1B = _BV( WGM12 ) | _BV( CS11 ) | _BV( CS10 );

    set_sleep_mode( SLEEP_MODE_IDLE );

    DDRB = _BV( 1 );

    sei();
}

ISR( ADC_vect ) {
    OCR1A = ADCH;
}

int
main( void ) {
    for( setup();; ) sleep_mode();

    return 0;
}

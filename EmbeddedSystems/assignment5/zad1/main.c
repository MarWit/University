#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>

void
setup( void ) {
    TCCR0A = _BV( WGM01 );
    TCCR0B = _BV( CS02 );
    TIMSK0 = _BV( OCIE0A );

    OCR0A = 243; // ~256Hz

    DDRB = _BV( 5 );

    sei();
}

volatile uint8_t counter = 0;
volatile uint8_t data[ 0x100 ] = { 0 };

ISR( TIMER0_COMPA_vect ) {
    PORTB = data[ counter ] ? _BV( 5 ) : 0;
    data[ counter ++ ] = !!( PIND & _BV( 2 ) );
}

int
main( void ) {
    setup();

    set_sleep_mode( SLEEP_MODE_IDLE );

    for( ;; ) {
        sleep_mode();
    }

    return 0;
}

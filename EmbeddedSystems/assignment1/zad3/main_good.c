#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

void
init( void ) {
    UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);

    DDRD = 0xFF;
}


int
main( void ) {
    init();

    PORTD = 0x7;
    uint8_t carry = 0;

    for( ;; ) {
        _delay_ms( 25 );

        carry = !!(PORTD & _BV(7));
        PORTD <<= 1;
        PORTD |= carry;
    }

    return 0;
}

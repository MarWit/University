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

    const uint8_t MAP[] = {
        0b11000000,
        0b11111001,
        0b10100100,
        0b10110000,
        0b10011001,
        0b10010010,
        0b10000010,
        0b11111000,
        0b10000000,
        0b10010000
    };

    uint8_t counter = 0;

    for( ;; ) {
        _delay_ms( 500 );

        PORTD = MAP[ counter ];
        if( ++ counter > 9 )
            counter = 0;
    }

    return 0;
}

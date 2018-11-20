#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

void
init( void ) {
    UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);

    DDRD = 0xFF;
    DDRC = 0xFF;
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

    uint8_t a, b, counter, now;

    a = b = 0;
    counter = 0;
    now = 0;

    PORTD = MAP[ 0 ];

    for( ;; ) {
        _delay_ms( 10 );

        now = !now;
        PORTC = _BV( now );
        PORTD = now ? MAP[ b ] : MAP[ a ];

        counter ++;

        if( counter >= 100 ) {
            counter = 0;

            if( ++ b > 9 ) {
                b = 0;
                if( ++ a > 5 ) {
                    a = 0;
                }
            }
        }
    }

    return 0;
}

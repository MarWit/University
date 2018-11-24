#include <avr/interrupt.h>
#include <avr/io.h>
#include <util/delay.h>


void
setup( void ) {
    SPCR = _BV( SPE ) | _BV( MSTR ) | _BV( SPI2X );

    DDRB = _BV( 1 ) | _BV( 2 ) | _BV( 3 ) | _BV( 5 );
}

void
spi_transmit( uint8_t byte ) {
    SPDR = byte;
    while( !( SPSR & _BV( SPIF ) ) );
}

int
main( void ) {
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

    setup();

    for( uint8_t i = 0;; i = i == 9 ? 0 : i + 1 ) {
        PORTB &= ~_BV( 1 );
        spi_transmit( ~ MAP[ i ] );
        PORTB |= _BV( 1 );

        _delay_ms( 1000 );
    }
}

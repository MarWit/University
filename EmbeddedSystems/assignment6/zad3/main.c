#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>
#include <avr/pgmspace.h>

#include "song.h"

volatile uint16_t song_pointer = 0;
volatile uint16_t song_stage = 0;

void
setup( void ) {
    SPCR = _BV( SPE ) | _BV( MSTR );
    SPSR = _BV( SPI2X );

    TCCR0A = _BV( WGM01 );
    TCCR0B = _BV( CS01 );
    TIMSK0 = _BV( OCIE0A );
    OCR0A = 249;

    DDRB = _BV( 2 ) | _BV( 3 ) | _BV( 5 );
    PORTB = _BV( 2 );

    set_sleep_mode( SLEEP_MODE_IDLE );
    sei();
}

void
spi_transmit( uint8_t byte ) {
    SPDR = byte;
    while( !( SPSR & _BV( SPIF ) ) );
}

ISR( TIMER0_COMPA_vect ) {
    if( song_pointer >= 32767 ) {
        song_pointer = 0;
        song_stage ++;
    }

    if( song_stage * 32767 + song_pointer >= SONG_LENGTH ) {
        song_pointer = 0;
        song_stage = 0;
    }

    PORTB &= ~_BV( PB2 );

    uint8_t byte = pgm_read_byte( (uint8_t *)pgm_read_word( & SONG_STAGES[ song_stage ] ) + song_pointer ++ );

    spi_transmit( 0b01110000 | byte >> 4 );
    spi_transmit( byte << 4 );

    PORTB |= _BV( PB2 );
}

int
main( void ) {
    for( setup();; ) sleep_mode();
}

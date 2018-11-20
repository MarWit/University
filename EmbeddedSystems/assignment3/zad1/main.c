#include <avr/io.h>
#include <avr/pgmspace.h>
#include <stdio.h>
#include <util/delay.h>

#include "song.h"

void
setup( void ) {
    UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);

    DDRD = 0xFF;
}

static inline void
_var_delay( uint16_t delay ) {
    while( delay -- != 0 ) _delay_us( 1 );
}

static inline void
_var_delay_ms( uint16_t delay ) {
    while( delay -- != 0 ) _delay_ms( 1 );
}

static inline void
play_tone( uint32_t period, uint32_t msecs ) {
    int32_t k = (msecs * 1000);

    while( k > 0 ) {
        PORTD = _BV( 2 );
        _var_delay( period / 10 );
        PORTD = 0;
        _var_delay( 9 * period / 10 );

        k -= period;
    }
}

int
main( void ) {
    setup();

    uint32_t song_length = sizeof( song ) / sizeof( uint16_t );

    const float time_divider = 1.2;

    for( ;; ) {
        for( uint32_t i = 0; i < song_length; ++ i ) {
            _var_delay_ms( pgm_read_word( & times[ 2 * i ] ) / time_divider );
            play_tone( pgm_read_word( & song[ i ] ), pgm_read_word( & times[ 2 * i + 1 ] ) / time_divider );
        }
    }

    return 0;
}

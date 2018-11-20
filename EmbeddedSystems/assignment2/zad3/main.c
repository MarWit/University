#include <avr/io.h>

#define PRESSED(KEY) (!!(PINC & _BV(KEY)))

void
setup( void ) {
    UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);

    DDRD = 0xff;
    DDRC = 0;
}


int
main( void ) {
    setup();

    uint8_t num = 0;
    uint8_t buttons[ 3 ] = {0, 0, 0};

    PORTD = 0;
    PORTC = 0;

    for( ;; ) {
        if( PRESSED( PC0 ) ^ buttons[ 0 ] ) {
            buttons[ 0 ] = !buttons[ 0 ];
            if( buttons[ 0 ] ) {
                num = 0;
                PORTD = 0;
            }
        }

        if( PRESSED( PC1 ) ^ buttons[ 1 ] ) {
            buttons[ 1 ] = !buttons[ 1 ];
            if( buttons[ 1 ] ) {
                PORTD = --num ^ (num >> 1);
            }
        }

        if( PRESSED( PC2 ) ^ buttons[ 2 ] ) {
            buttons[ 2 ] = !buttons[ 2 ];
            if( buttons[ 2 ] ) {
                PORTD = ++num ^ (num >> 1);
            }
        }
    }
}

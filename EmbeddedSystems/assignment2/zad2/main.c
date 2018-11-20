#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

#define PRESSED (!!(PINC & _BV(PC0)))
#define BAUD (9600)

int
uart_getc( FILE *s ) {
    while( !(UCSR0A & _BV(RXC0) ) );

    return UDR0;
}

int
uart_putc( char c, FILE *s ) {
    while( ! ( UCSR0A & _BV(UDRE0) ) );
    UDR0 = c;

    return 0;
}

static inline void
uart_init( void ) {
    UBRR0 = (F_CPU / 16 / BAUD - 1);
    UCSR0A = 0;
    UCSR0B = _BV(RXEN0) | _BV(TXEN0);
    UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);

    static FILE fd;
    fdev_setup_stream( & fd, uart_putc, uart_getc, _FDEV_SETUP_RW );
    stdin = stdout = stderr = & fd;
}

char
decode_morse( uint8_t code ) {
    const uint8_t MORSE_MAPPING[] = {
        0xBF, 0xBE, 0xBC, 0xB8, 0xB0, 0xA0, 0xA1, 0xA3, 0xA7,
        0xAF, 0x42, 0x81, 0x85, 0x61, 0x20, 0x84, 0x63, 0x80,
        0x40, 0x8E, 0x65, 0x82, 0x43, 0x41, 0x67, 0x86, 0x8B,
        0x62, 0x60, 0x21, 0x64, 0x88, 0x66, 0x89, 0x8D, 0x83
    };

    uint8_t i = 0;
    for( ; i < sizeof( MORSE_MAPPING ); ++ i ) {
        if( code == MORSE_MAPPING[ i ] )
            break;
    }

    if( i >= sizeof( MORSE_MAPPING ) ) return '?';
    if( i >= 0 && i <= 9 ) return '0' + i;

    return ('A' + i) - 10;
}

int
main( void ) {
    uart_init();

    uint8_t ticks = 0;
    uint8_t state = 0;

    uint8_t resolution = 3;

    uint8_t num = 0;
    uint8_t code = 0;

    for( ;; ) {
        switch( state ) {
            case 0:
                num = code = 0;

                if( PRESSED ) {
                    ticks ++;
                    state ++;
                }

                break;

            case 1:
                if( PRESSED ) {
                    ticks ++;
                    if( ticks >= 3 * resolution )
                        PORTB = _BV( 5 );
                } else {
                    state = 2;
                    PORTB = 0;

                    if( ticks >= 3 * resolution )
                        // putchar( '-' );
                        code |= _BV( num );
                    // else
                        // putchar( '.' );

                    num ++;
                    ticks = 0;
                }

                break;

            case 2:
                if( PRESSED ) {
                    if( ticks >= 1 * resolution && ticks < 3 * resolution ) {
                        state = 1; ticks = 0;
                        break;
                    }
                } else {
                    ticks ++;
                }

                if( ticks >= 3 * resolution ) {
                    code |= num << 5;
                    putchar( decode_morse( code ) );

                    ticks = 0;
                    state = 3;
                }

                break;

            case 3:
                if( PRESSED ) {
                    ticks = state = 0;
                } else {
                    ticks ++;
                }

                if( ticks >= 7 * resolution ) {
                    putchar( ' ' );
                    ticks = state = 0;
                }
        }

        _delay_ms( 100 );
    }
}

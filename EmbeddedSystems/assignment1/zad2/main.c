#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

#define BAUD (9600)
#define DOT_TIME (200)

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

static inline void
init( void ) {
    uart_init();

    DDRB = _BV(PB5);
}

static inline void
blink( double t ) {
    PORTB = _BV(PB5);
    _delay_ms( t );
    PORTB = 0;
}

int
main( void ) {
    init();

    const uint8_t MORSE_MAPPING[] = {
        0xBF, 0xBE, 0xBC, 0xB8, 0xB0, 0xA0, 0xA1, 0xA3, 0xA7,
        0xAF, 0x42, 0x81, 0x85, 0x61, 0x20, 0x84, 0x63, 0x80,
        0x40, 0x8E, 0x65, 0x82, 0x43, 0x41, 0x67, 0x86, 0x8B,
        0x62, 0x60, 0x21, 0x64, 0x88, 0x66, 0x89, 0x8D, 0x83
    };

    char c;
    uint8_t len;

    uint8_t n = 0;
    char buffer[ 0xFF ];

    for( ;; ) {
        n = 0;

        while( (c = getchar()) != '\r' && n < 0xFF ) {
            putchar( c );
            buffer[ n ++ ] = c;
        }

        printf( "\r\n" );

        for( uint8_t i = 0; i < n; ++ i ) {
            c = buffer[ i ];

            switch( c ) {
                case ' ':
                    _delay_ms( 7 * DOT_TIME );
                    continue;

                case '0' ... '9':
                    c -= '0';
                    break;

                case 'A' ... 'Z':
                    c =  c + 10 - 'A';
                    break;

                case 'a' ... 'z':
                    c = c + 10 - 'a';
                    break;

                default:
                    continue;
            }

            c = MORSE_MAPPING[ c ];

            len = (c & 0xE0) >> 5;
            while( len -- ) {
                if( c & 1 ) blink( DOT_TIME * 3 );
                else blink( DOT_TIME );

                c >>= 1;

                _delay_ms( DOT_TIME );
            }

            _delay_ms( DOT_TIME * 2 );
        }

        printf( "READY\r\n" );
    }

    return 0;
}

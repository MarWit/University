#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

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

void
setup( void ) {
    uart_init();

    TCCR1B = _BV( CS10 );
}

#define MKVARS(TYPE, A, B) \
    volatile TYPE TYPE##_a = A; \
    volatile TYPE TYPE##_b = B; \
    __attribute__((unused)) volatile TYPE TYPE##_c

#define MKOP(TYPE, OP) \
    TYPE##_c = TYPE##_a OP TYPE##_b

#define BENCHOP(TYPE, OP, TIME) \
    TIME = TCNT1; \
    MKOP(TYPE, OP); \
    TIME = TCNT1 - TIME; \
    printf( "type: " #TYPE ", operation: " #OP ", cycles: %d\r\n", TIME )

#define BENCHTYPE(TYPE, A, B, TIME) \
    MKVARS( TYPE, A, B ); \
    BENCHOP( TYPE, +, TIME ); \
    BENCHOP( TYPE, -, TIME ); \
    BENCHOP( TYPE, *, TIME ); \
    BENCHOP( TYPE, /, TIME )

int
main( void ) {
    setup();

    uint16_t t;

    for( ;; ) {
        BENCHTYPE( int8_t, 16, 77, t );
        BENCHTYPE( int16_t, 234, 654, t );
        BENCHTYPE( int32_t, 23454, 34654, t );
        BENCHTYPE( int64_t, 12345, 65432, t );
        BENCHTYPE( float, 23.54, 76.12, t );

        printf( "\r\n" );
        _delay_ms( 5000 );
    }
}

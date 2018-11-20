#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

#include <stdint.h>

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

#define NOP __asm__( "nop\nnop\nnop" );

#define FOO(TYPE, FMT) \
    TYPE TYPE##_a, TYPE##_b, TYPE##_c; \
    scanf( FMT " " FMT, & TYPE##_a, & TYPE##_b ); \
    NOP; TYPE##_c = TYPE##_a + TYPE##_b; NOP; NOP; printf( FMT " + " FMT " = " FMT "\r\n", TYPE##_a, TYPE##_b, TYPE##_c ); \
    NOP; TYPE##_c = TYPE##_a - TYPE##_b; NOP; NOP; printf( FMT " - " FMT " = " FMT "\r\n", TYPE##_a, TYPE##_b, TYPE##_c ); \
    NOP; TYPE##_c = TYPE##_a * TYPE##_b; NOP; NOP; printf( FMT " * " FMT " = " FMT "\r\n", TYPE##_a, TYPE##_b, TYPE##_c ); \
    NOP; TYPE##_c = TYPE##_a / TYPE##_b; NOP; NOP; printf( FMT " / " FMT " = " FMT "\r\n", TYPE##_a, TYPE##_b, TYPE##_c )

int
main( void ) {
    uart_init();

    FOO(int8_t, "%hhd");
    FOO(int16_t, "%d");
    FOO(int32_t, "%ld");
    FOO(int64_t, "%ld");
    FOO(float, "%f");

    return 0;
}

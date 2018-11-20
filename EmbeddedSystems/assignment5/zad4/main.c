#include <avr/io.h>
#include <avr/sleep.h>
#include <avr/interrupt.h>
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

    TCCR1B = _BV( CS12 ) | _BV( ICES1 );
    TIMSK1 = _BV( ICIE1 );

    sei();
}

#define N (0x100)

volatile uint16_t counter = 0;
volatile float mean = 0.0;
volatile uint16_t last = 0;

ISR( TIMER1_CAPT_vect ) {
    volatile uint16_t now = (ICR1);

    if( counter >= N ) {
        counter = 0;
        mean = 0.0;
        last = 0;
        return;
    }

    if( ! last ) {
        last = now;
        return;
    }

    last = now - last;
    mean += (float) last / (float) N;
    last = now;

    if( ++ counter >= N ) {
        printf( "%.2fHz\r\n", (16e6 / 256.) / mean );
        while( !(UCSR0A & _BV(TXC0) ) );
    }
}

int
main( void ) {
    setup();
    set_sleep_mode( SLEEP_MODE_IDLE );

    for( ;; ) {
        sleep_mode();
    }
}

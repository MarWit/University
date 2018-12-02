#include <avr/interrupt.h>
#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

#define SS _BV(4)
#define MOSI _BV(5)
#define MISO _BV(6)
#define SCK _BV(7)

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
    SPCR = _BV( SPE );

    DDRB = _BV( 4 );
    DDRD = SS | MOSI | SCK;

    uart_init();
}

void
spi_send( uint8_t byte ) {
    SPDR = byte;
}

uint8_t
spi_read( void ) {
    return SPDR;
}

uint8_t
sspi_transmit( uint8_t byte, uint8_t reset ) {
    uint8_t i = 7;
    uint8_t out = 0;

    if( reset ) {
        PORTD = SS;
        _delay_us( 25 );
    }

    do {
        PORTD = !!(byte & _BV(i)) ? MOSI : 0;
        PORTD |= SCK;

        _delay_us( 25 );
        out |= (!!(PIND & MISO)) << i;
        PORTD &= ~SCK;

        _delay_us( 25 );
    } while( i -- );

    return out;
}

int
main( void ) {
    setup();

    spi_send( 0 );

    for( ;; ) {
        uint8_t byte = sspi_transmit( 0xff, 0 );
        sspi_transmit( byte + 1, 1 );

        printf( "slave sent: 0x%.2x\r\n", byte );

        byte = spi_read();
        spi_send( byte + 1 );

        printf( "master sent: 0x%.2x\r\n", byte );

        _delay_ms( 500 );
    }
}

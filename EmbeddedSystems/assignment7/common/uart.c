#include <avr/interrupt.h>
#include <avr/io.h>
#include <util/atomic.h>
#include <stdio.h>

#define BAUD (9600)

typedef struct {
    uint16_t p;
    uint16_t l;
    uint8_t buffer[ 0x100 ];
} cyclic_t;

cyclic_t receive;
cyclic_t send;
uint8_t all_sent = 1;

void
cyclic_write( cyclic_t *cyclic, char c, uint8_t *again ) {
    *again = 0;

    ATOMIC_BLOCK( ATOMIC_RESTORESTATE ) {
        if( cyclic -> l == 0x100 ) {
            *again = 1;
            return;
        }

        cyclic -> buffer[ (cyclic -> p + cyclic -> l ++) & 0xff ] = c;
    }
}

char
cyclic_read( cyclic_t *cyclic, uint8_t *again ) {
    char c;
    *again = 0;

    ATOMIC_BLOCK( ATOMIC_RESTORESTATE ) {
        if( ! cyclic -> l ) {
            *again = 1;
            return 0;
        }


        c = cyclic -> buffer[ cyclic -> p ++ ];
        cyclic -> p &= 0xff;
        cyclic -> l --;
    }

    return c;
}

int
uart_getc( FILE *s ) {
    uint8_t again;
    char r;

    do {
        r = cyclic_read( & receive, & again );
    } while( again );

    return r;
}

int
uart_putc( char c, FILE *s ) {
    uint8_t again;

    if( all_sent ) {
        UDR0 = c;
    } else {
        do {
            cyclic_write( & send, c, & again );
        } while( again );
    }

    all_sent = 0;

    return 0;
}

ISR(USART_RX_vect) {
    uint8_t again;
    cyclic_write( & receive, UDR0, & again );
}

ISR(USART_TX_vect) {
    all_sent = 1;
}

ISR(USART_UDRE_vect) {
    uint8_t again;

    uint8_t c = cyclic_read( & send, & again );
    if( ! again ) {
        UDR0 = c;
    }
}

void
uart_init( void ) {
    UBRR0 = (F_CPU / 16 / BAUD - 1);
    UCSR0A = 0;
    UCSR0B = _BV(RXEN0) | _BV(TXEN0) | _BV(RXCIE0) | _BV(TXCIE0) | _BV(UDRIE0);
    UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);

    static FILE fd;
    fdev_setup_stream( & fd, uart_putc, uart_getc, _FDEV_SETUP_RW );
    stdin = stdout = stderr = & fd;

    sei();
}

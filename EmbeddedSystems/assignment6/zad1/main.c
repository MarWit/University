#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>
#include <stdio.h>
#include <util/delay.h>

#define BAUD (9600)

static inline void
uart_init( void ) {
    UBRR0 = (F_CPU / 16 / BAUD - 1);
    UCSR0A = 0;
    UCSR0B = _BV(RXEN0) | _BV(TXEN0) | _BV(RXCIE0);
    UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
}

void
setup( void ) {
    uart_init();
    set_sleep_mode( SLEEP_MODE_IDLE );

    sei();
}

ISR(USART_RX_vect) {
    volatile uint16_t ret = UDR0;
    UDR0 = ret;
}

int
main( void ) {
    for( setup() ;; ) sleep_mode();
}

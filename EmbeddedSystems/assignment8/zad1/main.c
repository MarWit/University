#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>
#include <stdio.h>
#include <string.h>

#include "../common/hd44780.h"

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
    LCD_Initialize();

    LCD_WriteCommand( HD44780_DISPLAY_ONOFF | HD44780_DISPLAY_ON | HD44780_CURSOR_ON );
}


int
main( void ) {
    char text[ 17 ] = { '\0' };
    uint8_t i = 0;

    for( setup();; ) {
        char c = getchar();

        if( i > 15 ) {
            i = 0;
            LCD_GoTo( 0, 0 );
        }

        if( c == '\r' ) {
            LCD_Clear();
            LCD_GoTo( 0, 1 );
            LCD_WriteText( text );
            LCD_GoTo( 0, 0 );

            memset( text, 0, sizeof( text ) );
            i = 0;
        } else {
            text[ i ++ ] = c;
            LCD_WriteData( c );
        }
    }
}

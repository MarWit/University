#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>
#include <stdio.h>
#include <string.h>

#include "../common/hd44780.h"

void
setup( void ) {
    LCD_Initialize();

    char c = 0;

    for( uint8_t i = 0; i < 6; ++ i ) {
        LCD_WriteCommand( 0x40 + i * 8 );

        for( uint8_t j = 0; j < 8; ++ j ) {
            LCD_WriteData( c );
        }

        c |= (1 << (4 - i));
    }

    LCD_Home();
}


int
main( void ) {
    for( setup();; ) {
        LCD_Clear();
        _delay_ms( 20 );

        for( uint8_t j = 0; j < 17; ++ j ) {
            for( uint8_t i = 0; i < 6; ++ i ) {
                LCD_GoTo( j, 0 );
                LCD_WriteData( i );
                LCD_GoTo( j, 1 );
                LCD_WriteData( i );

                _delay_ms( 20 );
            }
        }
    }
}

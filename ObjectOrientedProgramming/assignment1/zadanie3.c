#include "zespolone.h"

int
main( void )
{
    Zespolone z1 = { .re = 8.0f, .im = 3.14f };
    Zespolone z2 = { .re = 6.76f, .im = 8.34f };

    printf( "Dodawanie: \n" );
    zespolonePrint( & z1 ); printf( " + " );
    zespolonePrint( & z2 ); printf( " = " );
    zespoloneAddition( & z1, & z2 );
    zespolonePrint( & z2 ); printf( "\n\n" );

    Zespolone * z = NULL;

    printf( "Odejmowanie: \n" );
    zespolonePrint( & z1 ); printf( " - " );
    zespolonePrint( & z2 ); printf( " = " );
    z = zespoloneSubtractionR( & z1, & z2 );
    zespolonePrint( z ); printf( "\n\n" );

    printf( "Mnożenie: \n" );
    zespolonePrint( & z1 ); printf( " * " );
    zespolonePrint( z ); printf( " = " );
    zespoloneMultiplication( & z1, z );
    zespolonePrint( z ); printf( "\n\n" );

    zespoloneFree( z );

    printf( "Dzielenie: \n" );
    zespolonePrint( & z1 ); printf( " / " );
    zespolonePrint( & z2 ); printf( " = " );
    z = zespoloneDivisionR( & z1, & z2 );
    zespolonePrint( z ); printf( "\n" );

    zespoloneFree( z );

    return 0;
}

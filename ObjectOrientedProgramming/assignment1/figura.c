#include "figura.h"

Figura *
constructorFigura( uint8_t type, float x, float y, float len ) {
    Figura * f = malloc( sizeof( Figura ) );

    f -> type = type;
    f -> x = x;
    f -> y = y;
    f -> len = len;

    return f;
}

void
deconstructorFigura( Figura * f ) {
    if( f != NULL )
        free( f );

    f = NULL;
}

Figura *
constructorPoint( float x, float y ) {
    return constructorFigura( fig_point, x, y, 0.0 );
}

Figura *
constructorCircle( float x, float y, float radius ) {
    return constructorFigura( fig_circle, x, y, radius );
}

Figura *
constructorSquare( float x, float y, float length ) {
    return constructorFigura( fig_square, x, y, length );
}

void
narysuj( Figura * f ) {
    switch( f -> type ) {
        case fig_point:
            printf( "Rysuje punkt o współrzędnych [%f, %f] .\n", f -> x, f -> y );
            break;
        case fig_circle:
            printf( "Rysuje koło o środku w [%f, %f] i r = %f .\n",
                    f -> x + f -> len, f -> y + f -> len,
                    f -> len
            );
            break;
        case fig_square:
            printf( "Rysuje kwadrat o współrzędnych górnego, lewego rogu w [%f, %f]"
                    " oraz długości boku a = %f.\n",
                    f -> x, f -> y, f -> len
            );
            break;
        default:
            printf( "Błędna figura.\n" );
            break;
    }
}

void
przesun( Figura * f, float x, float y ) {
    f -> x += x;
    f -> y += y;
}

int
zawiera( Figura * f, float x, float y ) {
    switch( f -> type ) {
        case fig_point:
            return f -> x == x && f -> y == y;
        case fig_circle:
            return POW2( x - ( f -> x + f -> len ) ) +
                   POW2( y - ( f -> y + f -> len ) ) <=
                   POW2( f -> len );
        case fig_square:
            return ( x >= f -> x && x <= f -> x + f -> len &&
                     y >= f -> y && y <= f -> y + f -> len );
        default:
            return -1;

    }

    return -1;
}

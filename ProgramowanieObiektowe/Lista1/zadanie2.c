#include "figura.h"

int
main( void ) {
    Figura * punkt      = constructorPoint( 5.4f, 4.5f );
    Figura * kolo       = constructorCircle( 0.0f, 0.0f, 5.0f );
    Figura * kwadrat    = constructorSquare( 5.0f, 5.0f, 5.0f );

    narysuj( punkt );
    narysuj( kolo );
    narysuj( kwadrat );

    printf( "Punkt [%f, %f] %szawiera się w kole.\n",
            punkt -> x, punkt -> y,
            zawiera( kolo, punkt -> x, punkt -> y ) ? "" : "nie "
    );

    przesun( punkt, 10.0f, 10.0f );

    printf( "Punkt [%f, %f] %szawiera się w kole.\n",
            punkt -> x, punkt -> y,
            zawiera( kolo, punkt -> x, punkt -> y ) ? "" : "nie "
    );

    deconstructorPoint( punkt );
    deconstructorCircle( kolo );
    deconstructorSquare( kwadrat );

    return 0;
}

#include <stdio.h>
#include <stdlib.h>

typedef struct {
    float re;
    float im;
} Zespolone;

Zespolone *
zespoloneCreate( ) {
    Zespolone * z = malloc( sizeof( Zespolone ) );
    return z;
}

void
zespoloneFree( Zespolone * z ) {
    if( z != NULL )
        free( z );

    z = NULL;
}

void
zespoloneAddition( Zespolone * z1, Zespolone * z2 ) {
    z2 -> re += z1 -> re;
    z2 -> im += z1 -> im;
}

Zespolone *
zespoloneAdditionR( Zespolone * z1, Zespolone * z2 ) {
    Zespolone * z = zespoloneCreate( );

    z -> re = z1 -> re + z2 -> re;
    z -> im = z1 -> im + z2 -> im;

    return z;
}

void
zespoloneSubtraction( Zespolone * z1, Zespolone * z2 ) {
    z2 -> re -= z1 -> re;
    z2 -> im -= z1 -> im;
}

Zespolone *
zespoloneSubtractionR( Zespolone * z1, Zespolone * z2 ) {
    Zespolone * z = zespoloneCreate( );

    z -> re = z1 -> re - z2 -> re;
    z -> im = z1 -> im - z2 -> im;

    return z;
}

void
zespoloneMultiplication( Zespolone * z1, Zespolone * z2 ) {
    float buf = ( z1 -> re * z2 -> re ) - ( z1 -> im * z2 -> im );

    z2 -> im = ( z1 -> re * z2 -> im ) + ( z1 -> im * z2 -> re );
    z2 -> re = buf;
}

Zespolone *
zespoloneMultiplicationR( Zespolone * z1, Zespolone * z2 ) {
    Zespolone * z = zespoloneCreate( );

    z -> re = ( z1 -> re * z2 -> re ) - ( z1 -> im * z2 -> im );
    z -> im = ( z1 -> re * z2 -> im ) + ( z1 -> im * z2 -> re );

    return z;
}

void
zespoloneDivision( Zespolone * z1, Zespolone * z2 ) {
    float d = z2 -> re * z2 -> re + z2 -> im * z2 -> im;
    float t = ( z1 -> re * z2 -> re + z1 -> im * z2 -> im ) / d;

    z2 -> im = ( ( z1 -> im * z2 -> re ) -
                 ( z1 -> re * z2 -> im ) ) / d;

    z2 -> re = t;
}

Zespolone *
zespoloneDivisionR( Zespolone * z1, Zespolone * z2 ) {
    Zespolone * z = zespoloneCreate( );

    float d = z2 -> re * z2 -> re + z2 -> im * z2 -> im;

    z -> re = ( ( z1 -> re * z2 -> re ) +
                ( z1 -> im * z2 -> im ) ) / d;

    z -> im = ( ( z1 -> im * z2 -> re ) -
                ( z1 -> re * z2 -> im ) ) / d;

    return z;
}

void
zespolonePrint( Zespolone * z ) {
    printf( "(%.2f%s%.2fi)", z -> re, z -> im < 0.0 ? "" : "+", z -> im );
}

#ifndef _FIGURA_H_
#define _FIGURA_H_

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

// Poor man x^2
#define POW2(A) ((A)*(A))

// Cause I'm lazy
#define deconstructorPoint(A) deconstructorFigura(A)
#define deconstructorCircle(A) deconstructorFigura(A)
#define deconstructorSquare(A) deconstructorFigura(A)

enum typfig {
    fig_point,
    fig_circle,
    fig_square
};

typedef struct {
    uint8_t type;
    float len;

    float x;
    float y;
} Figura;


Figura * constructorFigura( uint8_t type, float x, float y, float len );
void deconstructorFigura( Figura * f );
Figura * constructorPoint( float x, float y );
Figura * constructorCircle( float x, float y, float radius );
Figura * constructorSquare( float x, float y, float length );
void narysuj( Figura * f );
void przesun( Figura * f, float x, float y );
int zawiera( Figura * f, float x, float y );

#endif

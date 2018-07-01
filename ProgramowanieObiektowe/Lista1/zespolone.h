#ifndef _ZESPOLONE_H_
#define _ZESPOLONE_H_

#include <stdio.h>
#include <stdlib.h>

typedef struct {
    float re;
    float im;
} Zespolone;

Zespolone * zespoloneCreate( );
void zespoloneFree( Zespolone * z );
void zespoloneAddition( Zespolone * z1, Zespolone * z2 );
Zespolone * zespoloneAdditionR( Zespolone * z1, Zespolone * z2 );
void zespoloneSubtraction( Zespolone * z1, Zespolone * z2 );
Zespolone * zespoloneSubtractionR( Zespolone * z1, Zespolone * z2 );
void zespoloneMultiplication( Zespolone * z1, Zespolone * z2 );
Zespolone * zespoloneMultiplicationR( Zespolone * z1, Zespolone * z2 );
void zespoloneDivision( Zespolone * z1, Zespolone * z2 );
Zespolone * zespoloneDivisionR( Zespolone * z1, Zespolone * z2 );
void zespolonePrint( Zespolone * z );

#endif

#include <string.h>

int
strdrop( char *str, const char *set ) {
    int slen  = strlen( str );
    int selen = strlen( set );

    int i, j, k;

    for( i = 0, j = 0; i < slen; i ++ ) {
        for( k = 0; k < selen; k ++ ) {
            if( set[ k ] == str[ i ] ) {
                k = -1;
                break;
            }
        }

        if( k != -1 )
            str[ j ++ ] = str[ i ];
    }

    str[ j ] = '\0';
    return j;
}

int
strcnt( const char *str, const char *set ) {
    int slen  = strlen( str );
    int selen = strlen( set );

    int i, j, c;

    for( i = 0, c = 0; i < slen; i ++ ) {
        for( j = 0; j < selen; j ++ ) {
            if( set[ j ] == str[ i ] ) {
                c ++;
                break;
            }
        }
    }

    return c;
}

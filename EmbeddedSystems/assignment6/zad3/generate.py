#!/usr/bin/env python

with open( 'darude_small.raw', 'rb' ) as f:
    data = f.read()

print( 'uint16_t SONG_LENGTH = {};'.format( len( data ) ) )

for i in range( (len( data ) // 32767) + 1 ):
    print( 'const uint8_t SONG_DATA{}[] PROGMEM = {{'.format( i ) )

    for sample in data[ 32767 * i : 32767 * (i+1) ]:
        print( '\t0x{:02X},'.format( sample ) )

    print( '};' )

print( 'const uint8_t* const SONG_STAGES[] PROGMEM = {' )

for i in range( (len( data ) // 32767) + 1 ):
      print( '\tSONG_DATA{},'.format( i ) )

print( '};' )

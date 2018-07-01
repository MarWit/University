#!/usr/bin/env python

import sys
import re
import subprocess

program = './' + sys.argv[ 1 ]

N = 25
T = 20
V = int( sys.argv[ 2 ] )

rout = re.compile( r'Time elapsed: ([0-9]+.[0-9]+)' )

print( '#N, TIME (in ms.)' )

for i in range( 8, 1 + N ):
    val = 0.0
    print( i, file=sys.stderr )

    for r in range( 20 ):
        out = subprocess.check_output( [ program, '-n', str( i ), '-t', str( T ), '-s', str(20) ] )
        out = rout.search( out.decode( 'utf-8' ) ).group( 1 )

        val += float( out )

    val /= 20

    print( '{}, {:f}'.format( ((1 << i) * 4 ) // 1024, val * 1000 ) )

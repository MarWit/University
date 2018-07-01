#!/usr/bin/env python2

from itertools import chain

def B(i,j):
    return 'B_%d_%d' % (i,j)

def storms(raws, cols, triples):
    writeln(':- use_module(library(clpfd)).')

    R = len(raws)
    C = len(cols)

    bs = [ B(i,j) for i in range(R) for j in range(C)]

    writeln('solve([' + ', '.join(bs) + ']) :- ')

    raws_constrain = [ ' + '.join( [ B(i, j) for j in range(C) ] ) + ' #= ' + str( raws[ i ] ) for i in range(R) ]
    cols_constrain = [ ' + '.join( [ B(i, j) for i in range(R) ] ) + ' #= ' + str( cols[ j ] ) for j in range(C) ]

    for constrain in chain(raws_constrain, cols_constrain):
        writeln(constrain + ',')

    # Wyklad 5
    storms_2x2 = [
        [0,0,0,0], [1,1,1,1], [1,0,1,0], [0,1,0,1],
        [1,0,0,0], [0,1,0,0], [0,0,1,0], [0,0,0,1],
        [1,1,0,0], [0,0,1,1]
    ]

    storms_3x = [
        [0,0,0], [1,0,0], [0,0,1],
        [1,1,1], [1,1,0], [0,1,1],
        [1,0,1]
    ]

    constrains_2x2 = [ [B(i, j), B(i+1,j), B(i, j+1), B(i+1,j+1)] for i in range(R-1) for j in range(C-1) ]
    constrains_3x_1 = [ [B(i, j), B(i+1,j), B(i+2, j)] for i in range(R-2) for j in range(C) ]
    constrains_3x_2 = [ [B(i, j), B(i,j+1), B(i, j+2)] for i in range(R) for j in range(C-2) ]

    writeln( 'tuples_in({},{}),'.format( str( constrains_2x2 ).replace("'", "" ), storms_2x2 ) )
    writeln( 'tuples_in({},{}),'.format( str (constrains_3x_1 ).replace("'", ""), storms_3x ) )
    writeln( 'tuples_in({},{}),'.format( str (constrains_3x_2 ).replace("'", ""), storms_3x ) )

    for v in triples:
        writeln( '{} #= {},'.format( B(v[0], v[1]), v[2] ) )

    writeln('    labeling([ff], [' +  ', '.join(bs) + ']).' )
    writeln('')
    writeln(":- tell('prolog_result.txt'), solve(X), write(X), nl, told.")

def writeln(s):
    output.write(s + '\n')

txt = open('zad_input.txt').readlines()
output = open('zad_output.txt', 'w')

raws = map(int, txt[0].split())
cols = map(int, txt[1].split())
triples = []

for i in range(2, len(txt)):
    if txt[i].strip():
        triples.append(map(int, txt[i].split()))

storms(raws, cols, triples)


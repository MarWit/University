#!/usr/bin/env python

from copy import deepcopy
from collections import deque

ch2c = lambda c: (ord(c[0]) - 97, int(c[1]) - 1)
c2ch = lambda x, y: chr(x+97) + str(y + 1)

class Pawn( object ):
    x, y = 0, 0

    def __init__( self, x, y ):
        self.x, self.y = x, y

    def __str__( self ):
        return "{} @ {}".format( self.__class__.__name__, c2ch( self.x, self.y ) )

    def __repr__( self ):
        return self.__str__()

class King( Pawn ):
    def can_hit( self, x, y ):
        return abs(self.x - x) <= 1 and abs(self.y - y) <= 1

    def moves( self ):
        l = []
        for x in range(-1,2):
            for y in range(-1,2):
                if self.x + x < 8 and self.x + x >= 0 and self.y + y < 8 and self.y + y >= 0 and (x != 0 or y != 0):
                    l.append( (self.x + x, self.y + y) )

        return l

class Tower( Pawn ):
    def can_hit( self, x, y ):
        return self.x == x or self.y == y

    def moves( self ):
        l = []
        for x in range(8):
            if x != self.x:
                l.append( (x, self.y) )

        for y in range(8):
            if y != self.y:
                l.append( (self.x, y) )

        return l

# data = input( ">>> " ).split()
#data = 'black c4 c8 h3'.split()
data = 'white a1 b2 c3'.split()

now_white = data[ 0 ] == 'white'

white_king = King( *ch2c( data[ 1 ] ) )
white_tower = Tower( *ch2c( data[ 2 ] ) )
black_king = King( *ch2c( data[ 3 ] ) )

queue = deque()
queue.append( (white_king, white_tower, black_king, now_white) )
states = {str(queue[0]): None}

def can_move_black( x, y, wt, wk ):
    return not wt.can_hit( x, y ) and not wk.can_hit( x, y )

def can_move_white( x, y, bk, wo ):
    return not bk.can_hit( x, y ) and x != wo.x and y != wo.y

st = None

def draw( wk, wt, bk, now_white ):
    for y in range( 8 ):
        l = [ '.' ] * 8
        if y == wk.y:
            l[ wk.x ] = 'w'
        if y == wt.y:
            l[ wt.x ] = 't'
        if y == bk.y:
            l[ bk.x ] = 'b'

        print( ''.join( l ) )
    print( '' )


while len(queue) > 0:
    st = queue.popleft()
    (wk, wt, bk, now_white) = st

    if now_white:
        moves_k = list( filter( lambda p: can_move_white( *p, bk, wt ), wk.moves() ) )
        moves_t = list( filter( lambda p: can_move_white( *p, bk, wk ), wt.moves() ) )

        for p in moves_k:
            owk = deepcopy( wk )
            owk.x, owk.y = p

            new_state = (owk, wt, bk, False)
            if str( new_state ) in states:
                continue
            queue.append( new_state )

            states[ str( new_state ) ] = st

        for p in moves_t:
            owt = deepcopy( wt )
            owt.x, owt.y = p

            new_state = (wk, owt, bk, False)
            if str( new_state ) in states:
                continue
            queue.append( new_state )

            states[ str( new_state ) ] = st

    else:
        moves = bk.moves()
        moves = list( filter( lambda p: can_move_black( *p, wt, wk ), moves ) )
        if not moves:
            print( "FOUND" )
            break

        for p in moves:
            obk = deepcopy( bk )
            obk.x, obk.y = p

            new_state = (wk, wt, obk, True)
            if str( new_state ) in states:
                continue

            queue.append( new_state )
            states[ str( new_state ) ] = st

while st:
    draw( *st )
    st = states[ str( st ) ]


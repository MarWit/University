#!/usr/bin/env python2

import random
import sys
from collections import defaultdict as dd
from math import *
from copy import deepcopy

BOK = 30
SX = -100
SY = 0
M = 8

def initial_board():
    B = [ [None] * M for i in range(M)]
    B[3][3] = 1
    B[4][4] = 1
    B[3][4] = 0
    B[4][3] = 0
    return B

class Node:
    def __init__( self, move = None, parent = None, board = None, player = None ):
        self.move = move
        self.parent = parent
        self.childs = []
        self.wins = 0
        self.visited = 0
        self.player = 1 - player
        self.pending = board.moves( player )

    def uct_select( self ):
        self.childs.sort( key = lambda x: x.wins/x.visited + sqrt( 2*log(self.visited) / x.visited ), reverse = True )
        return self.childs[ 0 ]

    def add( self, move, board ):
        n = Node( move = move, board = board, parent = self, player = self.player )
        self.pending.remove( move )
        self.childs.append( n )

        return n

    def update( self, res ):
        self.wins += res
        self.visited += 1

class Board:
    dirs  = [ (0,1), (1,0), (-1,0), (0,-1), (1,1), (-1,-1), (1,-1), (-1,1) ]
    rng = random.Random()

    def __init__(self):
        self.board = initial_board()
        self.fields = set()
        self.move_list = []
        self.history = []
        for i in range(M):
            for j in range(M):
                if self.board[i][j] == None:
                    self.fields.add( (j,i) )

    def draw(self):
        for i in range(M):
            res = []
            for j in range(M):
                b = self.board[i][j]
                if b == None:
                    res.append('.')
                elif b == 1:
                    res.append('#')
                else:
                    res.append('o')
            print ''.join(res)
        print

    def moves(self, player):
        res = []
        for (x,y) in self.fields:
            if any( self.can_beat(x,y, direction, player) for direction in Board.dirs):
                res.append( (x,y) )
        if not res:
            return [None]
        return res

    def can_beat(self, x,y, d, player):
        dx,dy = d
        x += dx
        y += dy
        cnt = 0
        while self.get(x,y) == 1-player:
            x += dx
            y += dy
            cnt += 1
        return cnt > 0 and self.get(x,y) == player

    def get(self, x,y):
        if 0 <= x < M and 0 <=y < M:
            return self.board[y][x]
        return None

    def do_move(self, move, player):
        self.history.append([x[:] for x in self.board])
        self.move_list.append(move)

        if move == None:
            return
        x,y = move
        x0,y0 = move
        self.board[y][x] = player
        self.fields -= set([move])
        for dx,dy in self.dirs:
            x,y = x0,y0
            to_beat = []
            x += dx
            y += dy
            while self.get(x,y) == 1-player:
              to_beat.append( (x,y) )
              x += dx
              y += dy
            if self.get(x,y) == player:
                for (nx,ny) in to_beat:
                    self.board[ny][nx] = player

    def result(self):
        res = 0
        for y in range(M):
            for x in range(M):
                b = self.board[y][x]
                if b == 0:
                    res -= 1
                elif b == 1:
                    res += 1
        return res

    def player_result( self, player ):
        r = self.result()

        if (r < 0 and player == 0) or (r > 0 and player == 1):
            return 1
        elif r == 0:
            return 0.5
        else:
            return 0

    def terminal(self):
        if not self.fields:
            return True
        if len(self.move_list) < 2:
            return False
        return self.move_list[-1] == self.move_list[-2] == None

    def random_move(self, player):
        ms = self.moves(player)
        if ms:
            return random.choice(ms)
        return [None]

    def mcts_move(self, player):
        root = Node( board = self, player = player )

        for i in range( 1000 ):
            node = root
            state = deepcopy( self )
            now = player

            while node.pending == [] and node.childs != []:
                node = node.uct_select()
                state.do_move( node.move, now )
                now = 1 - now

            if node.pending != []:
                m = self.rng.choice( node.pending )
                state.do_move( m, now )
                now = 1 - now
                node = node.add( m, state )

            while not state.terminal():
                state.do_move( self.rng.choice( state.moves( now ) ), now )
                now = 1 - now

            while node != None:
                node.update( state.player_result( node.player ) )
                node = node.parent

        return sorted( root.childs, key = lambda s: s.visited )[ -1 ].move

score = 0

for i in range(10):
    print( i, score )
    player = 0
    B = Board()

    while True:
        if player == 1:
            m = B.random_move(player)
        else:
            m = B.mcts_move(player)
        B.do_move(m, player)
        player = 1-player
        if B.terminal():
            break

    if B.result() < 0:
        score += 1

print( score )

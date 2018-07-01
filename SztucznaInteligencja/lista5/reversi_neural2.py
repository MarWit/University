#!/usr/bin/env python3

import random
import sys
from collections import defaultdict as dd
from math import *
from copy import deepcopy

from keras.models import Sequential
from keras.layers import *
import keras.utils

import numpy as np

model = Sequential()
model.add( Dense(192, activation='relu') )
model.add( Dense(128, activation='sigmoid') )
model.add( Dense(64, activation='sigmoid') )
model.add( Dense(16, activation='sigmoid') )
model.add( Dense(2, activation='softmax' ) )

model.compile( optimizer = 'rmsprop', loss = ['categorical_crossentropy'], metrics=['accuracy' ] )

with open('/home/marwit/Downloads/reversi_learning_data/bigger.dat' ) as f:
    data = f.read().splitlines()

games = []
labels = []

for line in data:
    (who, game) = line.split()

    who = [0] if who == '1' else [1]
    l = []
    ll = []

    for c in game:
        if c == '1':
            l += [1]
            ll += [1,0,0]
        elif c == '0':
            l += [-1]
            ll += [0,1,0]
        else:
            l += [0]
            ll += [0,0,1]

    a = sum( l ) / 64
    b = (l[ 0 ] + l[ 7 ] + l[ 56 ] + l[ 63 ]) / 4
    c = l[ 1 ] + l[ 6 ] + l[ 8 ] + l[ 9 ] + l[ 14 ] + l[ 15 ]
    c += l[ 48 ] + l[ 49 ] + l[ 54 ] + l[ 55 ] + l[ 57 ] + l[ 62 ]
    c /= 12

    games.append( ll + [a, b, c] )
    labels.append( who )

games = np.vstack( games )
labels = np.vstack( labels )

llabels = keras.utils.to_categorical( labels, num_classes = 2 )
model.fit( games, llabels, epochs = 25, batch_size = 1024 )

BOK = 30
SX = -100
SY = 0
M = 8

def neural_result( board, player ):
    global model
    l = []
    ll = []

    for j in range(8):
        for i in range(8):
            x = board.get( i, j )

            if x == 1:
                ll += [1,0,0]
                l += [1]
            elif x == 0:
                l += [-1]
                ll += [0,1,0]
            else:
                l += [0]
                ll += [0,0,1]

    a = sum( l ) / 64
    b = (l[ 0 ] + l[ 7 ] + l[ 56 ] + l[ 63 ]) / 4
    c = l[ 1 ] + l[ 6 ] + l[ 8 ] + l[ 9 ] + l[ 14 ] + l[ 15 ]
    c += l[ 48 ] + l[ 49 ] + l[ 54 ] + l[ 55 ] + l[ 57 ] + l[ 62 ]
    c /= 12

    x = ll + [a, b, c]

    x = np.array( x ).reshape( (1, 3 + 192) )
    v = model.predict( x )

    return 1 if int( v[ 0 ][ 0 ] > v[ 0 ][ 1 ] ) == player else -1

def initial_board():
    B = [ [None] * M for i in range(M)]
    B[3][3] = 1
    B[4][4] = 1
    B[3][4] = 0
    B[4][3] = 0
    return B

def pvs( board, depth, alpha, beta, now, player ):
    if depth >= 2:
        return now * neural_result( board, player )

    value = 0.0
    first = True

    player_now = 1 - player if now > 0 else player
    boards = []

    for move in board.moves( player_now ):
        new_board = deepcopy(board)
        new_board.do_move( move, player_now )
        boards.append((new_board, new_board.result() * (1, -1)[ player ]))

    boards.sort( key = lambda x: x[1], reverse = True )
    for (new_board, _) in boards:
        if first:
            first = False
            value = -pvs( new_board, depth + 1, -beta, -alpha, -now, player )
        else:
            value = -pvs( new_board, depth + 1, -alpha-1, -alpha, -now, player )
            if alpha < value and beta > value:
                value = -pvs( new_board, depth + 1, -beta, -value, -now, player )

            alpha = max( alpha, value )
            if alpha >= beta:
                break

    return alpha

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
            print( ''.join(res) )
        print()

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

    def negascout_move(self, player):
        moves = []

        for move in self.moves( player ):
            board = deepcopy( self )
            board.do_move( move, player )
            moves.append( (move, pvs(board, 0, float('-inf'), float('inf'), 1.0, player)) )

        if moves:
            moves.sort( key = lambda x: x[1], reverse = True )
            return moves[ 0 ][ 0 ]
        else:
            return [None]


score = 0

for i in range(10):
    print( i, score )
    player = 1
    B = Board()

    while True:
        if player == 1:
            m = B.negascout_move(player)
        else:
            m = B.random_move(player)
        B.do_move(m, player)
        player = 1-player
        if B.terminal():
            break

    if B.result() < 0:
        score += 1

print( score )

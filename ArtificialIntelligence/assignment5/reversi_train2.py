#!/usr/bin/env python

from keras.models import Sequential
from keras.layers import *
import keras.utils

import numpy as np

def valid(x,y):
    return x >= 0 and x < 8 and y >=0 and y < 8

def can_beat(b, x, y, d, player):
    dx,dy = d
    x += dx
    y += dy
    cnt = 0
    while valid(x,y) and b[ y*8+x ] == 1-player:
        x += dx
        y += dy
        cnt += 1
    return cnt > 0 and valid( x, y ) and b[ y*8+x ] == player


model = Sequential()
model.add( Dense(192, activation='relu') )
model.add( Dense(128, activation='sigmoid') )
model.add( Dense(64, activation='sigmoid') )
model.add( Dense(16, activation='sigmoid') )
model.add( Dense(2, activation='softmax' ) )

model.compile( optimizer = 'rmsprop', loss = ['binary_crossentropy'], metrics=['accuracy' ] )

with open('/home/marwit/Downloads/reversi_learning_data/bigger.dat' ) as f:
    data = f.read().splitlines()

games = []
labels = []

for line in data:
    (who, game) = line.split()

    who = [1,0] if who == '1' else [0,1]
    l = []
    b = []

    for c in game:
        b += [ int( c ) ] if c.isdecimal() else [ None ]
        if c == '1':
            l += [1,0,0]
        elif c == '0':
            l += [0,1,0]
        else:
            l += [0,0,1]

    games.append( l )
    labels.append( who )

games = np.vstack( games )
labels = np.vstack( labels )

model.fit( games, labels, epochs = 50, batch_size = 1024 )
model.save( 'trained.h5' )

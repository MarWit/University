#!/usr/bin/env python

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

model.compile( optimizer = 'rmsprop', loss = ['binary_crossentropy'], metrics=['accuracy' ] )

with open('/home/marwit/Downloads/reversi_learning_data/bigger.dat' ) as f:
    data = f.read().splitlines()

games = []
labels = []

for line in data:
    (who, game) = line.split()

    who = [1,0] if who == '1' else [0,1]
    l = []

    for c in game:
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
model.save( 'trained.hdf5' )

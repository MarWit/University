#!/usr/bin/env python

import struct
from collections import namedtuple

class WAVFile( object ):
    Header = namedtuple( 'header', [ 'ChunkID', 'ChunkSize', 'Format' ] )
    HeaderStruct = '4si4s'
    HeaderSize = struct.calcsize( HeaderStruct )
    HeaderPack = lambda self,  dataSize: struct.pack( self . HeaderStruct, 'RIFF', dataSize + 36, 'WAVE' )
    HeaderUnpack = lambda self,  what, off: self . Header . _make( struct.unpack_from( self . HeaderStruct, what, off ) )

    FMT = namedtuple( 'fmt', [ 'SubchunkID', 'SubchunkSize', 'AudioFormat', 'NumChannels', 'SampleRate', 'ByteRate', 'BlockAlign', 'BitsPerSample' ] )
    FMTStruct = '4sihhiihh'
    FMTSize = struct.calcsize( FMTStruct )
    FMTPack = lambda self,  chan, sr, bps: struct.pack( self . FMTStruct, 'fmt ', 16, 1, chan, sr, sr * chan * bps // 8, chan * bps // 8, bps )
    FMTUnpack = lambda self,  what, off: self . FMT . _make( struct.unpack_from( self . FMTStruct, what, off ) )

    Data = namedtuple( 'data', [ 'SubchunkID', 'SubchunkSize', 'Data' ] )
    DataStruct = '4sib'
    DataSize = struct.calcsize( DataStruct )
    DataPack = lambda self,  size, data: struct.pack( self . Data, 'data', size, data )
    DataUnpack = lambda self,  what, off: self . Data . _make( struct.unpack_from( self . DataStruct, what, off ) )

    loadedHeader = None
    loadedFmt = None
    loadedData = None

    def __init__( self, filename = None, bytes = None ):
        if filename != None:
            with open( filename, 'rb' ) as f:
                bytes = f . read( )

        if bytes != None:
            self . loadData( bytes )

        print( self . loadedHeader )
        print( self . loadedFmt )
        print( self . loadedData )

    def loadData( self, bytes ):
        self . loadedHeader = self . HeaderUnpack( bytes, 0 )
        self . loadedFmt = self . FMTUnpack( bytes, self . HeaderSize )
        self . loadedData = self . DataUnpack( bytes, self . HeaderSize + self . FMTSize )
        self . loadedData[ 'Data' ] = bytes[ ( self . HeaderSize + self . FMTSize + self . DataSize - 1 ) ]

    def flushData( self ):
        self . loadedHeader = None
        self . loadedFmt = None
        self . loadedData = None

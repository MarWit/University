using System;
using System.IO;

namespace SIDious {
    /**
     * Klasa odpowiedzialna za pliki P-SID.
     * Klasa implementuje metody pozwalające załadować pliki typu
     * P-SID do pamięci a także udostępniająca informacje o danym pliku
     */
    public class PSID {
        public class TrackInfo {
            public string Name { get; set; }
            public string Author { get; set; }
            public string Published { get; set; }
        }

        public TrackInfo trackInfo;

        public short InitAddr;
        public short PlayAddr;
        public byte StartSong;
        public byte SubSongs;
        public byte SongSpeed;

        public void loadFileToMemory( string fileName, Memory memory ) {
            try {
                using( BinaryReader br = new BinaryReader( File.OpenRead( fileName ) ) ) {
                    br . BaseStream . Seek( 0x07, SeekOrigin . Begin );

                    byte dataOffset = br . ReadByte( );
                    br.ReadInt16( );

                    InitAddr = br . ReadByte( );
                    InitAddr <<= 8;
                    InitAddr |= br . ReadByte( );

                    PlayAddr = br . ReadByte( );
                    PlayAddr <<= 8;
                    PlayAddr |= br . ReadByte( );


                    br . BaseStream . Seek( 0x0f, SeekOrigin . Begin );
                    SubSongs = br . ReadByte( );
                    SubSongs --;
                    br . BaseStream . Seek( 0x11, SeekOrigin . Begin );
                    StartSong = br . ReadByte( );
                    StartSong --;
                    br . BaseStream . Seek( 0x15, SeekOrigin . Begin );
                    SongSpeed = br . ReadByte( );

                    if ( this . trackInfo == null )
                        this . trackInfo = new TrackInfo( );

                    this . trackInfo . Name = new string( br . ReadChars( 32 ) );
                    this . trackInfo . Author = new string( br . ReadChars( 32 ) );
                    this . trackInfo . Published = new string( br . ReadChars( 32 ) );

                    br . BaseStream . Seek( dataOffset, SeekOrigin . Begin );
                    short loadAddr = br . ReadInt16();

                    while( br . BaseStream . Position != br . BaseStream . Length ) {
                        memory[ loadAddr ] = br . ReadByte( );
                        loadAddr ++;
                    }
                }

            } catch( Exception e ) {
                Console . WriteLine( "The file could not be read:" );
                Console . WriteLine( e . Message );
            }

        }
    }
}

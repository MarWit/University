using System;
using System.Threading;

namespace SIDious {
    class SIDious {
        public static void Play( IntPtr hPulseHandle, CPU cpu, PSID psid ) {
            int sampleSize = 882 - 441 * (psid.SongSpeed);
            int sampleCount = 8 + (psid.SongSpeed) * 8;
            uint bufferSize = ( uint )( 2 * sampleSize * sampleCount );

            short[] buffer = new short[ 1 << 16 ];
            int error = 0;

            while( true ) {
                for( int i = 0; i < sampleCount; i ++ ) {
                    cpu . jumpSubroutine( (ushort) psid.PlayAddr, 0 );
                    cpu . sid . Render( sampleSize, buffer, i * sampleSize );
                }

                PulseAudio.pa_simple_write( hPulseHandle, buffer, bufferSize, ref error );
            }
        }

        public static void Main( string[ ] args ) {
            if( args . Length != 1 ) {
                Console . WriteLine( "Usage: SIDious.exe <filename.sid>" );
                return;
            }

            PulseAudio . pa_sample_spec ss;
            ss . format = 4;
            ss . rate = 44100;
            ss . channels = 1;

            IntPtr hPulseHandle = PulseAudio.pa_simple_new( null, "SIDious", PulseAudio.PA_STREAM_PLAYBACK, null, "Music", ref ss, IntPtr.Zero, IntPtr.Zero, IntPtr.Zero );

            CPU cpu = new CPU( );
            PSID psid = new PSID( );

            psid . loadFileToMemory( args[ 0 ], cpu . getMemory( ) );
            cpu.jumpSubroutine( (ushort) psid.InitAddr, psid.StartSong );

            Console.WriteLine( "Name: {0}", psid . trackInfo . Name );
            Console.WriteLine( "Author: {0}", psid . trackInfo . Author );
            Console.WriteLine( "Published: {0}", psid . trackInfo . Published );

            Play( hPulseHandle, cpu, psid );

            PulseAudio.pa_simple_destroy( hPulseHandle );
        }
    }
}

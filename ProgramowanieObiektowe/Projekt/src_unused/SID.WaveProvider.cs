using System;
using NAudio.Wave;

namespace SIDious {
    partial class SID {
        class WaveProvider : IWaveProvider {
            public WaveFormat WaveFormat { get; set; }
            private AudioBuffer audioBuffer;
        
            public WaveProvider(AudioBuffer audioBuffer)
                : this(44100, 1) {
                this.audioBuffer = audioBuffer;
            }
        
            public WaveProvider(int sampleRate, int channels) {
                this.WaveFormat = new WaveFormat(sampleRate, 16, channels);
            }
        
            public int Read( byte[] buffer, int offset, int count ) {
                WaveBuffer waveBuffer = new WaveBuffer(buffer);
                int samplesRequired = count / 2;
                int samplesRead = Read(waveBuffer.ShortBuffer, offset / 2, samplesRequired);
                return samplesRead * 2;
            }
        
            public int Read( short[] buffer, int offset, int sampleCount ) {
                return audioBuffer.copyToArray( buffer, offset, sampleCount );
            }
        }

        public class AudioBuffer {
            public static readonly int BUFFER_SIZE = 1 << 16;

            short[] ringBuffer = new short[ BUFFER_SIZE ];
            ushort startPointer;
            ushort nextSamplePointer;

            public void Write( short _value ) {
                ringBuffer[ nextSamplePointer ] = _value;
                nextSamplePointer ++;
            }

            public int copyToArray( short[] output, int offset, int numSamples ) {
                if( startPointer < nextSamplePointer ) {
                    int len = Math.Min( nextSamplePointer - startPointer, numSamples );
                    copy( output, ringBuffer, startPointer, offset, len );
                    startPointer += (ushort) len;
                    return len;
                } else if( nextSamplePointer < startPointer ) {
                    int endLen = Math.Min( BUFFER_SIZE - startPointer, numSamples );
                    copy( output, ringBuffer, startPointer, offset, endLen );
                    numSamples -= endLen;

                    int startLen = Math.Min( nextSamplePointer, numSamples );
                    copy( output, ringBuffer, 0, offset + endLen, startLen );

                    int dataCopied = endLen + startLen;
                    nextSamplePointer += (ushort) dataCopied;

                    return dataCopied;
                }

                return 0;
            }

            void copy( short[] dst, short[] src, int srcOff, int dstOff, int len ) {
                for( int i = 0; i < len; ++ i ) {
                    dst[ dstOff + i ] = src[ srcOff + i ];
                }
            }
        }
    }
}

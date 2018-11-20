using System;

namespace SIDious {

    /**
     * Klasa emulująca SID
     * Klasa mająca za zadanie obsłużyć emulacje interfejsu dzwiękowego SID
     * w wersji 6581 znanego z Commodore 64.
     */
    partial class SID : IMappable<byte> {
        /**
         * Struktura przechowywująca informacje o pojedyńczym głosie
         */
        struct SVoice {
            public ushort Freq;
            public ushort Pulse;
            public byte Control;
            public byte AtDec;
            public byte SusRel;
        }

        /**
         * Struktura przechowywująca informacje o filtrze.
         * @note Aktualnie nie jest używana
         */
        struct SFilter {
            public byte FreqLo;
            public byte FreqHi;
            public byte ResFil;
            public byte ModeVol;
        }

        /**
         * Struktura przechowywująca informacje wewnętrzne ( głownie o samplach )
         */
        struct SInternal {
            public int Peroid;
            public int Order;
            public int Start;
            public int End;
            public int Add;
            public int RepeatStart;
            public int RepeatCount;
            public int FreqMul;
        }

        /**
         * Struktura przechowywująca informacje o pojedyńczym oscylatorze.
         */
        struct SOscilator {
            public int Freq;
            public int Pulse;
            public byte Control;
            public byte Filter;
            public int Attack;
            public int Decay;
            public int Sustain;
            public int Release;
            public int Counter;
            public int EnvValue;
            public byte EnvPhase;
            public int NoisePos;
            public int NoiseVal;
            public byte NoiseOutput;
        }

        SVoice[] Voices = new SVoice[ 3 ];
        SOscilator[] Oscilators = new SOscilator[ 3 ];
        SFilter Filter;
        SInternal Internal;
        ArrayChunk<byte> memory;

        int[] Attacks = new int[ 16 ];
        int[] Releases = new int[ 16 ];

        public SID( int frequency = 44100 ) {
            for( int i = 0; i < 16; i ++ ) {
                Attacks[ i ] = (int)( 0x1000000 / ( AttackTimes[ i ] * frequency ) );
                Releases[ i ] = (int)( 0x1000000 / ( DecayReleaseTimes[ i ] * frequency ) );
            }

            Internal.FreqMul = 15872000 / frequency;

            for (int i = 0; i < 3; i++)
                Oscilators[ i ].NoiseVal = 0xffffff;

        }

        public void PreCalculate( int idx ) {
            Oscilators[ idx ] . Pulse = (Voices[ idx ] . Pulse & 0xfff) << 16;
            Oscilators[ idx ] . Filter = (byte) ( ( Filter.ResFil >> idx ) & 1 );
            Oscilators[ idx ] . Attack = Attacks[Voices[ idx ] . AtDec >> 4];
            Oscilators[ idx ] . Decay = Releases[Voices[ idx ] . AtDec & 0xf];
            Oscilators[ idx ] . Sustain = Voices[ idx ] . SusRel & 0xf0;
            Oscilators[ idx ] . Release = Releases[ Voices[ idx ] . SusRel & 0xf ];
            Oscilators[ idx ] . Control = Voices[ idx ] . Control;
            Oscilators[ idx ] . Freq = Voices[ idx ] . Freq * Internal . FreqMul;
        }

        // DISCLAIMER: This part was hardly based on tinysid implementation
        // because I thought that something was wrong with my sound rendering
        // alghirithm. After all, nothing really changed except I lost my own
        // implementation.
        public void Render( int amount, short[] buffer, int offset ) {
            for( int i = 0; i < 3; i ++ )
                PreCalculate( i );

            int refOsc;
            byte triangleSig, sawSig, pulseSig, noiseSig, concatSig;
            short finalSig;

            int arrPtr = 0;

            while( -- amount > 0 ) {
                finalSig = 0;

                // for( int i = 0; i < 3; i ++) {
                for( int i = 0; i < 1; i ++) {
                    Oscilators[ i ] . Counter = ( Oscilators[ i ] . Counter + Oscilators[ i ] . Freq) % 0xFFFFFFF;

                    if( ( Oscilators[ i ] . Control & 8) != 0 ) {
                        Oscilators[ i ] . Counter = 0;
                        Oscilators[ i ] . NoisePos = 0;
                        Oscilators[ i ] . NoiseVal = 0xFFFFFF;
                    }

                    refOsc = i > 0 ? i - 1 : 2;

                    if( ( Oscilators[ i ] . Control & 0x2) != 0 )
                        if( Oscilators[ refOsc ] . Counter < Oscilators[ refOsc ] . Freq )
                            Oscilators[ i ] . Counter = Oscilators[ refOsc ] . Counter *
                                                    Oscilators[ i ] . Freq / Oscilators[ refOsc ] . Freq;

                    triangleSig = ( byte )( Oscilators[ i ] . Counter >> 19 );
                    if( ( Oscilators[ i ] . Counter >> 27) != 0 )
                        triangleSig ^= 0xff;

                    sawSig = ( byte )( Oscilators[ i ] . Counter >> 20 );
                    if( Oscilators[ i ] . Counter > Oscilators[ i ] . Pulse )
                        pulseSig = 0;
                    else
                        pulseSig = 0xff;

                    if( Oscilators[ i ] . NoisePos != ( Oscilators[ i ] . Counter >> 23 ) ) {
                        Oscilators[ i ] . NoisePos = Oscilators[ i ] . Counter >> 23;
                        Oscilators[ i ] . NoiseVal = (Oscilators[ i ] . NoiseVal << 1) |
                                                   (((Oscilators[ i ] . NoiseVal >> 22) & 1) ^ ((Oscilators[ i ] . NoiseVal >> 17) & 1));

                        Oscilators[ i ] . NoiseOutput = (byte) ( ( ( Oscilators[ i ] . NoiseVal >> 22 ) & 1 ) << 7 |
                                                                 ( ( Oscilators[ i ] . NoiseVal >> 20 ) & 1 ) << 6 |
                                                                 ( ( Oscilators[ i ] . NoiseVal >> 16 ) & 1 ) << 5 |
                                                                 ( ( Oscilators[ i ] . NoiseVal >> 13 ) & 1 ) << 4 |
                                                                 ( ( Oscilators[ i ] . NoiseVal >> 11 ) & 1 ) << 3 |
                                                                 ( ( Oscilators[ i ] . NoiseVal >> 7 ) & 1 ) << 2  |
                                                                 ( ( Oscilators[ i ] . NoiseVal >> 4 ) & 1 ) << 1  |
                                                                 ( ( Oscilators[ i ] . NoiseVal >> 2 ) & 1 )
                                                               );

                    }

                    noiseSig = Oscilators[ i ] . NoiseOutput;

                    if( ( Oscilators[ i ] . Control & 4) != 0 )
                        if( Oscilators[ refOsc ] . Counter < 0x8000000 )
                            triangleSig ^= 0xff;

                    concatSig = 0xff;

                    // XXX: Those should be uncommented
                    // if( ( Oscilators[ i ] . Control & 0x10 ) != 0 ) concatSig &= triangleSig;
                    // if( ( Oscilators[ i ] . Control & 0x20 ) != 0 ) concatSig &= sawSig;
                    if( ( Oscilators[ i ].Control & 0x40 ) != 0 ) concatSig &= pulseSig;
                    // if( ( Oscilators[ i ] . Control & 0x80 ) != 0 ) concatSig &= noiseSig;

                    if( ( Oscilators[ i ] . Control & 1 ) == 0 )
                        Oscilators[ i ] . EnvPhase = 3;
                    else if( Oscilators[ i ] . EnvPhase == 3)
                        Oscilators[ i ] . EnvPhase = 0;

                    switch( Oscilators[ i ].EnvPhase ) {
                        case 0:
                            Oscilators[ i ] . EnvValue += Oscilators[ i ] . Attack;
                            if( Oscilators[ i ] . EnvValue >= 0xFFFFFF ) {
                                Oscilators[ i ] . EnvValue = 0XFFFFFF;
                                Oscilators[ i ] . EnvPhase = 1;
                            }

                            break;

                        case 1:
                            Oscilators[ i ] . EnvValue -= Oscilators[ i ] . Decay;
                            if( Oscilators[ i ] . EnvValue <= ( Oscilators[ i ] . Sustain << 16 ) ) {
                                Oscilators[ i ] . EnvValue = Oscilators[ i ] . Sustain << 16;
                                Oscilators[ i ] . EnvPhase = 2;
                            }

                            break;

                        case 2:
                            if( Oscilators[ i ] . EnvValue != ( Oscilators[ i ] . Sustain << 16 ) )
                                Oscilators[ i ] . EnvPhase = 1;

                            break;

                        case 3:
                            Oscilators[ i ] . EnvValue -= Oscilators[ i ] . Release;
                            if( Oscilators[ i ] . EnvValue < 0x40000 )
                                Oscilators[ i ] . EnvValue = 0x40000;

                            break;
                    }

                    finalSig += (short)( ( concatSig - 0x80 ) * ( Oscilators[ i ] . EnvValue >> 8 ) );
                }

                buffer[ offset + arrPtr ++ ] = (short) (finalSig >> 10);
            }
        }

        public int RenderDigital( int sIn ) {
            throw new Exception( "Not implemented yet." );
        }

        public void storeValue( uint address, byte _value )
        {
            Poke( address & 0x1f, _value );

            switch( address )
            {
                case 0xd41d:
                    break; // TODO: Render samples

                case 0xd41e:
                    Internal . Start &= 0xff00;
                    Internal . Start |= _value;
                    break;

                case 0xd41f:
                    Internal . Start &= 0xff;
                    Internal . Start |= (_value << 8);
                    break;

                case 0xd43d:
                    Internal . End &= 0xff00;
                    Internal . End |= _value;
                    break;

                case 0xd43e:
                    Internal . End &= 0xff;
                    Internal . End |= (_value << 8);
                    break;

                case 0xd43f:
                    Internal . RepeatCount = _value;
                    break;

                case 0xd45d:
                    Internal . Peroid &= 0xff00;
                    Internal . Peroid |= _value;
                    break;

                case 0xd45e:
                    Internal . Peroid &= 0xff;
                    Internal . Peroid |= (_value << 8);
                    break;

                case 0xd45f:
                    Internal . Add = _value;
                    break;

                case 0xd47d:
                    Internal . Order = _value;
                    break;

                case 0xd47e:
                    Internal . RepeatStart &= 0xff00;
                    Internal . RepeatStart |= _value;
                    break;

                case 0xd47f:
                    Internal . RepeatStart &= 0xff;
                    Internal . RepeatStart |= (_value << 8);
                    break;

            }
        }

        public void Poke( uint reg, byte _value ) {
            uint voice = 0;
            if( ( reg >= 7 ) && ( reg <= 13 ) ) {
                voice = 1;
                reg -= 7;
            } else if( ( reg >= 14 ) && ( reg <= 20 ) ) {
                voice = 2;
                reg -= 14;
            }

            switch( reg ) {
                case 0:
                    Voices[ voice ] . Freq &= 0xff00;
                    Voices[ voice ] . Freq |= _value;
                    break;

                case 1:
                    Voices[ voice ] . Freq &= 0xff;
                    Voices[ voice ] . Freq |= (ushort) (_value << 8);
                    break;

                case 2:
                    Voices[ voice ] . Pulse &= 0xff00;
                    Voices[ voice ] . Pulse |= _value;
                    break;

                case 3:
                    Voices[ voice ] . Pulse &= 0xff;
                    Voices[ voice ] . Pulse |= (ushort) (_value << 8);
                    break;

                case 4:
                    Voices[ voice ] . Control = _value;
                    break;

                case 5:
                    Voices[ voice ] . AtDec = _value;
                    break;

                case 6:
                    Voices[ voice ] . SusRel = _value;
                    break;

                case 21:
                    Filter . FreqLo = _value;
                    break;

                case 22:
                    Filter . FreqHi = _value;
                    break;

                case 23:
                    Filter . ResFil = _value;
                    break;

                case 24:
                    Filter . ModeVol = _value;
                    break;
            }
        }

        public void attachMemory( ArrayChunk<byte> memory ) {
            this . memory = memory;
        }
    }
}

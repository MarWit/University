using System;
using System.Collections;

namespace SIDious {
    /**
     * Klasa implementująca stos procesora.
     */
    class Stack : IMappable<byte> {
        ArrayChunk<byte> memory;
        byte stackPointer = 0xFF;

        public byte getStackPointer( ) {
            return stackPointer;
        }

        public void setStackPointer( byte sp ) {
            stackPointer = sp;
        }

        public void pushByte( byte _value ) {
            memory[ stackPointer ] = _value;
            stackPointer --;
        }

        public void pushWord( uint _value ) {

            memory[ stackPointer ] = (byte)(_value >> 8);
            stackPointer --;

            memory[ stackPointer ] = (byte)(_value & 0xff);
            stackPointer --;
        }

        public void pushFlags( CPU . SRegister . SFlags flags ) {
            BitArray a = new BitArray(
                    new Boolean[] {
                        flags . N,
                        flags . V,
                        flags . B,
                        flags . D,
                        flags . I,
                        flags . Z,
                        flags . C
                    }
            );

            byte[] d = new byte[ 1 ];
            a.CopyTo( d, 0 );

            pushByte( d[ 0 ] );
        }

        public byte popByte( ) {
            stackPointer ++;
            return memory[ stackPointer ];
        }

        public uint popWord( ) {
            stackPointer += 2;
            return (uint) (memory[ stackPointer] << 8) | memory[ stackPointer - 1 ];
        }

        public CPU . SRegister . SFlags popFlags( ) {
            byte flags = popByte( );
            BitArray a = new BitArray( new byte[] { flags } );

            CPU . SRegister . SFlags ret;

            ret . N = a[ 0 ];
            ret . V = a[ 1 ];
            ret . B = a[ 2 ];
            ret . D = a[ 3 ];
            ret . I = a[ 4 ];
            ret . Z = a[ 5 ];
            ret . C = a[ 6 ];

            return ret;
        }

        public void Flush( ) {
            for (int i = 0; i < 0xFF; i++)
                memory[i] = 0;
        }

        public void attachMemory( ArrayChunk<byte> memory ) {
            this . memory = memory;
        }
    }
}

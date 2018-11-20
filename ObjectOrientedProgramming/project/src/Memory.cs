using System;

namespace SIDious {
    /**
     * Klasa implementująca metody do zarządaniem pamięcią (proste MCU).
     */
    public class Memory {
        byte[] memory = new byte[ 0xffff ];

        public ArrayChunk<byte> createMemoryChunk( int start, int end ) {
            return new ArrayChunk<byte>( memory, start, end );
        }

        public uint readWord( uint address ) {
            return ( (uint)( memory[ ( address + 1 ) % 0xffff ] << 8 ) | memory[ address % 0xffff ] );
        }

        public byte readByte( uint address ) {
            return memory[ address % 0xffff ];
        }

        public sbyte readSByte( uint address ) {
            return unchecked( (sbyte) memory[ address % 0xffff ] );
        }

        public void writeByte( uint address, byte _value ) {
            memory[ address ] = _value;
        }

        public void Clear( ) {
            for( int i = 0; i < 0xffff; ++ i )
                memory[ i ] = 0;
        }

        public byte this[int key] {
            get { return memory[key]; } 
            set { memory[key] = value; } 
        }
    }
}

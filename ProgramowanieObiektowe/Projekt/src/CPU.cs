using System;

namespace SIDious {
    /**
     * Klasa implementująca procesor MOS 6510
     * Głowna klasa implementująca logikę służącą do wykonywania
     * plików binarnych napisanych na procesor MOS 6510. Zaimplementowana
     * jest większość opcode'ów w tym niektóre zabronione
     */
    partial class CPU {
        Memory memory = new Memory( );
        Stack stack = new Stack( );
        public SID sid = new SID( );

        public CPU( ) {
            registerFunctions( );

            stack . attachMemory( memory . createMemoryChunk( 0x100, 0x1ff ) );
            sid . attachMemory( memory . createMemoryChunk( 0xd400, 0xd7ff ) );

            Reset( );
        }

        public Memory getMemory( ) {
            return this . memory;
        }


        void updateFlags( byte _value ) {
            if( ( _value & 0x80 ) != 0 )
                register . Flags . N = true;
            else
                register . Flags . N = false;

            register . Flags . Z = ( _value == 0 );
        }

        void updateFlags( int _value ) {
            updateFlags( (byte) _value );
        }

        void Reset( ) {
            memory . writeByte( 0, 0xFF );
            memory . writeByte( 1, 0x07 );
            stack . Flush( );
            register . PC = (ushort) ( memory . readByte( 0xFFFC ) | ( memory . readByte( 0xFFFD ) << 8 ) );
            stack . setStackPointer( 0xff );

            register . A = register . X = register . Y = 0;
            register . Flags . N =  register . Flags . V =
                                    register . Flags . B =
                                    register . Flags . D =
                                    register . Flags . I =
                                    register . Flags . Z =
                                    register . Flags . C = false;
        }

        void Step( ) {
            byte op = memory . readByte( register . PC );
            Opcode opcode = (Opcode) opcodes[ (int) op ];
            register . PC += 1;

            //Console.WriteLine("0x{0:x} :: {1:x} :: {2} :: {3} :: {4}", register.PC - 1, op, register.Flags.N ? 1 : 0, register . A, register . X) ;

            operationHandlers[ (int) opcode . operation ]( opcode . operand );
        }
    }
}

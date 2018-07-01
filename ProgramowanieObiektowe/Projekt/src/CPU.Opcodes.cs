using System;
using System.Reflection;

namespace SIDious {
    partial class CPU {
        /**
         * Enum opisujący wszystkie instrukcje assemblera procesora 6510
         */
        private enum Operation : byte {
            ADC, AHX, ALR, ANC, AND, ARR, ASL, AXS,
            BCC, BCS, BEQ, BIT, BMI, BNE, BPL, BRK, BVC, BVS,
            CLC, CLD, CLI, CLV, CMP, CPX, CPY,
            DCP, DEC, DEX, DEY,
            EOR,
            INC, INX, INY, ISC,
            JMP, JSR,
            LAS, LAX, LDA, LDX, LDY, LSR,
            NOP,
            ORA,
            PHA, PHP, PLA, PLP,
            RLA, ROL, ROR, RRA, RTI, RTS,
            SAX, SBC, SEC, SED, SEI, SHX, SHY, SLO, SRE, STA, STX, STY,
            TAS, TAX, TAY, TSX, TXA, TXS, TYA,
            XAA,

            COUNT
        }

        /**
         * Enum opisujący rodzaj operandu danego opcode'u.
         */
        private enum Operand : byte {
            NONE,
            ABSOLUTE,
            ABSOLUTE_X,
            ABSOLUTE_Y,
            ACCUMULATOR,
            IMMEDIATE,
            IMPLIED,
            INDIRECT,
            INDIRECT_X,
            INDIRECT_Y,
            RELATIVE,
            ZEROPAGE,
            ZEROPAGE_X,
            ZEROPAGE_Y
        }

        /**
         * Struktura zawierająca informacje o opcode'ie.
         */
        struct Opcode {
            public Opcode( Operation _operation, Operand _operand ) {
                operation = _operation;
                operand = _operand;
            }

            public Operation operation;
            public Operand operand;
        }

        /**
         * Klasa implementacji niestandardowego atrubutu używanego do oznaczenia metod obsługujących opcode'y.
         */
        class OperationHandlerAttribute : Attribute {
            public Operation operation;
            public OperationHandlerAttribute( Operation _operation ) {
                this.operation = _operation;
            }
        }

        private static Opcode?[ ] opcodes = {
            // 0x0*
            new Opcode( Operation.BRK, Operand.IMPLIED ),
            new Opcode( Operation.ORA, Operand.INDIRECT_X ),
            null,
            new Opcode( Operation.SLO, Operand.INDIRECT_X ),
            new Opcode( Operation.NOP, Operand.ZEROPAGE ),
            new Opcode( Operation.ORA, Operand.ZEROPAGE ),
            new Opcode( Operation.ASL, Operand.ZEROPAGE ),
            new Opcode( Operation.SLO, Operand.ZEROPAGE ),
            new Opcode( Operation.PHP, Operand.IMPLIED ),
            new Opcode( Operation.ORA, Operand.IMMEDIATE ),
            new Opcode( Operation.ASL, Operand.ACCUMULATOR ),
            new Opcode( Operation.ANC, Operand.IMMEDIATE ),
            new Opcode( Operation.NOP, Operand.ABSOLUTE ),
            new Opcode( Operation.ORA, Operand.ABSOLUTE ),
            new Opcode( Operation.ASL, Operand.ABSOLUTE ),
            new Opcode( Operation.SLO, Operand.ABSOLUTE ),

            // 0x1*
            new Opcode( Operation.BPL, Operand.RELATIVE ),
            new Opcode( Operation.ORA, Operand.INDIRECT_Y ),
            null,
            new Opcode( Operation.SLO, Operand.INDIRECT_Y ),
            new Opcode( Operation.NOP, Operand.ZEROPAGE_X ),
            new Opcode( Operation.ORA, Operand.ZEROPAGE_X ),
            new Opcode( Operation.ASL, Operand.ZEROPAGE_X ),
            new Opcode( Operation.SLO, Operand.ZEROPAGE_X ),
            new Opcode( Operation.CLC, Operand.IMPLIED ),
            new Opcode( Operation.ORA, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.NOP, Operand.IMPLIED ),
            new Opcode( Operation.SLO, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.NOP, Operand.ABSOLUTE_X ),
            new Opcode( Operation.ORA, Operand.ABSOLUTE_X ),
            new Opcode( Operation.ASL, Operand.ABSOLUTE_X ),
            new Opcode( Operation.SLO, Operand.ABSOLUTE_X ),

            // 0x2*
            new Opcode( Operation.JSR, Operand.ABSOLUTE ),
            new Opcode( Operation.AND, Operand.INDIRECT_X ),
            null,
            new Opcode( Operation.RLA, Operand.INDIRECT_X ),
            new Opcode( Operation.BIT, Operand.ZEROPAGE ),
            new Opcode( Operation.AND, Operand.ZEROPAGE ),
            new Opcode( Operation.ROL, Operand.ZEROPAGE ),
            new Opcode( Operation.RLA, Operand.ZEROPAGE ),
            new Opcode( Operation.PLP, Operand.IMPLIED ),
            new Opcode( Operation.AND, Operand.IMMEDIATE ),
            new Opcode( Operation.ROL, Operand.ACCUMULATOR ),
            new Opcode( Operation.ANC, Operand.IMMEDIATE ),
            new Opcode( Operation.BIT, Operand.ABSOLUTE ),
            new Opcode( Operation.AND, Operand.ABSOLUTE ),
            new Opcode( Operation.ROL, Operand.ABSOLUTE ),
            new Opcode( Operation.RLA, Operand.ABSOLUTE ),

            // 0x3*
            new Opcode( Operation.BMI, Operand.RELATIVE ),
            new Opcode( Operation.AND, Operand.INDIRECT_Y ),
            null,
            new Opcode( Operation.RLA, Operand.INDIRECT_Y ),
            new Opcode( Operation.NOP, Operand.ZEROPAGE_X ),
            new Opcode( Operation.AND, Operand.ZEROPAGE_X ),
            new Opcode( Operation.ROL, Operand.ZEROPAGE_X ),
            new Opcode( Operation.RLA, Operand.ZEROPAGE_X ),
            new Opcode( Operation.SEC, Operand.IMPLIED ),
            new Opcode( Operation.AND, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.NOP, Operand.IMPLIED ),
            new Opcode( Operation.RLA, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.NOP, Operand.ABSOLUTE_X ),
            new Opcode( Operation.AND, Operand.ABSOLUTE_X ),
            new Opcode( Operation.ROL, Operand.ABSOLUTE_X ),
            new Opcode( Operation.RLA, Operand.ABSOLUTE_X ),

            // 0x4*
            new Opcode( Operation.RTI, Operand.IMPLIED ),
            new Opcode( Operation.EOR, Operand.INDIRECT_X ),
            null,
            new Opcode( Operation.SRE, Operand.INDIRECT_X ),
            new Opcode( Operation.NOP, Operand.ZEROPAGE ),
            new Opcode( Operation.EOR, Operand.ZEROPAGE ),
            new Opcode( Operation.LSR, Operand.ZEROPAGE ),
            new Opcode( Operation.SRE, Operand.ZEROPAGE ),
            new Opcode( Operation.PHA, Operand.IMPLIED ),
            new Opcode( Operation.EOR, Operand.IMMEDIATE ),
            new Opcode( Operation.LSR, Operand.ACCUMULATOR ),
            new Opcode( Operation.ALR, Operand.IMMEDIATE ),
            new Opcode( Operation.JMP, Operand.ABSOLUTE ),
            new Opcode( Operation.EOR, Operand.ABSOLUTE ),
            new Opcode( Operation.LSR, Operand.ABSOLUTE ),
            new Opcode( Operation.SRE, Operand.ABSOLUTE ),

            // 0x5*
            new Opcode( Operation.BVC, Operand.RELATIVE ),
            new Opcode( Operation.EOR, Operand.INDIRECT_Y ),
            null,
            new Opcode( Operation.SRE, Operand.INDIRECT_Y ),
            new Opcode( Operation.NOP, Operand.ZEROPAGE_X ),
            new Opcode( Operation.EOR, Operand.ZEROPAGE_X ),
            new Opcode( Operation.LSR, Operand.ZEROPAGE_X ),
            new Opcode( Operation.SRE, Operand.ZEROPAGE_X ),
            new Opcode( Operation.CLI, Operand.IMPLIED ),
            new Opcode( Operation.EOR, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.NOP, Operand.IMPLIED ),
            new Opcode( Operation.SRE, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.NOP, Operand.ABSOLUTE_X ),
            new Opcode( Operation.EOR, Operand.ABSOLUTE_X ),
            new Opcode( Operation.LSR, Operand.ABSOLUTE_X ),
            new Opcode( Operation.SRE, Operand.ABSOLUTE_X ),

            // 0x6*
            new Opcode( Operation.RTS, Operand.IMPLIED ),
            new Opcode( Operation.ADC, Operand.INDIRECT_X ),
            null,
            new Opcode( Operation.RRA, Operand.INDIRECT_X ),
            new Opcode( Operation.NOP, Operand.ZEROPAGE ),
            new Opcode( Operation.ADC, Operand.ZEROPAGE ),
            new Opcode( Operation.ROR, Operand.ZEROPAGE ),
            new Opcode( Operation.RRA, Operand.ZEROPAGE ),
            new Opcode( Operation.PLA, Operand.IMPLIED ),
            new Opcode( Operation.ADC, Operand.IMMEDIATE ),
            new Opcode( Operation.ROR, Operand.ACCUMULATOR ),
            new Opcode( Operation.ARR, Operand.IMMEDIATE ),
            new Opcode( Operation.JMP, Operand.INDIRECT ),
            new Opcode( Operation.ADC, Operand.ABSOLUTE ),
            new Opcode( Operation.ROR, Operand.ABSOLUTE ),
            new Opcode( Operation.RRA, Operand.ABSOLUTE ),

            // 0x7*
            new Opcode( Operation.BVS, Operand.RELATIVE ),
            new Opcode( Operation.ADC, Operand.INDIRECT_Y ),
            null,
            new Opcode( Operation.RRA, Operand.INDIRECT_Y ),
            new Opcode( Operation.NOP, Operand.ZEROPAGE_X ),
            new Opcode( Operation.ADC, Operand.ZEROPAGE_X ),
            new Opcode( Operation.ROR, Operand.ZEROPAGE_X ),
            new Opcode( Operation.RRA, Operand.ZEROPAGE_X ),
            new Opcode( Operation.SEI, Operand.IMPLIED ),
            new Opcode( Operation.ADC, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.NOP, Operand.IMPLIED ),
            new Opcode( Operation.RRA, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.NOP, Operand.ABSOLUTE_X ),
            new Opcode( Operation.ADC, Operand.ABSOLUTE_X ),
            new Opcode( Operation.ROR, Operand.ABSOLUTE_X ),
            new Opcode( Operation.RRA, Operand.ABSOLUTE_X ),

            // 0x8*
            new Opcode( Operation.NOP, Operand.IMMEDIATE ),
            new Opcode( Operation.STA, Operand.INDIRECT_X ),
            new Opcode( Operation.NOP, Operand.IMMEDIATE ),
            new Opcode( Operation.SAX, Operand.INDIRECT_X ),
            new Opcode( Operation.STY, Operand.ZEROPAGE ),
            new Opcode( Operation.STA, Operand.ZEROPAGE ),
            new Opcode( Operation.STX, Operand.ZEROPAGE ),
            new Opcode( Operation.SAX, Operand.ZEROPAGE ),
            new Opcode( Operation.DEY, Operand.IMPLIED ),
            new Opcode( Operation.NOP, Operand.IMMEDIATE ),
            new Opcode( Operation.TXA, Operand.IMPLIED ),
            new Opcode( Operation.XAA, Operand.IMMEDIATE ),
            new Opcode( Operation.STY, Operand.ABSOLUTE ),
            new Opcode( Operation.STA, Operand.ABSOLUTE ),
            new Opcode( Operation.STX, Operand.ABSOLUTE ),
            new Opcode( Operation.SAX, Operand.ABSOLUTE ),

            // 0x9*
            new Opcode( Operation.BCC, Operand.RELATIVE ),
            new Opcode( Operation.STA, Operand.INDIRECT_Y ),
            null,
            new Opcode( Operation.AHX, Operand.INDIRECT_Y ),
            new Opcode( Operation.STY, Operand.ZEROPAGE_X ),
            new Opcode( Operation.STA, Operand.ZEROPAGE_X ),
            new Opcode( Operation.STX, Operand.ZEROPAGE_Y ),
            new Opcode( Operation.SAX, Operand.ZEROPAGE_Y ),
            new Opcode( Operation.TYA, Operand.IMPLIED ),
            new Opcode( Operation.STA, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.TXS, Operand.IMPLIED ),
            new Opcode( Operation.TAS, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.SHY, Operand.ABSOLUTE_X ),
            new Opcode( Operation.STA, Operand.ABSOLUTE_X ),
            new Opcode( Operation.SHX, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.AHX, Operand.ABSOLUTE_Y ),

            // 0xA*
            new Opcode( Operation.LDY, Operand.IMMEDIATE ),
            new Opcode( Operation.LDA, Operand.INDIRECT_X ),
            new Opcode( Operation.LDX, Operand.IMMEDIATE ),
            new Opcode( Operation.LAX, Operand.INDIRECT_X ),
            new Opcode( Operation.LDY, Operand.ZEROPAGE ),
            new Opcode( Operation.LDA, Operand.ZEROPAGE ),
            new Opcode( Operation.LDX, Operand.ZEROPAGE ),
            new Opcode( Operation.LAX, Operand.ZEROPAGE ),
            new Opcode( Operation.TAY, Operand.IMPLIED ),
            new Opcode( Operation.LDA, Operand.IMMEDIATE ),
            new Opcode( Operation.TAX, Operand.IMPLIED ),
            new Opcode( Operation.LAX, Operand.IMMEDIATE ),
            new Opcode( Operation.LDY, Operand.ABSOLUTE ),
            new Opcode( Operation.LDA, Operand.ABSOLUTE ),
            new Opcode( Operation.LDX, Operand.ABSOLUTE ),
            new Opcode( Operation.LAX, Operand.ABSOLUTE ),

            // 0xB*
            new Opcode( Operation.BCS, Operand.RELATIVE ),
            new Opcode( Operation.LDA, Operand.INDIRECT_Y ),
            null,
            new Opcode( Operation.LAX, Operand.INDIRECT_Y ),
            new Opcode( Operation.LDY, Operand.ZEROPAGE_X ),
            new Opcode( Operation.LDA, Operand.ZEROPAGE_X ),
            new Opcode( Operation.LDX, Operand.ZEROPAGE_Y ),
            new Opcode( Operation.LAX, Operand.ZEROPAGE_Y ),
            new Opcode( Operation.CLV, Operand.IMPLIED ),
            new Opcode( Operation.LDA, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.TSX, Operand.IMPLIED ),
            new Opcode( Operation.LAS, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.LDY, Operand.ABSOLUTE_X ),
            new Opcode( Operation.LDA, Operand.ABSOLUTE_X ),
            new Opcode( Operation.LDX, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.LAX, Operand.ABSOLUTE_Y ),

            // 0xC*
            new Opcode( Operation.CPY, Operand.IMMEDIATE ),
            new Opcode( Operation.CMP, Operand.INDIRECT_X ),
            new Opcode( Operation.NOP, Operand.IMMEDIATE ),
            new Opcode( Operation.DCP, Operand.INDIRECT_X ),
            new Opcode( Operation.CPY, Operand.ZEROPAGE ),
            new Opcode( Operation.CMP, Operand.ZEROPAGE ),
            new Opcode( Operation.DEC, Operand.ZEROPAGE ),
            new Opcode( Operation.DCP, Operand.ZEROPAGE ),
            new Opcode( Operation.INY, Operand.IMPLIED ),
            new Opcode( Operation.CMP, Operand.IMMEDIATE ),
            new Opcode( Operation.DEX, Operand.IMPLIED ),
            new Opcode( Operation.AXS, Operand.IMMEDIATE ),
            new Opcode( Operation.CPY, Operand.ABSOLUTE ),
            new Opcode( Operation.CMP, Operand.ABSOLUTE ),
            new Opcode( Operation.DEC, Operand.ABSOLUTE ),
            new Opcode( Operation.DCP, Operand.ABSOLUTE ),

            // 0xD*
            new Opcode( Operation.BNE, Operand.RELATIVE ),
            new Opcode( Operation.CMP, Operand.INDIRECT_Y ),
            null,
            new Opcode( Operation.DCP, Operand.INDIRECT_Y ),
            new Opcode( Operation.NOP, Operand.ZEROPAGE_X ),
            new Opcode( Operation.CMP, Operand.ZEROPAGE_X ),
            new Opcode( Operation.DEC, Operand.ZEROPAGE_X ),
            new Opcode( Operation.DCP, Operand.ZEROPAGE_X ),
            new Opcode( Operation.CLD, Operand.IMPLIED ),
            new Opcode( Operation.CMP, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.NOP, Operand.IMPLIED ),
            new Opcode( Operation.DCP, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.NOP, Operand.ABSOLUTE_X ),
            new Opcode( Operation.CMP, Operand.ABSOLUTE_X ),
            new Opcode( Operation.DEC, Operand.ABSOLUTE_X ),
            new Opcode( Operation.DCP, Operand.ABSOLUTE_X ),

            // 0xE*
            new Opcode( Operation.CPX, Operand.IMMEDIATE ),
            new Opcode( Operation.SBC, Operand.INDIRECT_X ),
            new Opcode( Operation.NOP, Operand.IMMEDIATE ),
            new Opcode( Operation.ISC, Operand.INDIRECT_X ),
            new Opcode( Operation.CPX, Operand.ZEROPAGE ),
            new Opcode( Operation.SBC, Operand.ZEROPAGE ),
            new Opcode( Operation.INC, Operand.ZEROPAGE ),
            new Opcode( Operation.ISC, Operand.ZEROPAGE ),
            new Opcode( Operation.INX, Operand.IMPLIED ),
            new Opcode( Operation.SBC, Operand.IMMEDIATE ),
            new Opcode( Operation.NOP, Operand.IMPLIED ),
            new Opcode( Operation.SBC, Operand.IMMEDIATE ),
            new Opcode( Operation.CPX, Operand.ABSOLUTE ),
            new Opcode( Operation.SBC, Operand.ABSOLUTE ),
            new Opcode( Operation.INC, Operand.ABSOLUTE ),
            new Opcode( Operation.ISC, Operand.ABSOLUTE ),

            // 0xF*
            new Opcode( Operation.BEQ, Operand.RELATIVE ),
            new Opcode( Operation.SBC, Operand.INDIRECT_Y ),
            null,
            new Opcode( Operation.ISC, Operand.INDIRECT_Y ),
            new Opcode( Operation.NOP, Operand.ZEROPAGE_X ),
            new Opcode( Operation.SBC, Operand.ZEROPAGE_X ),
            new Opcode( Operation.INC, Operand.ZEROPAGE_X ),
            new Opcode( Operation.ISC, Operand.ZEROPAGE_X ),
            new Opcode( Operation.SED, Operand.IMPLIED ),
            new Opcode( Operation.SBC, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.NOP, Operand.IMPLIED ),
            new Opcode( Operation.ISC, Operand.ABSOLUTE_Y ),
            new Opcode( Operation.NOP, Operand.ABSOLUTE_X ),
            new Opcode( Operation.SBC, Operand.ABSOLUTE_X ),
            new Opcode( Operation.INC, Operand.ABSOLUTE_X ),
            new Opcode( Operation.ISC, Operand.ABSOLUTE_X )
        };

        Action<Operand>[] operationHandlers;

        private void registerFunctions( ) {
            operationHandlers = new Action<Operand>[ (int) Operation . COUNT ];

            var type = this.GetType();

            foreach( var method in type . GetMethods( BindingFlags . Instance | BindingFlags . NonPublic ) ) {
                var attr = method . GetCustomAttributes( typeof( OperationHandlerAttribute ), false );
                if( attr . Length > 0 ) {
                    Operation op = ((OperationHandlerAttribute) attr[ 0 ] ) . operation;
                    operationHandlers[ (int) op ] = (Action<Operand>) method . CreateDelegate( typeof( Action<Operand> ), this );
                }
            }
        }

        private byte readByteOperand( ) {
            byte val = memory.readByte( register . PC );
            ++ register . PC;
            return val;
        }

        private sbyte readSByteOperand( ) {
            sbyte val = memory.readSByte( register . PC );
            ++ register . PC;
            return val;

        }

        private uint readWordOperand( ) {
            uint val = memory.readWord( register . PC );
            register . PC += 2;
            return val;
        }

        private byte readValue( Operand type ) {
            uint address = 0;

            switch( type ) {
                case Operand . ABSOLUTE:
                    address = readWordOperand( );
                    break;
                case Operand . ABSOLUTE_X:
                    address = readWordOperand( );
                    address += register . X;
                    break;
                case Operand . ABSOLUTE_Y:
                    address = readWordOperand( );
                    address += register . Y;
                    break;
                case Operand . ACCUMULATOR:
                    return register . A;
                case Operand . IMMEDIATE:
                    return readByteOperand( );
                case Operand . IMPLIED:
                    return register . A;
                case Operand . INDIRECT:
                    address = readWordOperand( );
                    address = memory . readWord( address );
                    break;
                case Operand . INDIRECT_X:
                    address = readByteOperand( );
                    address = memory.readWord(address);
                    address += register . X;
                    break;
                case Operand . INDIRECT_Y:
                    uint tmp;
                    tmp = readByteOperand();
                    address = memory.readByte(tmp);
                    address |= (uint)(memory.readByte((tmp + 1)&0xff) << 8 );
                    address += register . Y;
                    break;
                case Operand . RELATIVE:
                    address = (uint) (register.PC + readSByteOperand());
                    break;
                case Operand . ZEROPAGE:
                    address = readByteOperand( );
                    break;
                case Operand . ZEROPAGE_X:
                    address = readByteOperand( );
                    address += register . X;
                    break;
                case Operand . ZEROPAGE_Y:
                    address = readByteOperand( );
                    address += register . Y;
                    break;
            }

            return memory.readByte( address );
        }

        // C# 4.0 allow for default parameters but whatever..
        private byte readValue( Operand type, bool preserve_pc ) {
            ushort PC = register . PC;
            byte _return = readValue( type );

            if( preserve_pc )
                register . PC = PC;

            return _return;
        }

        private void storeValue( Operand type, byte _value ) {
            uint address = 0;
            switch( type ) {
                case Operand . ABSOLUTE:
                    address = readWordOperand( );
                    break;
                case Operand . ABSOLUTE_X:
                    address = readWordOperand( );
                    address += register . X;
                    break;
                case Operand . ABSOLUTE_Y:
                    address = readWordOperand( );
                    address += register . Y;
                    break;
                case Operand . ACCUMULATOR:
                    register . A = _value;
                    return;
                case Operand . INDIRECT:
                    address = readWordOperand( );
                    address = memory . readWord( address );
                    break;
                case Operand . INDIRECT_X:
                    address = readWordOperand( );
                    address += register . X;
                    address = memory . readWord( address );
                    break;
                case Operand . INDIRECT_Y:
                    address = readWordOperand( );
                    address += register . Y;
                    address = memory . readWord( address );
                    break;
                case Operand . ZEROPAGE:
                    address = readByteOperand( );
                    break;
                case Operand . ZEROPAGE_X:
                    address = readByteOperand( );
                    address += register . X;
                    break;
                case Operand . ZEROPAGE_Y:
                    address = readByteOperand( );
                    address += register . Y;
                    break;
            }

            memory . writeByte( address, _value );

            // Little hackish way to instant inform SID about changes
            if ( (address & 0xfc00) == 0xd400 )
                sid . storeValue( address, _value );
        }

        public void jumpSubroutine( ushort address, byte accumulator ) {
            Reset( );

            register . A = accumulator;
            register . PC = address;

            stack . pushWord( 0 );

            while( register . PC > 0 ) {
                Step( );
            }
        }

        [OperationHandler( Operation.ADC )]
        private void handlerADC( Operand type ) {
            byte _value = readValue( type );
            int _result = _value + register . A + (register . Flags . C ? 1 : 0);

            register.Flags.C = (_result & 0x100) != 0;
            register . A = (byte) _result;

            updateFlags( register . A );
            register.Flags.V = register.Flags.C ^ register.Flags.N;

        }

        [OperationHandler( Operation.AHX )]
        private void handlerAHX( Operand type ) {
            throw new Exception( "AHX is not yet implemented." );
        }

        [OperationHandler( Operation.ALR )]
        private void handlerALR( Operand type ) {
            throw new Exception( "ALR is not yet implemented." );
        }

        [OperationHandler( Operation.ANC )]
        private void handlerANC( Operand type ) {
            throw new Exception( "ANC is not yet implemented." );
        }

        [OperationHandler( Operation.AND )]
        private void handlerAND( Operand type ) {
            byte _value1 = readValue( type );
            byte _value2 = register . A;

            register . A = ( byte )( _value1 & _value2 );
            updateFlags( register . A );
        }

        [OperationHandler( Operation.ARR )]
        private void handlerARR( Operand type ) {
            throw new Exception( "ARR is not yet implemented." );
        }

        [OperationHandler( Operation.ASL )]
        private void handlerASL( Operand type ) {
            byte _value = readValue( type, true );

            if( (_value & 0x80) != 0x0 )
                register . Flags . C = true;
            else
                register . Flags . C = false;

            _value = (byte) ( _value << 0x01 );
            storeValue( type, _value );
            updateFlags( _value );
        }

        [OperationHandler( Operation.AXS )]
        private void handlerAXS( Operand type ) {
            throw new Exception( "AXS is not yet implemented." );
        }

        [OperationHandler( Operation.BCC )]
        private void handlerBCC( Operand type ) {
            sbyte offset = readSByteOperand( );

            if( ! register . Flags . C )
                register . PC = (ushort) ( register . PC + (int) offset );
        }

        [OperationHandler( Operation.BCS )]
        private void handlerBCS( Operand type ) {
            sbyte offset = readSByteOperand( );

            if( register . Flags . C )
                register . PC = (ushort) ( register . PC + (int) offset );
        }

        [OperationHandler( Operation.BEQ )]
        private void handlerBEQ( Operand type ) {
            sbyte offset = readSByteOperand( );

            if( register . Flags . Z )
                register . PC = (ushort) ( register . PC + (int) offset );
        }

        [OperationHandler( Operation.BIT )]
        private void handlerBIT( Operand type ) {
            byte _value1 = readValue( type );
            byte _value2 = register . A;

            if( (_value1 & 0x40) != 0x0 )
                register . Flags . V = true;
            else
                register . Flags . V = false;

            if( (_value1 & 0x80) != 0x0 )
                register . Flags . N = true;
            else
                register . Flags . N = false;

            _value1 &= _value2;
            updateFlags( _value1 );
        }

        [OperationHandler( Operation.BMI )]
        private void handlerBMI( Operand type ) {
            sbyte offset = readSByteOperand( );

            if( register . Flags . N )
                register . PC = (ushort) ( register . PC + (int) offset );
        }

        [OperationHandler( Operation.BNE )]
        private void handlerBNE( Operand type ) {
            sbyte offset = readSByteOperand( );

            if( ! register . Flags . Z )
                register . PC = (ushort) ( register . PC + (int) offset );
        }

        [OperationHandler( Operation.BPL )]
        private void handlerBPL( Operand type ) {
            sbyte offset = readSByteOperand( );
            if( ! register . Flags . N )
                register . PC = (ushort) ( register . PC + (int) offset );
        }

        [OperationHandler( Operation.BRK )]
        private void handlerBRK( Operand type ) {
            stack.pushWord(register.PC);
            stack.pushFlags(register.Flags);
            register.Flags.B = true;
            register.PC = (ushort) memory.readWord(0xfffe);
        }

        [OperationHandler( Operation.BVC )]
        private void handlerBVC( Operand type ) {
            sbyte offset = readSByteOperand( );

            if( ! register . Flags . V )
                register . PC = (ushort) ( register . PC + (int) offset );
        }

        [OperationHandler( Operation.BVS )]
        private void handlerBVS( Operand type ) {
            sbyte offset = readSByteOperand( );

            if( register . Flags . V )
                register . PC = (ushort) ( register . PC + (int) offset );
        }

        [OperationHandler( Operation.CLC )]
        private void handlerCLC( Operand type ) {
            register . Flags . C = false;
        }

        [OperationHandler( Operation.CLD )]
        private void handlerCLD( Operand type ) {
            register . Flags . D = false;
        }

        [OperationHandler( Operation.CLI )]
        private void handlerCLI( Operand type ) {
            register . Flags . I = false;
        }

        [OperationHandler( Operation.CLV )]
        private void handlerCLV( Operand type ) {
            register . Flags . V = false;
        }

        [OperationHandler( Operation.CMP )]
        private void handlerCMP( Operand type ) {
            byte _value1 = register . A;
            byte _value2 = readValue( type );

            updateFlags( _value1 - _value2 );
            register . Flags . C = ( _value1 >= _value2 );
        }

        [OperationHandler( Operation.CPX )]
        private void handlerCPX( Operand type ) {
            byte _value1 = register . X;
            byte _value2 = readValue( type );

            updateFlags( _value1 - _value2 );
            register . Flags . C = ( _value1 >= _value2 );
        }

        [OperationHandler( Operation.CPY )]
        private void handlerCPY( Operand type ) {
            byte _value1 = register . Y;
            byte _value2 = readValue( type );

            updateFlags( _value1 - _value2 );
            register . Flags . C = ( _value1 >= _value2 );
        }

        [OperationHandler( Operation.DCP )]
        private void handlerDCP( Operand type ) {
            byte _value = readValue( type, true );
            _value = (byte) ( _value - 1 );

            storeValue( type, _value );
            updateFlags( _value );
            register . Flags . C = ( register . A >= _value );
        }

        [OperationHandler( Operation.DEC )]
        private void handlerDEC( Operand type ) {
            byte _value = readValue( type, true );
            _value = (byte) ( _value - 1 );

            storeValue( type, _value );
            updateFlags( _value );
        }

        [OperationHandler( Operation.DEX )]
        private void handlerDEX( Operand type ) {
            register . X --;
            updateFlags( register . X );
        }

        [OperationHandler( Operation.DEY )]
        private void handlerDEY( Operand type ) {
            register . Y --;
            updateFlags( register . Y );
        }

        [OperationHandler( Operation.EOR )]
        private void handlerEOR( Operand type ) {
            byte _value1 = readValue( type );
            byte _value2 = register . A;

            register . A = ( byte )( _value1 ^ _value2 );
            updateFlags( register . A );
        }

        [OperationHandler( Operation.INC )]
        private void handlerINC( Operand type ) {
            byte _value = readValue( type, true );
            _value = (byte) ( _value + 1 );

            storeValue( type, _value );
            updateFlags( _value );
        }

        [OperationHandler( Operation.INX )]
        private void handlerINX( Operand type ) {
            register . X ++;
            updateFlags( register . X );
        }

        [OperationHandler( Operation.INY )]
        private void handlerINY( Operand type ) {
            register . Y ++;
            updateFlags( register . Y );
        }

        [OperationHandler( Operation.ISC )]
        private void handlerISC( Operand type ) {
            throw new Exception( "ISC is not yet implemented." );
        }

        [OperationHandler( Operation.JMP )]
        private void handlerJMP( Operand type ) {
            uint address = readWordOperand( );

            if( type == Operand . INDIRECT )
                address = memory . readWord( address );

            register . PC = (ushort) address;
        }

        [OperationHandler( Operation.JSR )]
        private void handlerJSR( Operand type ) {
            uint address = readWordOperand( );
            stack . pushWord( register.PC );
            register . PC = (ushort) address;
        }

        [OperationHandler( Operation.LAS )]
        private void handlerLAS( Operand type ) {
            throw new Exception( "LAS is not yet implemented." );
        }

        [OperationHandler( Operation.LAX )]
        private void handlerLAX( Operand type ) {
            byte _value = readValue( type );

            register . A = _value;
            register . X = _value;

            updateFlags( _value );
        }

        [OperationHandler( Operation.LDA )]
        private void handlerLDA( Operand type ) {
            byte _value = readValue( type );

            register . A = _value;
            updateFlags( _value );
        }

        [OperationHandler( Operation.LDX )]
        private void handlerLDX( Operand type ) {
            byte _value = readValue( type );

            register . X = _value;
            updateFlags( _value );
        }

        [OperationHandler( Operation.LDY )]
        private void handlerLDY( Operand type ) {
            byte _value = readValue( type );

            register . Y = _value;
            updateFlags( _value );
        }

        [OperationHandler( Operation.LSR )]
        private void handlerLSR( Operand type ) {
            byte _value = readValue( type, true );

            register . Flags . C = ( ( _value & 1 ) != 0 );
            _value >>= 1;

            storeValue( type, _value );
            updateFlags( _value );
        }

        [OperationHandler( Operation.NOP )]
        private void handlerNOP( Operand type ) { }

        [OperationHandler( Operation.ORA )]
        private void handlerORA( Operand type ) {
            byte _value = readValue( type );
            register . A |= _value;

            updateFlags( register . A );
        }

        [OperationHandler( Operation.PHA )]
        private void handlerPHA( Operand type ) {
            stack . pushByte( register . A );
        }

        [OperationHandler( Operation.PHP )]
        private void handlerPHP( Operand type ) {
            stack . pushFlags( register . Flags );
        }

        [OperationHandler( Operation.PLA )]
        private void handlerPLA( Operand type ) {
            register . A = stack . popByte( );
            updateFlags( register . A );
        }

        [OperationHandler( Operation.PLP )]
        private void handlerPLP( Operand type ) {
            register . Flags = stack . popFlags( );
        }

        [OperationHandler( Operation.RLA )]
        private void handlerRLA( Operand type ) {
            throw new Exception( "RLA is not yet implemented." );
        }

        [OperationHandler( Operation.ROL )]
        private void handlerROL( Operand type ) {
            byte _value = readValue( type, true );

            _value <<= 1;
            if( register . Flags . C )
                _value |= 1;

            register . Flags . C = ( ( _value & 0x100 ) != 0x0 );

            _value &= 0xFF;
            storeValue( type, _value );
            updateFlags( _value );
        }

        [OperationHandler( Operation.ROR )]
        private void handlerROR( Operand type ) {
            byte _value = readValue( type, true );

            _value >>= 1;
            if( register . Flags . C )
                _value |= 0x80;

            register . Flags . C = ( ( _value & 1 ) != 0x0 );

            _value &= 0xFF;
            storeValue( type, _value );
            updateFlags( _value );
        }

        [OperationHandler( Operation.RRA )]
        private void handlerRRA( Operand type ) {
            throw new Exception( "RRA is not yet implemented." );
        }

        [OperationHandler( Operation.RTI )]
        private void handlerRTI( Operand type ) {
            handlerRTS(type);
        }

        [OperationHandler( Operation.RTS )]
        private void handlerRTS( Operand type ) {
            register.PC = (ushort) stack.popWord( );
        }

        [OperationHandler( Operation.SAX )]
        private void handlerSAX( Operand type ) {
            throw new Exception( "SAX is not yet implemented." );
        }

        [OperationHandler( Operation.SBC )]
        private void handlerSBC( Operand type ) {
            byte _value = readValue( type );
            _value ^= 0xff;
            int _result = _value + register . A + (register . Flags . C ? 1 : 0);

            register.Flags.C = (_result & 0x100) != 0;
            register . A = (byte) _result;

            updateFlags( register . A );
            register.Flags.V = register.Flags.C ^ register.Flags.N;
        }

        [OperationHandler( Operation.SEC )]
        private void handlerSEC( Operand type ) {
            register . Flags . C = true;
        }

        [OperationHandler( Operation.SED )]
        private void handlerSED( Operand type ) {
            register . Flags . D = true;
        }

        [OperationHandler( Operation.SEI )]
        private void handlerSEI( Operand type ) {
            register . Flags . I = true;
        }

        [OperationHandler( Operation.SHX )]
        private void handlerSHX( Operand type ) {
            throw new Exception( "SHX is not yet implemented." );
        }

        [OperationHandler( Operation.SHY )]
        private void handlerSHY( Operand type ) {
            throw new Exception( "SHY is not yet implemented." );
        }

        [OperationHandler( Operation.SLO )]
        private void handlerSLO( Operand type ) {
            Console.WriteLine("Shit happened!");
            while (true) { }
            throw new Exception( "SLO is not yet implemented." );
        }

        [OperationHandler( Operation.SRE )]
        private void handlerSRE( Operand type ) {
            throw new Exception( "SRE is not yet implemented." );
        }

        [OperationHandler( Operation.STA )]
        private void handlerSTA( Operand type ) {
            storeValue( type, register . A );
        }

        [OperationHandler( Operation.STX )]
        private void handlerSTX( Operand type ) {
            storeValue( type, register . X );
        }

        [OperationHandler( Operation.STY )]
        private void handlerSTY( Operand type ) {
            storeValue( type, register . Y );
        }

        [OperationHandler( Operation.TAS )]
        private void handlerTAS( Operand type ) {
            throw new Exception( "TAS is not yet implemented." );
        }

        [OperationHandler( Operation.TAX )]
        private void handlerTAX( Operand type ) {
            register.X = register.A;
            updateFlags(register.X);
        }

        [OperationHandler( Operation.TAY )]
        private void handlerTAY( Operand type ) {
            register.Y = register.A;
            updateFlags(register.Y);
        }

        [OperationHandler( Operation.TSX )]
        private void handlerTSX( Operand type ) {
            throw new Exception( "TSX is not yet implemented." );
        }

        [OperationHandler( Operation.TXA )]
        private void handlerTXA( Operand type ) {
            register . A = register . X;
            updateFlags( register . A );
        }

        [OperationHandler( Operation.TXS )]
        private void handlerTXS( Operand type ) {
            stack . setStackPointer( register . X );
        }

        [OperationHandler( Operation.TYA )]
        private void handlerTYA( Operand type ) {
            register . A = register . Y;
            updateFlags( register . A );
        }

        [OperationHandler( Operation.XAA )]
        private void handlerXAA( Operand type ) {
            throw new Exception( "XAA is not yet implemented." );
        }
    }
}

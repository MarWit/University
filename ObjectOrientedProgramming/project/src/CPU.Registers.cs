namespace SIDious {
    partial class CPU {
        /**
         * Struktura opisująca rejestry w procesorze
         */
        public struct SRegister {
            /**
             * Struktura opicująca flagi w procesorze.
             */
            public struct SFlags {
                public bool N;     // Negative flag
                public bool V;     // Overflow flag
                public bool B;     // Break flag
                public bool D;     // Decimal mode flag
                public bool I;     // Interrupt disable flag
                public bool Z;     // Zero flag
                public bool C;     // Carry flag
            }

            public ushort PC;      // Program counter
            public SFlags Flags;   // Flags
            public byte A;         // Accumulator
            public byte X;         // Index register
            public byte Y;         // Index register
        }

        SRegister register;
    }
}

using System;
using Zadanie3;

namespace Test3 {
    public class Program {
        public static int Main( string[ ] args ) {
            for( int i = 0; i < 10; i ++ )
                Console.WriteLine( TimeNTon . Instance( ) );

            return 0;
        }
    }
}

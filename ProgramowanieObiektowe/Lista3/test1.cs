using System;
using Zadanie1;

namespace Test1 {
    public class Program {
        public static int Main( string[ ] args ) {

            String xd = "ABCD";
            Lista<String> myList = new Lista<String>();

            Console.WriteLine( "Is Empty? : {0}", myList.Empty() );
            myList.Push( xd );

            Console.WriteLine( "Is Empty? : {0}", myList.Empty() );
            myList.Push( "ABCDEF" );

            Console.WriteLine( "First: {0}", myList.first );
            Console.WriteLine( "Last: {0}", myList.last );

            myList.Push( "TEST TEST TEST" );

            Console.WriteLine( "First: {0}", myList.first );
            Console.WriteLine( "Last: {0}", myList.last );

            myList.Unshift( "IMMA PREPENDING!!!" );

            Console.WriteLine( "First: {0}", myList.first );
            Console.WriteLine( "Last: {0}", myList.last );

            Console.WriteLine( "Last: {0}", myList.Pop( ) );
            Console.WriteLine( "Last: {0}", myList.last );

            Console.WriteLine( "First: {0}", myList.Shift( ) );
            Console.WriteLine( "First: {0}", myList.first );

            return 0;
        }
    }
}

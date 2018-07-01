using System;

namespace Zadanie1 {
    public class IntStream {
        protected uint number;

        public IntStream( ) {
            number = 0;
        }

        virtual public int next( ) {
            return number ++;
        }

        virtual public bool eos( ) {
            return number >= UInt32.MaxValue;
        }

        public void reset( ) {
            number = 0;
        }
    }

    public class PrimeStream : IntStream {
        private static bool isPrime( int num ) {
            if( num == 0 || num == 1 ) return false;
            if( num == 2 ) return true;

            int b = ( int ) Math.Floor( Math.Sqrt( num ) );
            for( int i = 2; i <= b; i ++ )
                if( num % i == 0 ) return false;

            return true;
        }

        override public int next( ) {
            while( number < UInt32.MaxValue ) {
                base.next( );

                if( isPrime( number ) )
                    return number;
            }

            return 1;
        }
    }

    public class RandomStream : IntStream {
        private Random rand;

        public RandomStream( ) {
            rand = new Random( );
        }

        override public int next( ) {
            return rand.Next( 1, Int32.MaxValue );
        }
    }

    public class RandomWordStream {
        private PrimeStream ps = new PrimeStream( );
        private RandomStream rs = new RandomStream( );

        public RandomWordStream( ) {}

        public string next( ) {
            int len = ps.next( );
            char[ ] ret = new char[ len ];

            for( int i = 0; i < len; i ++ )
                ret[ i ] = ( char ) ( 65 + rs.next( ) % 25 );

            return new string( ret );
        }
    }

    class Program {
        static void Main( ) {
            {
                Console.WriteLine( "Dziesięć pierwszych liczb naturalnych:" );

                IntStream is_ = new IntStream( );

                for( int i = 0; i < 10; i ++ ) {
                    Console.Write( is_.next( ) );
                    Console.Write( " " );
                }

                Console.WriteLine( "\n" );
            }

            {
                Console.WriteLine( "Dziesięć pierwszych liczb pierwszych:" );

                PrimeStream ps = new PrimeStream( );

                for( int i = 0; i < 10; i ++ ) {
                    Console.Write( ps.next( ) );
                    Console.Write( " " );
                }

                Console.WriteLine( "\n" );
            }

            {
                Console.WriteLine( "Dziesięć losowych stringów o długościach równym kolejnym liczbą pierwszym:" );

                RandomWordStream rs = new RandomWordStream( );

                for( int i = 0; i < 10; i ++ ) {
                    Console.WriteLine( rs.next( ) );
                }
            }
        }
    }
}

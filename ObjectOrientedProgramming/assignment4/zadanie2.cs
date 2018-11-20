using System;
using System.Collections;

namespace Zadanie2 {
    class PrimeCollectionEnum : IEnumerator {
        PrimeCollection p;

        public PrimeCollectionEnum( PrimeCollection p ) {
            this . p = p;
        }

        public bool MoveNext( ) {
            try {
                p . next( );
                return true;
            } catch( Exception ) {
                return false;
            }
        }

        public void Reset( ) {
            p . reset( );
        }

        object IEnumerator.Current {
            get { return Current; }
        }

        public uint Current {
            get { return p .number; }
        }

    }

    class PrimeCollection : IEnumerable {
        public uint number;

        public PrimeCollection() {}

        private static bool isPrime( uint num ) {
            if( num == 0 || num == 1 ) return false;
            if( num == 2 ) return true;

            uint b = ( uint ) Math.Floor( Math.Sqrt( num ) );
            for( uint i = 2; i <= b; i ++ )
                if( num % i == 0 ) return false;

            return true;
        }

        public void reset( ) {
            number = 0;
        }

        public uint next( ) {
            while( number < UInt32.MaxValue ) {
                number ++;

                if( isPrime( number ) )
                    return number;
            }

            throw new Exception( "End of numbers." );
        }

        IEnumerator IEnumerable.GetEnumerator( ) {
            return ( IEnumerator ) GetEnumerator( );
        }

        public PrimeCollectionEnum GetEnumerator( ) {
            return new PrimeCollectionEnum( this );
        }
    }

    class Program {
        public static void Main(string[] args) {
            PrimeCollection pc = new PrimeCollection( );

            foreach( int n in pc ) {
                Console.WriteLine(n);
            }

        }
    }
}

using System;
using System.Collections;

namespace Zadanie1 {
    public interface INumberCollection {
        void Reset();
        uint Length();
        uint Next();
        bool Empty();
    }

    public class IntegerCollectionEnum : IEnumerator {
        IntegerCollection p;

        public IntegerCollectionEnum( IntegerCollection p ) {
            this . p = p;
        }

        public bool MoveNext( ) {
            try {
                p . Next( );
                return true;
            } catch( Exception ) {
                return false;
            }
        }

        public void Reset( ) {
            p . Reset( );
        }

        object IEnumerator.Current {
            get { return Current; }
        }

        public uint Current {
            get { return p . number; }
        }

    }

    public class IntegerCollection : INumberCollection, IEnumerable {
        public uint number;

        public IntegerCollection( ) {
            number = 0;
        }

        public uint Next( ) {
            return number ++;
        }

        uint INumberCollection.Next( ) {
            return Next( );
        }

        public bool Empty( ) {
            return false;
        }

        bool INumberCollection.Empty( ) {
            return Empty( );
        }

        public void Reset( ) {
            number = 0;
        }

        void INumberCollection.Reset( ) {
            Reset( );
        }

        public uint Length {
            get { return UInt32.MaxValue; }
        }

        public override string ToString() {
            return number.ToString( );
        }

        uint INumberCollection.Length( ) {
            return Length;
        }

        public uint this[ uint idx ] {
            get { return idx; }
        }

        IEnumerator IEnumerable.GetEnumerator( ) {
            return ( IEnumerator ) GetEnumerator( );
        }

        public IntegerCollectionEnum GetEnumerator( ) {
            return new IntegerCollectionEnum( this );
        }
    }


    class PrimeCollection : INumberCollection {
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

        public void Reset( ) {
            number = 0;
        }

        void INumberCollection.Reset( ) {
            Reset( );
        }

        public bool Empty( ) {
            return false;
        }

        bool INumberCollection.Empty( ) {
            return Empty( );
        }

        public uint Next( ) {
            while( number < UInt32.MaxValue ) {
                number ++;

                if( isPrime( number ) )
                    return number;
            }

            throw new Exception( "End of numbers." );
        }

        uint INumberCollection.Next( ) {
            return Next( );
        }

        public uint Length( ) {
            return 203280221; // Number of primes between 2 and 4294967295.
        }

        uint INumberCollection.Length( ) {
            return Length( );
        }
    }

    class Program {
        public static void Main(string[] args) {
            IntegerCollection ic = new IntegerCollection( );

            foreach( uint i in ic ) {
                Console.WriteLine(i);
            }
        }
    }
}

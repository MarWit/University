using System;
using System.Linq;

namespace Zadanie3 {
    class BigInt {
        private string number;
        private bool negative;

        public static readonly BigInt biZero    = new BigInt( 0 );
        public static readonly BigInt biOne     = new BigInt( 1 );

        public BigInt( string start, bool negative ) {
            number = start;
            this.negative = negative;
        }

        public BigInt( int start ) {
            negative = false;

            if( start < 0 ) {
                start *= -1;
                negative = true;
            }

            number = start.ToString( );
        }

        public static bool operator ==( BigInt a, BigInt b ) {
            if( a.number != b.number || a.negative != b.negative ) return false;
            return true;
        }

        public static bool operator !=( BigInt a, BigInt b ) {
            return !( a == b );
        }

        public static bool operator >( BigInt a, BigInt b ) {
            if( ! a.negative && b.negative ) return true;
            if( a.negative && ! b.negative ) return false;

            if( ! a.negative ) {
                if( a.number.Length > b.number.Length ) return true;
                if( b.number.Length > a.number.Length ) return false;

                for( int i = a.number.Length - 1; i >= 0; i -- )
                    if( b.number[ i ] > a.number[ i ] ) return false;

            } else {
                if( b.number.Length > a.number.Length ) return true;
                if( a.number.Length > b.number.Length ) return false;

                for( int i = a.number.Length - 1; i >= 0; i -- )
                    if( b.number[ i ] < a.number[ i ] ) return false;

            }

            return true;
        }

        public static bool operator <( BigInt a, BigInt b ) {
            return b > a;
        }

        public static BigInt operator +( BigInt a, BigInt b ) {
            string min, max;

            if( a.number.Length > b.number.Length ) {
                max = a.number;
                min = b.number;
            } else {
                max = b.number;
                min = a.number;
            }

            char[ ] ret = new char[ max.Length + 1 ];
            int carry = 0;
            int diff = max.Length - min.Length;
            int i, num;

            for( i = min.Length - 1; i >= 0; i -- ) {
                num = ( int )( min[ i ] - '0' ) +
                      ( int )( max[ i + diff ] - '0' );

                num += carry;
                ret[ i + diff + 1 ] = ( char )( '0' + num % 10 );
                carry = num / 10;
            }

            for( i = max.Length - min.Length - 1; i >= 0; i -- ) {
                num = ( int )( max[ i ] - '0' ) + carry;
                if( num < 0 ) continue;

                ret[ i ] = ( char )( '0' + num % 10 );
                carry = num / 10;
            }

            if( carry > 0 )
                ret[ 0 ] = ( char )( '0' + carry );

            return new BigInt( new string( ret ), a.negative ^ b.negative );
        }

        public static BigInt operator -( BigInt a, BigInt b ) {
            char[] min, max;
            bool negative;

            if( a > b ) {
                max = a.number.ToArray( );
                min = b.number.ToArray( );

                negative = false;
            } else {
                max = b.number.ToArray( );
                min = a.number.ToArray( );

                negative = true;
            }

            char[ ] ret = new char[ max.Length ];
            int diff = max.Length - min.Length;

            for( int i = min.Length - 1; i >= 0; i -- ) {
                if( min[ i ] > max[ i + diff ] ) {
                    max[ i + diff - 1 ] --;
                    max[ i + diff ] += ( char ) 10;
                }

                ret[ i + diff ] = ( char )( max[ i + diff ] - min[ i ] + '0' );
            }

            while( diff -- > 0 ) {
                ret[ diff ] = max[ diff ];
            }

            return new BigInt( new string( ret ).TrimStart( '0' ), negative );
        }

        public static BigInt operator *( BigInt a, BigInt b ) {
            string min, max;

            if( a.number.Length > b.number.Length ) {
                max = a.number;
                min = b.number;
            } else {
                max = b.number;
                min = a.number;
            }

            char[ ] ret = Enumerable.Repeat( '0', max.Length + min.Length ).ToArray( );
            int carry = 0;
            int i, j, num;

            for( j = min.Length - 1, carry = 0; j >= 0; j --, carry = 0 ) {
                for( i = max.Length - 1; i >= 0; i -- ) {
                    num = ( int )( min[ j ] - '0' ) *
                          ( int )( max[ i ] - '0' );

                    num += carry + ret[ i + j + 1 ] - '0';
                    ret[ j + i + 1 ] = ( char )( '0' + num % 10 );
                    carry = num / 10;
                }

                ret[ j + i + 1 ] += ( char ) carry;
            }

            return new BigInt( new string( ret ).TrimStart( '0' ), a.negative ^ b.negative );
        }

        public static BigInt operator /( BigInt a, BigInt b ) {
            if( a == b ) return biOne;
            if( a < b ) return biZero;

            BigInt buf = b;
            BigInt ret = new BigInt( 1 );

            while( true ) {
                if( buf > a ) break;
                buf = buf + b;
                ret += biOne;
            }

            return ret;
        }

        override public string ToString( ) {
            return ( negative ? "-" : "" ) + number;
        }

    }

    class Program {

        public static void Main( ) {
            BigInt a = new BigInt( 1372 );
            BigInt b = new BigInt( 373 );

            BigInt c = a + b;
            Console.WriteLine( string.Format( "{0} + {1} = {2}", a, b, c ) );

            c = a * b;
            Console.WriteLine( string.Format( "{0} * {1} = {2}", a, b, c ) );

            Console.Write( string.Format( "{0} * {0} = ", c ) );
            c = c * c;
            Console.WriteLine( c.ToString( ) );

            Console.Write( string.Format( "{0} * {0} = ", c ) );
            c = c * c;
            Console.WriteLine( c.ToString( ) );

            Console.Write( string.Format( "{0} * {0} = ", c ) );
            c = c * c;
            Console.WriteLine( c.ToString( ) );

            Console.Write( string.Format( "{0} * {0} = ", c ) );
            c = c * c;
            Console.WriteLine( c.ToString( ) );

            Console.Write( string.Format( "{0} * {0} = ", c ) );
            c = c * c;
            Console.WriteLine( c.ToString( ) );

            c = a - b;
            Console.WriteLine( string.Format( "{0} - {1} = {2}", a, b, c ) );

            c = a / b;
            Console.WriteLine( string.Format( "{0} / {1} = {2}", a, b, c ) );
        }
    }
}

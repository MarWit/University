using System;
using System.Linq;
using System.Text;

namespace Zadanie4 {
    public class Vector {
        private float[ ] values;

        public int Length {
            get { return values.Length; }
        }

        public Vector( int dimension ) {
            values = new float[ dimension ];
        }

        public Vector( float[ ] values ) {
            this . values = values;
        }

        public static Vector operator +( Vector a, Vector b ) {
            if( a . Length != b . Length )
                throw new Exception( "Wektory są w innych wymiarach." );

            Vector ret = a;
            for( int i = 0; i < b . Length; i ++ )
                ret . values[ i ] += b . values [ i ];

            return ret;
        }

        public static Vector operator -( Vector a, Vector b ) {
            if( a . Length != b . Length )
                throw new Exception( "Wektory są w innych wymiarach." );

            Vector ret = a;
            for( int i = 0; i < b . Length; i ++ )
                ret . values[ i ] -= b . values[ i ];

            return ret;
        }

        public static Vector operator *( float a, Vector v ) {
            Vector ret = v;
            for( int i = 0; i < v . Length; i ++ )
                ret . values[ i ] *= a;

            return ret;
        }

        public float this[ int idx ] {
            get { return values[ idx ]; }
            set{ values[ idx ] = value; }
        }

        public override string ToString( ) {
            return "(" + String.Join( ", ", values.Select( i => i.ToString( ) ).ToArray( ) ) + ")";
        }

        public float[ ] ToArray( ) {
            return values;
        }

    }

    public class Matrix {
        Vector[ ] vectors;

        public int height {
            get { return vectors . Length; }
        }

        public int width {
            get { return vectors[ 0 ] . Length; }
        }

        public Matrix( int i, int j ) {
            vectors = new Vector[ i ];
            for( int p = 0; p < i; p ++ )
                vectors[ p ] = new Vector( j );
        }

        public Matrix( Vector[ ] vectors ) {
            this . vectors = vectors;
        }

        public static Matrix operator +( Matrix a, Matrix b ) {
            if( a . width != b . width || a . height != b . height )
                throw new Exception( "Macierze mają inne rozmiary." );

            Matrix ret = a;

            for( int i = 0; i < a . vectors . Length; i ++ )
                ret . vectors[ i ] += b . vectors[ i ];

            return ret;
        }

        public static Matrix operator *( Matrix a, Matrix b ) {
            if( a . width != b . height )
                throw new Exception( "" );

            Matrix ret = new Matrix( a . height, b . width );
            for( int row = 0; row < a . height; row ++ )
                for( int col = 0; col < b . width; col ++ )
                    for( int inner = 0; inner < a . width; inner ++ )
                        ret . vectors[ row ][ col ] +=
                            a . vectors[ row ][ inner ] *
                            b . vectors[ inner ][ col ];

            return ret;
        }

        // public static Matrix operator *( Vector v, Matrix m ) {
        //     Matrix r = new Matrix( v . Length, 1 )
        // }

        public override string ToString( ) {
            StringBuilder r = new StringBuilder( );

            for( int i = 0; i < vectors . Length; i ++ )
                r.Append(
                    "|" +
                    String.Join( " ", vectors[ i ].ToArray( ).Select( p => p.ToString( ) ).ToArray( ) ) +
                    "|\n"
                );

            return r . ToString( );
         }
    }

    public class Program {
        public static int Main( string[ ] args ) {
            Vector v = new Vector( new float[ ] { 0, 1 ,3, 2, 4} );
            Vector u = new Vector( new float[ ] { 5, 6, 3, 1, 3} );
            Vector x = new Vector( 5 );

            Console.WriteLine(v);
            Console.WriteLine(u);
            Console.WriteLine(x);
            Console.WriteLine(v + u);
            Console.WriteLine(v + x);

            Matrix m = new Matrix( new Vector[ ] { v, u } );
            Matrix n = new Matrix( new Vector[ ] {
                new Vector( new float[ ] { 4,  5 } ),
                new Vector( new float[ ] { 8,  1 } ),
                new Vector( new float[ ] { 0,  3 } ),
                new Vector( new float[ ] { -4, 2 } ),
                new Vector( new float[ ] { 9, 12 } ),
            } );

            Console.WriteLine( "" );
            Console.WriteLine( m );
            Console.WriteLine( "" );
            Console.WriteLine( n );
            Console.WriteLine( "" );
            Console.WriteLine( m * n );



            return 0;
        }
    }
}

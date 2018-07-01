using System;

namespace SIDious {
    /**
     * Klasa pozwalająca na 'zmapowanie' tablicy na parę części
     */
    public class ArrayChunk<T> {
        T[] array;
        int offset;
        int size;

        public ArrayChunk( T[] a, int start, int end ) {
            array = a;
            this . offset = start;
            this . size = end - start;
        }

        public T this[ int key ] {
            get {
                if( key > size )
                    throw new ArgumentOutOfRangeException( );

                return array[ offset + key ];
            }

            set {
                if( key > size )
                    throw new ArgumentOutOfRangeException( );

                array[ offset + key ] = value;

            }
        }
    }
}

using System;
using System.Collections;

namespace Zadanie3 {
    public class List<T> {
        private class ListElement<U> {
            public U Value;
            public ListElement<U> Next;

            public ListElement( U element ) {
                Value = element;
            }

            public ListElement( U element, ListElement<U> Next ) {
                Value = element;
                this . Next = Next;
            }

            public override String ToString( ) {
                return Value.ToString( );
            }
        }

        private ListElement<T> head;
        private ListElement<T> current;
        private uint size;

        public uint Count {
            get { return size; }
        }

        public List( ) {  }
        public List( T[] elements ) {
            foreach( T e in elements )
                Add( e );
        }

        public void Add( T element ) {
            if( head == null ) {
                head = new ListElement<T>( element );
                current = head;
            } else
                current = new ListElement<T>( element, current );

            size ++;
        }

        public void Delete( uint idx ) {
            ListElement<T> query;

            if( idx == 0 ) {
                query = head;
                if( query . Next != null )
                    head = query . Next;
                else
                    head = current = null;
            } else {
                ListElement<T> before = Find( idx - 1 );
                query = Find( idx );

                if( current == query )
                    current = before;

                before . Next = query . Next;
            }

            size --;
        }

        private ListElement<T> Find( uint idx ) {
            if( head == null )
                throw new InvalidOperationException( "List is empty." );

            if( idx < 0 || idx > size )
                throw new IndexOutOfRangeException( );

            ListElement<T> query = head;
            while( idx -- > 0 ) query = query . Next;
            return query;
        }

        public T this[ uint idx ] {
            get {
                return Find( idx ) . Value;
            }

            set {
                ListElement<T> query = Find( idx );
                query . Value = value;
            }
        }
    }

    public class NeighborGraph {
        private class NeighborGraphEdge {
            private Vertex Node1;
            private Vertex Node2;
        }

        private class NeighborGraphVertex {
            private string Label;
            private List<Edge> neighbors;
        }

    }

    class Program {
        public static void Main(string[] args) {
        }
    }
}

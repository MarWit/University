using System;

namespace Zadanie1 {
    public class Lista<T> {
        private class Element<U> {
            public U value { get; set; }
            public Element<U> prev { get; set; }
            public Element<U> next { get; set; }

            public Element( ) { }

            public Element( U value, Element<U> prev ) {
                this . value = value;
                this . prev = prev;
            }

            public Element( U value, Element<U> prev, Element<U> next ) {
                this . value = value;
                this . prev = prev;
                this . next = next;
            }
        }

        private Element<T> tail;
        private Element<T> head;

        public T first {
            get {
                return head.value;
            }

            set {
                head.value = value;
            }
        }

        public T last {
            get {
                return tail.value;
            }

            set {
                tail.value = value;
            }
        }

        public uint Length { get; private set; }

        public Lista( ) {
            tail = new Element<T>();
            head = tail;
        }

        public void Push( T e ) {
            if( this . Empty( ) )
                tail . value = e;
            else
                tail = new Element<T>( e, tail );

            Length ++;
        }

        public T Pop( ) {
            Length --;

            T ret = last;
            tail = tail . prev;

            return ret;
        }

        public void Unshift( T e ) {
            if( this . Empty( ) )
                head . value = e;
            else {
                head . prev = new Element<T>( );
                head . prev . value = e;
                head . prev . next = head;
                head = head . prev;
            }

            Length ++;
        }

        public T Shift( ) {
            if( this . Empty() )
                throw Exception( "List is empty." );

            Length --;

            T ret = first;
            if( Length > 1 )
                head = head . next;

            return ret;
        }


        public bool Empty( ) {
            return Length == 0;
        }
    }
}

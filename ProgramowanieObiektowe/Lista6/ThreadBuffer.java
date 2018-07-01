package lista6;

public class ThreadBuffer<T> {
    class ThreadBufferElement {
        T value;
        ThreadBufferElement next;

        ThreadBufferElement( T value ) {
            this . value = value;
            this . next = next;
        }
    }

    private ThreadBufferElement head;
    private ThreadBufferElement tail;
    private int size;
    private int count;
    private boolean closed = false;

    ThreadBuffer( int size ) {
        this . size = size;
    }

    public synchronized void Push( T element ) {
        while( this . Full( )  ) {
            try {
                wait( );
            } catch( InterruptedException e ) {
                e . printStackTrace( );
            }
        }

        this . count ++;

        if( this . head == null )
            this . head = this . tail = new ThreadBufferElement( element );
        else if( this . head == this . tail )
            this . head . next = this . tail = new ThreadBufferElement( element );
        else {
            this . tail . next = new ThreadBufferElement( element );
            this . tail = this . tail . next;
        }

        notifyAll( );
    }

    public synchronized T Get( ) {
        if( this . Empty( ) ) {
            try {
                wait( );
            } catch( InterruptedException e ) {
                e . printStackTrace( );
            }
        }

        this . count --;

        T ret = this . head . value;

        if( this . head == this . tail )
            this . tail = null;

        this . head = this . head . next;

        notifyAll( );

        return ret;
    }

    public synchronized void close( ) {
        closed = true;
    }

    public synchronized boolean isClosed( ) {
        return closed;
    }

    public synchronized boolean Empty( ) {
        return this . count == 0;
    }

    public synchronized boolean Full( ) {
        return this . count == this . size;
    }
}

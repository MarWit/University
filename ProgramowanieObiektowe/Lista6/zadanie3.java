package lista6;

import java.util.*;

class StringFactory implements Runnable {
    private Thread t;
    private String threadName;
    private ThreadBuffer<String> buffer;

    StringFactory( String threadName, ThreadBuffer<String> buffer ) {
        this . threadName = threadName;
        this . buffer = buffer;
    }

    public void run( ) {
        for( ;; )
            buffer.Push( UUID.randomUUID( ).toString( ) );
    }

    public void start( ) {
        if( t == null ) {
            t = new Thread( this, threadName );
            t . start( );
        }
    }
}

class StringEater implements Runnable {
    private Thread t;
    private String threadName;
    private ThreadBuffer<String> buffer;

    StringEater( String threadName, ThreadBuffer<String> buffer ) {
        this . threadName = threadName;
        this . buffer = buffer;
    }

    public void run( ) {
        for( ;; )
            System.out.println( buffer . Get( ) );
    }

    public void start( ) {
        if( t == null ) {
            t = new Thread( this, threadName );
            t . start( );
        }
    }
}

public class zadanie3 {
    public static void main(String args[]) {
        ThreadBuffer<String> tb = new ThreadBuffer<String>( 15 );

        StringFactory sf = new StringFactory( "SF", tb );
        StringEater se = new StringEater( "SE", tb );

        sf . start( );
        se . start( );
    }
}

package lista6;

import java.util.*;
import java.text.SimpleDateFormat;

interface PersonFilter {
    boolean filter( Person p );
}

class Person {
    public String firstName;
    public String lastName;
    public String birthCity = "-- UNKNOWN --";

    private Calendar _birthDate = Calendar.getInstance( );

    final SimpleDateFormat dateFormat = new SimpleDateFormat( "dd.MM.yyyy" );

    Person( String firstName, String lastName, int day, int month, int year ) {
        this . firstName = firstName;
        this . lastName = lastName;

        this . birthDate( day, month, year );
    }

    Person( String firstName, String lastName, int day, int month, int year, String birthCity ) {
        this . firstName = firstName;
        this . lastName = lastName;
        this . birthCity = birthCity;

        this . birthDate( day, month, year );
    }

    public void birthDate( int day, int month, int year ) {
        _birthDate.set( year, month - 1, day );
    }

    public Date birthDate( ) {
        return _birthDate . getTime( );
    }

    public long age( ) {
        return ( new Date( ).getTime( ) - _birthDate.getTime( ).getTime( ) ) / 31536000000L;
    }

    public String toString( ) {
        return String.format( "%s %s, %s years old, born at %s in %s.",
                firstName,
                lastName,
                age( ),
                dateFormat.format( birthDate( ) ),
                birthCity
        );
    }
}

class FilterThread implements Runnable {
    private Thread t;
    private String threadName;
    private ThreadBuffer<Person> buffer;
    private PersonFilter personFilter;
    private Person[] peopleList;

    FilterThread( String threadName, ThreadBuffer<Person> buffer, PersonFilter filter ) {
        this . threadName = threadName;
        this . buffer = buffer;
        this . personFilter = filter;
    }

    public void setPeopleList( Person[] peopleList ) {
        this . peopleList = peopleList;
    }

    public void run( ) {
        for( Person p : peopleList ) {
            if( personFilter . filter( p ) ) {
                buffer . Push( p );
            }
        }

        buffer . close( );
    }

    public void start( ) {
        if( t == null ) {
            t = new Thread( this, threadName );
            t . start( );
        }
    }
}

class CollectorThread implements Runnable {
    private Thread t;
    private String threadName;
    private ThreadBuffer<Person> buffer;

    public enum SortBy {
        FIRST_NAME,
        LAST_NAME,
        AGE,
        BIRTH_CITY
    }

    private SortBy sortBy;

    CollectorThread( String threadName, ThreadBuffer<Person> buffer, SortBy sortBy ) {
        this . threadName = threadName;
        this . buffer = buffer;
        this . sortBy = sortBy;
    }

    private boolean isBefore( Person p, Person what ) {
        switch( sortBy ) {
            case FIRST_NAME:
                return p . firstName . compareTo( what . firstName ) <= 0;
            case LAST_NAME:
                return p . lastName . compareTo( what . lastName ) <= 0;
            case AGE:
                return p . age( ) <= what . age( );
            case BIRTH_CITY: {
                return p . birthCity . compareTo( what . birthCity ) <= 0;
            }
        }

        return false;
    }

    public void run( ) {
        LinkedList<Person> sorted = new LinkedList<Person>( );
        int i;

        while( ! buffer . Empty( ) || ! buffer . isClosed( ) ) {
            Person p = buffer . Get( );
            i = 0;

            for( Person what : sorted ) {
                if( isBefore( p, what ) )
                    break;

                i ++;
            }

            sorted . add( i, p );
        }

        for( Person who : sorted )
            System.out.println( who );
    }

    public void start( ) {
        if( t == null ) {
            t = new Thread( this, threadName );
            t . start( );
        }
    }
}

class zadanie4 {
    public static void main(String args[]) {
        ThreadBuffer<Person> peopleBuffer = new ThreadBuffer<Person>( 25 );

        PersonFilter f = (p) -> {
            return p . age( ) < 25;
        };

        Person[] peopleList = {
            new Person( "Gloria", "White", 4, 3, 1951, "Baton Rouge" ),
            new Person( "James", "Blackwell", 10, 9, 1997, "Chicago" ),
            new Person( "Amy", "King", 8, 12, 1985, "New York" ),
            new Person( "Rosa", "Atkinson", 4, 7, 2005 ),
            new Person( "Regina", "Dennis", 15, 7, 1992, "Tuscon" ),
            new Person( "Morton", "Tyler", 22, 3, 2001, "Waldorf" ),
            new Person( "Wayne", "Downard", 18, 8, 1958 ),
            new Person( "Daniel", "Tran", 28, 9, 1999, "Midland" ),
            new Person( "Adam", "Whitehurst", 6, 6, 1966, "Fort Mill" ),
            new Person( "Doris", "Barret", 12, 12, 1945, "Rockville" )
        };

        FilterThread ft = new FilterThread( "FT", peopleBuffer, f );
        ft . setPeopleList( peopleList );
        ft . start( );

        CollectorThread ct = new CollectorThread( "CT", peopleBuffer, CollectorThread . SortBy . AGE );
        ct . start( );
    }
}

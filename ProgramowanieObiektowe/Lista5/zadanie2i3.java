package lista5;

import java.util.*;


// TODO: Zrobić pochodne cząstkowe

class Wyrazenie {
    public int oblicz( ) {
        return 0;
    }

    public Wyrazenie negative( ) {
        return this;
    }

    public Wyrazenie derivative( ) {
        return this;
    }

    public String toString( ) {
        return String.valueOf( 0 );
    }

    public void setVarValue( String var, int value ) {};
}

class Stala extends Wyrazenie {
    int value;

    Stala( int value ) {
        this.value = value;
    }

    public int oblicz( ) {
        return value;
    }

    public Wyrazenie negative( ) {
        return new Stala( -value );
    }

    public Wyrazenie derivative( ) {
        return new Stala( 0 );
    }

    public String toString( ) {
        return String.valueOf( value );
    }
}

class Zmienna extends Wyrazenie {
    String var;
    int value;

    Zmienna( String var ) {
        this.var = var;
    }

    public int oblicz( ) {
        return value;
    }

    public Wyrazenie derivative( ) {
        return new Stala( 1 );
    }

    public void setVarValue( String var, int value ) {
        if( var == this.var )
            this.value = value;
    }

    public String toString( ) {
        return var;
    }
}

class Operator extends Wyrazenie {
    Wyrazenie a;
    Wyrazenie b;

    Operator( Wyrazenie a, Wyrazenie b ) {
        this.a = a;
        this.b = b;
    }

    public Wyrazenie derivative( ) {
        return new Operator( a.derivative( ), b.derivative( ) );
    }

    public void setVarValue( String var, int value ) {
        this.a.setVarValue( var, value );
        this.b.setVarValue( var, value );
    }
}

class Dodaj extends Operator {
    Dodaj( Wyrazenie a, Wyrazenie b ) {
        super( a, b );
    }

    public int oblicz( ) {
        return a.oblicz( ) + b.oblicz( );
    }

    public Wyrazenie derivative( ) {
        return new Dodaj( a.derivative( ), b.derivative( ) );
    }

    public String toString( ) {
        return a.toString( ) + " + " + b.toString( );
    }
}

class Odejmij extends Operator {
    Odejmij( Wyrazenie a, Wyrazenie b ) {
        super( a, b );
    }

    public int oblicz( ) {
        return a.oblicz( ) - b.oblicz( );
    }

    public Wyrazenie derivative( ) {
        return new Odejmij( a.derivative( ), b.derivative( ) );
    }

    public String toString( ) {
        return a.toString( ) + " - " + b.toString( );
    }

}

class Pomnoz extends Operator {
    Pomnoz( Wyrazenie a, Wyrazenie b ) {
        super( a, b );
    }

    public int oblicz( ) {
        return a.oblicz( ) * b.oblicz( );
    }

    public Wyrazenie derivative( ) {
        if( a instanceof Zmienna &&
            b instanceof Zmienna
        ) return new Pomnoz( new Stala( 2 ), a );

        if( a instanceof Stala &&
            b instanceof Zmienna
        ) return a;

        if( a instanceof Zmienna &&
            b instanceof Stala
        ) return b;

        return new Dodaj(
            new Pomnoz( a.derivative( ), b ),
            new Pomnoz( a, b.derivative( ) )
        );
    }

    public String toString( ) {
        String a_str;
        String b_str;

        if( a instanceof Zmienna || a instanceof Stala )
            a_str = a.toString( );
        else
            a_str = String.format( "(%s)", a );

        if( b instanceof Zmienna || b instanceof Stala )
            b_str = b.toString( );
        else
            b_str = String.format( "(%s)", b );

        return String.format( "%s * %s", a_str, b_str );
    }
}

class PodzielPrzez extends Operator {
    PodzielPrzez( Wyrazenie a, Wyrazenie b ) {
        super( a, b );
    }

    public int oblicz( ) {
        return a.oblicz( ) / b.oblicz( );
    }

    public Wyrazenie derivative( ) {
        if( a instanceof Zmienna &&
            b instanceof Zmienna
        ) return new Stala( 0 );

        if( a instanceof Stala &&
            b instanceof Zmienna
            ) return new PodzielPrzez(
                            a.negative( ),
                            new Pomnoz( b, b )
                        );

        if( a instanceof Zmienna &&
            b instanceof Stala
        ) return new PodzielPrzez( new Stala( 1 ), b );

        return new PodzielPrzez(
            new Odejmij(
                new Pomnoz( a.derivative( ), b ),
                new Pomnoz( a, b.derivative( ) )
            ), new Pomnoz( b, b )
        );
    }

    public String toString( ) {
        String a_str;
        String b_str;

        if( a instanceof Zmienna || a instanceof Stala )
            a_str = a.toString( );
        else
            a_str = String.format( "(%s)", a );

        if( b instanceof Zmienna || b instanceof Stala )
            b_str = b.toString( );
        else
            b_str = String.format( "(%s)", b );

        return String.format( "%s / %s", a_str, b_str );
    }
}

public class zadanie2i3 {
    public static void main( String[ ] args ) {
        Wyrazenie ez = new Pomnoz( new Stala( 4 ), new Zmienna( "x" ) );
        ez = new Pomnoz( ez, ez );
        ez = new PodzielPrzez( ez, new Dodaj( new Stala( 3 ), new Zmienna( "x" ) ) );
        ez.setVarValue( "x", 42 );
        System.out.println( ez );
        System.out.println( ez.oblicz() );
        

        Wyrazenie dez = ez.derivative( );

        System.out.println( String.format( "F(x) = %s", ez ) );
        System.out.println( String.format( "F(42) = %s", ez.oblicz( ) ) );
        System.out.println( String.format( "F'(x) = %s", dez ) );
        System.out.println( String.format( "F'(42) = %s", dez.oblicz( ) ) );
    }
}

package lista7;

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

interface IObjectDump {
    public void saveToFile( String filename );
}

class Pojazd implements Serializable {
    private static final long serialVersionUID = 7526471155622776147L;

    String marka;
    String model;
    String naped;

    public String toString( ) {
        return "Marka: " + marka + " Model: " + model + " Napęd: " + naped;
    }
}

class PojazdPanel extends JPanel implements IObjectDump {
    private static final long serialVersionUID = 7526471155622776147L;

    Pojazd pojazd;

    JTextField markaText;
    JTextField modelText;
    JTextField napedText;

    JButton confirmButton;

    PojazdPanel( Pojazd pojazd ) {
        super( );

        this . pojazd = pojazd;

        this . add( new JLabel( "Marka:" ) );
        this . markaText = new JTextField( pojazd . marka, 20 );
        this . add( this . markaText );

        this . add( new JLabel( "Model:" ) );
        this . modelText = new JTextField( pojazd . model, 20 );
        this . add( this . modelText );

        this . add( new JLabel( "Napęd:" ) );
        this . napedText = new JTextField( pojazd . naped, 20 );
        this . add( this . napedText );
    }

    public void updateObject( ) {
        pojazd . marka = this . markaText . getText( );
        pojazd . model = this . modelText . getText( );
        pojazd . naped = this . napedText . getText( );
    }

    public void saveToFile( String filename ) {
        updateObject( );

        try {
            OutputStream os = new FileOutputStream( filename );
            ObjectOutputStream oos = new ObjectOutputStream( os );

            oos . writeObject( pojazd );
            oos . close( );
            os . close( );
        } catch( Exception e ) {
            System.out.println( "Error occured during saving to file." );
        }
    }
}

class Samochod extends Pojazd implements Serializable {
    private static final long serialVersionUID = 7526471155622776147L;

    enum NADWOZIE {
        KOMBI( "Kombi" ),
        SEDAN( "Sedan" ),
        SUV( "SUV" ),
        HATCHBACK( "Hatchback" );

        private final String text;

        NADWOZIE( final String text ) {
            this . text = text;
        }

        public String toString( ) {
            return this . text;
        }
    };

    enum RODZAJ_PALIWA {
        BENZYNA( "Benzyna" ),
        LPG( "LPG" ),
        ON( "Olej napędowy" );

        private final String text;

        RODZAJ_PALIWA( final String text ) {
            this . text = text;
        }

        public String toString( ) {
            return this . text;
        }

    };

    int pojemnoscSilnika = 1;
    NADWOZIE typNadwozia;
    RODZAJ_PALIWA rodzajPaliwa;
}

class SamochodPanel extends PojazdPanel implements IObjectDump {
    private static final long serialVersionUID = 7526471155622776147L;

    Samochod samochod;

    JSpinner pojemnoscSilnikaSpinner;
    JComboBox<Samochod . NADWOZIE> nadwozieComboBox;
    JComboBox<Samochod . RODZAJ_PALIWA> rodzajPaliwaComboBox;


    SamochodPanel( Samochod samochod ) {
        super( samochod );
        this . samochod = samochod;

        this . add( new JLabel( "Pojemność Silnika" ) );
        this . pojemnoscSilnikaSpinner = new JSpinner( new SpinnerNumberModel( samochod . pojemnoscSilnika, 1, 100, 1 ) );
        this . add( this . pojemnoscSilnikaSpinner );

        this . add( new JLabel( "Rodzaj nadwozia" ) );
        this . nadwozieComboBox = new JComboBox<Samochod . NADWOZIE>(  );
        this . nadwozieComboBox . setModel( new DefaultComboBoxModel<Samochod . NADWOZIE>( Samochod . NADWOZIE . values( ) ) );
        this . nadwozieComboBox . setSelectedItem( samochod . typNadwozia );
        this . add( this . nadwozieComboBox );

        this . add( new JLabel( "Rodzaj paliwa" ) );
        this . rodzajPaliwaComboBox = new JComboBox<Samochod . RODZAJ_PALIWA>(  );
        this . rodzajPaliwaComboBox . setModel( new DefaultComboBoxModel<Samochod . RODZAJ_PALIWA>( Samochod . RODZAJ_PALIWA . values( ) ) );
        this . rodzajPaliwaComboBox . setSelectedItem( samochod . rodzajPaliwa );
        this . add( this . rodzajPaliwaComboBox );
    }

    public void updateObject( ) {
        super . updateObject( );

        samochod . pojemnoscSilnika = (int) this . pojemnoscSilnikaSpinner . getValue( );
        samochod . typNadwozia = ( Samochod . NADWOZIE ) this . nadwozieComboBox . getSelectedItem( );
        samochod . rodzajPaliwa = ( Samochod . RODZAJ_PALIWA ) this . rodzajPaliwaComboBox . getSelectedItem( );
    }

    public void saveToFile( String filename ) {
        updateObject( );

        try {
            OutputStream os = new FileOutputStream( filename );
            ObjectOutputStream oos = new ObjectOutputStream( os );

            oos . writeObject( samochod );
            oos . close( );
            os . close( );
        } catch( Exception e ) {
            System.out.println( "Error occured during saving to file." );
        }
    }
}

class Tramwaj extends Pojazd implements Serializable {
    private static final long serialVersionUID = 7526471155622776147L;

    enum WYSOKOSC_PODLOGI {
        NISKA( "Niska" ),
        SREDNIA( "Średnia" ),
        WYSOKA( "Wysoka" );

        private final String text;

        WYSOKOSC_PODLOGI( final String text ) {
            this . text = text;
        }

        public String toString( ) {
            return this . text;
        }
    };

    int iloscWagonow = 1;
    int iluKierunkowy = 1;
    WYSOKOSC_PODLOGI wysokoscPodlogi;
}

class TramwajPanel extends PojazdPanel implements IObjectDump {
    private static final long serialVersionUID = 7526471155622776147L;

    Tramwaj tramwaj;

    JSpinner iloscWagonowSpinner;
    JSpinner iluKierunkowySpinner;
    JComboBox<Tramwaj . WYSOKOSC_PODLOGI> wysokoscPodlogiComboBox;


    TramwajPanel( Tramwaj tramwaj ) {
        super( tramwaj );
        this . tramwaj = tramwaj;

        this . add( new JLabel( "Ilość wagonów" ) );
        this . iloscWagonowSpinner = new JSpinner( new SpinnerNumberModel( tramwaj . iloscWagonow, 1, 20, 1 ) );
        this . add( this . iloscWagonowSpinner );

        this . add( new JLabel( "Ilu kierunkowy: " ) );
        this . iluKierunkowySpinner = new JSpinner( new SpinnerNumberModel( tramwaj . iluKierunkowy, 1, 2, 1 ) );
        this . add( this . iluKierunkowySpinner );

        this . add( new JLabel( "Wysokość podłogi" ) );
        this . wysokoscPodlogiComboBox = new JComboBox<Tramwaj . WYSOKOSC_PODLOGI>(  );
        this . wysokoscPodlogiComboBox . setModel( new DefaultComboBoxModel<Tramwaj . WYSOKOSC_PODLOGI>( Tramwaj . WYSOKOSC_PODLOGI . values( ) ) );
        this . wysokoscPodlogiComboBox . setSelectedItem( tramwaj . wysokoscPodlogi );
        this . add( this . wysokoscPodlogiComboBox );
    }

    public void updateObject( ) {
        super . updateObject( );

        tramwaj . iloscWagonow = (int) this . iloscWagonowSpinner . getValue( );
        tramwaj . iluKierunkowy = (int) this . iluKierunkowySpinner . getValue( );
        tramwaj . wysokoscPodlogi = ( Tramwaj. WYSOKOSC_PODLOGI ) this . wysokoscPodlogiComboBox . getSelectedItem( );
    }

    public void saveToFile( String filename ) {
        updateObject( );

        try {
            OutputStream os = new FileOutputStream( filename );
            ObjectOutputStream oos = new ObjectOutputStream( os );

            oos . writeObject( tramwaj );
            oos . close( );
            os . close( );
        } catch( Exception e ) {
            System.out.println( "Error occured during saving to file." );
        }
    }
}

class savingButton implements ActionListener {
    private IObjectDump toSerialize;
    private String filename;

    savingButton( IObjectDump toSerialize, String filename ) {
        this . toSerialize = toSerialize;
        this . filename = filename;
    }

    public void actionPerformed( ActionEvent e ) {
        toSerialize . saveToFile( filename );
    }
}

class program {
    public static void main( String args[ ] ) {
        if( args . length < 2 ) {
            System.out.println( "Usage: java lista7.program <path> <class>" );
            return;
        }

        JFrame frame = new JFrame( "Lista 7" );
        frame . setSize( 280, 300 );

        ObjectInputStream ois;
        try {
            InputStream file = new FileInputStream( args[ 0 ] );
            ois = new ObjectInputStream( file );
        } catch( Exception e ) {
            ois = null;
        }

        switch( args[ 1 ] ) {
            case "Pojazd": {
                Pojazd p;

                if( ois != null ) {
                    try {
                        p = (Pojazd) ois . readObject( );
                        ois . close( );
                    } catch( Exception e ) {
                        p = new Pojazd( );
                    }
                } else
                    p = new Pojazd( );

                PojazdPanel pp = new PojazdPanel( p );
                frame . add( pp );

                JButton saveButton = new JButton( "Save!" );
                saveButton . addActionListener( new savingButton( pp, args[ 0 ] ) );
                pp . add( saveButton );

                break;
            }

            case "Samochod": {
                Samochod s;

                if( ois != null ) {
                    try {
                        s = (Samochod) ois . readObject( );
                    } catch( Exception e ) {
                        s = new Samochod( );
                    }
                } else
                    s = new Samochod( );

                SamochodPanel sp = new SamochodPanel( s );
                frame . add( sp );

                JButton saveButton = new JButton( "Save!" );
                saveButton . addActionListener( new savingButton( sp, args[ 0 ] ) );
                sp . add( saveButton );

                break;
            }

            case "Tramwaj": {
                Tramwaj t;

                if( ois != null ) {
                    try {
                        t = (Tramwaj) ois . readObject( );
                        ois . close( );
                    } catch( Exception e ) {
                        t = new Tramwaj( );
                    }
                } else
                    t = new Tramwaj( );

                TramwajPanel tp = new TramwajPanel( t );
                frame . add( tp );

                JButton saveButton = new JButton( "Save!" );
                saveButton . addActionListener( new savingButton( tp, args[ 0 ] ) );
                tp . add( saveButton );

                break;
            }

            default: {
                System . out . println( "Bad <class> argument. Available are 'Pojazd', 'Samochod' & 'Tramwaj'." );
                return;
            }
        }

        frame . setVisible( true );
    }
}

class Funkcja
    DELTA = 1e-10
    SAMPLES = 50

    def initialize( &func )
        @func = func
    end

    def value( x )
        return @func.call( x )
    end

    def pole( a, b )
        if @rand == nil; @rand = Random.new end

        ret = 0.0
        SAMPLES.times do
            nret = 0.0

            SAMPLES.times do
                x = a + ( b - a ) * @rand.rand
                nret += self.value( x )
            end

            nret /= SAMPLES
            nret *= ( b - a )

            ret += nret

        end

        return ret / SAMPLES
    end

    def zerowe( a, b, e )
        while true
            c = ( a + b ) / 2.0
            if self.value( c ) == 0 or ( b - a ) / 2.0 < e; return c end
            ((self.value(c) < 0) == (self.value(a) < 0)) ? a = c : b = c
        end

        return nil
    end

    def poch( x )
        return ( self.value( x + DELTA ) - self.value( x - DELTA ) ) / ( 2 * DELTA )
    end
end

if __FILE__ == $0
    f = Funkcja.new{ |x| Math.sin( x ) }

    almost_pi = f.zerowe( 1, 4, 1e-15 )
    puts "sin(%.10f) ~= 0" % almost_pi
    puts "PI calc error = +/- %0.15f" % ( almost_pi - Math::PI ).abs
    puts "sin(almost_pi) = %.10f" % f.value( almost_pi )
    puts "sin'(almost_pi) = cos(almost_pi) = %.10f" % f.poch( almost_pi )

    f = Funkcja.new{ |x| Math::E ** ( -x ** 2 ) }
    puts "Integral of e^-x^2 from %.10f to %.10f = %.10f" % [ 0, 2, f.pole( 0, 2 ) ]


    f = Funkcja.new{ |x| (2*x*Math.asin(2*x))/Math.sqrt(1-4*x**2) }
    puts "Nowy VAT to: %0.5f" % f.pole(0, Math.sqrt(3)/4)
end

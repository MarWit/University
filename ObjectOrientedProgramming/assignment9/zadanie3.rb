require_relative 'zadanie1'

class Funkcja
    RESOLUTION = 0.001

    def draw( a, b )
        i = a

        t = "# X Y\n"

        while i < b
            t += "%0.10f %0.10f\n" % [ i, self.value( i ) ]
            i += RESOLUTION
        end

        return t
    end
end

if __FILE__ == $0
    f = Funkcja.new{ |x| Math.sin(1/x) }
    puts f.draw( -10, 10 )
end

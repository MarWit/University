class Fixnum
    def czynniki
        return (1..self).select { |n| self % n == 0 }
    end

    def ack(y)
        return self == 0 ? y + 1 : ( y == 0 ? (self - 1).ack( 1 ) : (self - 1).ack( self.ack( y - 1 ) ) )
    end

    def doskonala
        return self == (self.czynniki.inject(0, :+) - self)
    end

    def slownie
        dict = [ "zero", "jeden", "dwa", "trzy", "cztery", "pięć", "sześć", "siedem", "osiem", "dziewięć" ]
        return self.to_s.split(//).map{ |n| dict[ n.to_i ] }.join( ' ' )
    end
end

class Jawna
    def initialize( what )
        @str = what
    end

    def zaszyfruj( klucz )
        return Zaszyfrowane.new( @str.split(//).map{ |c| klucz[ c ] || c }.join )
    end

    def to_s
        return @str
    end
end

class Zaszyfrowane
    def initialize( what )
        @str = what
    end

    def odszyfruj( klucz )
        klucz = klucz.invert

        return Jawna.new( @str.split(//).map{ |c| klucz[ c ] || c }.join )
    end

    def to_s
        return @str
    end
end

if __FILE__ == $0
    j = Jawna.new( "ruby" )
    puts j

    klucz = {
        'a' => 'b',
        'b' => 'r',
        'r' => 'y',
        'y' => 'u',
        'u' => 'a'
    }

    z = j.zaszyfruj( klucz )
    puts z

    j = z.odszyfruj( klucz )
    puts j
end


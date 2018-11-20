class Kolekcja
    class Element
        attr_accessor :value

        def initialize( value )
            @value = value
        end

        def next( )
            return @_next
        end

        def set_next( el )
            @_next = el
            return el
        end
    end

    def initialize( values = nil )
        if values != nil
            if values.is_a?( Array )
                for e in values
                    put( e )
                end
            else
                put( values )
            end
        end
    end

    def put( value )
        el = Element . new value

        if not @count
            @first = @last = el
            @count = 1
        else
            @last = @last . set_next( el )
            @count += 1
        end
    end

    def swap( i, j )
        max = i > j ? i : j

        if max > @count; raise "Index out of bounds"; end

        a = nil
        b = nil
        ptr = @first
        ( max + 1 ) . times do |e|
            if e == i; a = ptr; end
            if e == j; b = ptr; end

            ptr = ptr . next
        end

        ptr = a . value
        a . value = b . value
        b . value = ptr
    end

    def length( )
        return @count
    end

    def get( i )
        if i > @count; raise "Index out of bounds"; end

        ptr = @first
        i . times do |e|; ptr = ptr . next; end

        return ptr . value
    end
end

class Sortowanie
    def self . sort1( what )
        if not what . is_a?( Kolekcja )
            raise "Invalid type. Only 'Kolekcja' is supported."
        end

        what . length . times do |i|
            (what . length - 1) . times do |j|
                if what . get( j ) > what . get( j + 1 )
                    what . swap( j, j + 1 )
                end
            end
        end

        return what
    end

    def self . sort2( what )
        if not what . is_a?( Kolekcja )
            raise "Invalid type. Only 'Kolekcja' is supported."
        end

        ( what . length - 1 ) . times do |i|
            e = what . get( i + 1 )

            (0..i).reverse_each do |j|
                if what . get( j ) <= e; break; end
                what . swap( j, j + 1 )
            end
        end

        return what
    end
end

if __FILE__ == $0
    array = [4,7,6,9,3,4]
    k = Kolekcja . new array

    k . length . times do |i|
        puts k . get( i )
    end

    puts "\n"

    k = Sortowanie . sort2 k

    k . length . times do |i|
        puts k . get( i )
    end
end

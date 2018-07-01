#!/usr/bin/env ruby

class Snapshot
    def _get_data( )
        top_objects = ObjectSpace
                        .each_object
                        .inject( Hash . new 0 ) { |d,o| d[ o.class ] += 1; d }
                        .sort_by{ |k,v| -v }
                        .take( 15 )

        strings = ObjectSpace
                        .each_object( String )
                        .inject( Hash . new [] ) {
                            |d,o|
                            d[ ( o.to_s[ 0 ] || '#' ).upcase ] += [o.to_s]; d
                        }
                            .sort_by{ |k,v| -k }

        return [ top_objects, strings ]
    end

    def generate( formatObject )
        if formatObject.is_a?( FormatTXT ) or formatObject.is_a?( FormatHTML )
            top_objects, strings = _get_data( )
            formatObject . format( top_objects, strings )
        else
            raise "Invalid type of argument. Should be FormatTXT or FormatHTML."
        end
    end
end

class FormatHTML
    def format( top_objects, strings )
        html = <<-HTML
<!DOCTYPE html>
<html>
    <head>
        <title>Ruby program analize.</title>
        <style>
            code {
                display: block;
                white-space: pre-wrap;
            }
        </style>
    </head>
    <body>
        <div>
            <h3>TOP OBJECTS</h3>
            <code>
%{objects}
            </code>
        </div>
        <div>
            <h3>Strings</h3>
            <div>
%{strings}
            </div>
        </div>
    </body>
</html>
HTML

    fobjects = "Count\t\t\tName\n"
    fobjects += top_objects.reduce( '' ) { |f, (c, cnt)| f += "#{cnt.to_s.ljust(8)}\t\t#{c}\n" }

    fstrings = strings.reduce( '' ) {
        |f, (l, ar)|
        f += "<h2>#{l}</h2>\n<code>\n" + ar.inject { |m, s| m += s && "#{s}\n" } + "</code>\n"
    }

    return html % { :objects => fobjects, :strings => fstrings }
    end
end

class FormatTXT
    def format( top_objects, strings )
        txt = <<-TXT
#### TOP OBJECTS ###
%{objects}

### STRINGS BY LETTER ###
%{strings}
TXT

        fobjects = "Count\t\t\tName\n"
        fobjects += top_objects.reduce( '' ) { |f, (c, cnt)| f += "#{cnt.to_s.ljust(8)}\t\t#{c}\n" }


        fstrings = strings.reduce( '' ) {
            |f, (l, ar)|
            f += "#--- #{l} ---#\n" + ar.inject { |m, s| m += s && "#{s}\n" } + "\n#--- END #{l} ---#\n\n"
        }

        return txt % { :objects => fobjects, :strings => fstrings }
    end
end

b = Snapshot . new
puts b . generate FormatTXT . new

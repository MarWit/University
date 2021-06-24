set terminal png
set output "van_gogh_error.png"

set xlabel "polygons"

set autoscale

set ylabel "error"
set logscale y

set grid
set style data lines

plot "van_gogh_error.data" using 1:2 title "Van Gogh"

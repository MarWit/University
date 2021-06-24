set terminal png
set output "rust_error.png"

set xlabel "polygons"

set autoscale

set ylabel "error"
set logscale y

set grid
set style data lines

plot "rust_error.data" using 1:2 title "Rust Logo"

set terminal epslatex color
set output 'praca-gnuplottex-fig5.tex'
set grid
set title 'Rozmiar sterty procesu'
set ylabel 'słowa maszynowe'
set xlabel '$n$'
set size ratio 0.8
plot 'facstats.csv' using 1:9 w p pt 7 ps 1 title 'fac:fac($n$)',\
'facstats.csv' using 1:9 w steps lt 3 lw 4 lc 1 notitle,\
'fac2stats.csv' using 1:9 w p pt 7 ps 1 lc 3 title 'fac:fac2($n$)', \
'fac2stats.csv' using 1:9 w steps lt 3 lw 4 lc 3 notitle,\

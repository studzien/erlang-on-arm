set terminal epslatex color
set output 'praca-gnuplottex-fig3.tex'
set grid
set title 'Czas działania'
set ylabel 'czas ($\mu s$)'
set xlabel '$n$'
set size ratio 0.8
plot 'facstats.csv' using 1:5 w p pt 7 ps 1 title 'całkowity fac:fac($n$)',\
'facstats.csv' using 1:5 smooth csplines lt 3 lw 4 lc 1 notitle,\
'fac2stats.csv' using 1:5 w p pt 7 ps 1 lc 3 title 'całkowity fac:fac2($n$)', \
'fac2stats.csv' using 1:5 smooth csplines lt 3 lw 4 lc 3 notitle,\

set terminal epslatex color
set output 'praca-gnuplottex-fig6.tex'
set grid
set title 'Czas działania'
set ylabel 'czas ($\mu s$)'
set xlabel '$n$'
set yr [0:4000]
set size ratio 0.8
plot 'facstats.csv' using 1:7 w p pt 7 ps 1 title 'kod fac:fac($n$)',\
'facstats.csv' using 1:7 smooth csplines lt 3 lw 4 lc 1 notitle,\
'fac2stats.csv' using 1:7 w p pt 7 ps 1 lc 3 title 'kod fac:fac2($n$)', \
'fac2stats.csv' using 1:7 smooth csplines lt 3 lw 4 lc 3 notitle,\
'facstats.csv' using 1:6 w p pt 5 ps 1 lc 1 title 'GC fac:fac($n$)',\
'facstats.csv' using 1:6 smooth csplines lt 3 lw 4 lc 1 notitle,\
'fac2stats.csv' using 1:6 w p pt 5 ps 1 lc 3 title 'GC fac:fac2($n$)', \
'fac2stats.csv' using 1:6 smooth csplines lt 3 lw 4 lc 3 notitle

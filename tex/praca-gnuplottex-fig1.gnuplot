set terminal epslatex color
set output 'praca-gnuplottex-fig1.tex'
set grid
set title 'Pamięć zwolniona przez \emph{garbage collector}'
set ylabel 'słowa maszynowe'
set xlabel '$n$'
set yr [0:5000]
set size ratio 0.8
plot 'facstats.csv' using 1:2 w p pt 7 ps 1 title 'fac:fac($n$)',\
'facstats.csv' using 1:2 smooth csplines lt 3 lw 4 lc 1 notitle,\
'fac2stats.csv' using 1:2 w p pt 7 ps 1 lc 3 title 'fac2:fac($n$)', \
'fac2stats.csv' using 1:2 smooth csplines lt 3 lw 4 lc 3 notitle

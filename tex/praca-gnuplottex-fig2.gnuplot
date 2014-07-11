set terminal epslatex color
set output 'praca-gnuplottex-fig2.tex'
set grid
set title 'Uruchomienia \emph{garbage collectora}'
set ylabel 'uruchomienia'
set xlabel '$n$'
set yr [0:180]
set size ratio 0.8
plot 'facstats.csv' using 1:3 w p pt 7 ps 1 title 'fac:fac($n$)',\
'facstats.csv' using 1:3 smooth csplines lt 3 lw 4 lc 1 notitle,\
'fac2stats.csv' using 1:3 w p pt 7 ps 1 lc 3 title 'fac:fac2($n$)', \
'fac2stats.csv' using 1:3 smooth csplines lt 3 lw 4 lc 3 notitle

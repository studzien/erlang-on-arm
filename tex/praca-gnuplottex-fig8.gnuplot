set terminal epslatex color
set output 'praca-gnuplottex-fig8.tex'
set grid
set title 'Dostępna pamięć RAM'
set ylabel 'dostępna pamięć [B]'
set xlabel '$t$ [min]'
set yr [0:4000]
set xr [0:720]
set size 1.3,0.6
plot 'led_12h.csv' using 11:3 w p pt 7 ps 1 notitle

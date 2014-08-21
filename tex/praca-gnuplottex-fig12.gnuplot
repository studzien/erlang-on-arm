set terminal epslatex color
set output 'praca-gnuplottex-fig12.tex'
set grid
set title 'Liczba wysłanych wiadomości'
set ylabel 'wiadomości'
set xlabel '$t$ [min]'
set xr [0:720]
set size 1.3,0.6
plot 'led_12h.csv' using 11:8 w p pt 7 ps 1 notitle

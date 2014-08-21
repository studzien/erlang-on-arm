set terminal epslatex color
set output 'praca-gnuplottex-fig9.tex'
set grid
set title 'Łączny rozmiar sterty procesów'
set ylabel 'rozmiar sterty [B]'
set xlabel '$t$ [min]'
set yr [0:150]
set xr [0:720]
set size 1.3,0.6
plot 'led_12h.csv' using 11:4 w p pt 7 ps 1 notitle

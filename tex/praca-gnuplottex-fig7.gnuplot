set terminal epslatex color
set output 'praca-gnuplottex-fig7.tex'
set grid
set title 'Liczba uruchomionych procesów'
set ylabel 'procesy'
set xlabel '$t$ [min]'
set yr [0:5]
set xr [0:720]
set size 1.3,0.6
plot 'led_12h.csv' using 11:2 w p pt 7 ps 1 notitle

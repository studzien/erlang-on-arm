set terminal epslatex color
set output 'praca-gnuplottex-fig10.tex'
set grid
set title 'Liczba uruchomień \emph{garbage collectora}'
set ylabel 'uruchomienia'
set xlabel '$t$ [min]'
set xr [0:720]
set size 1.3,0.6
plot 'led_12h.csv' using 11:6 w p pt 7 ps 1 notitle

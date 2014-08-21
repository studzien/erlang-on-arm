set terminal epslatex color
set output 'praca-gnuplottex-fig11.tex'
set grid
set title 'Pamięć zwolniona przez \emph{garbage collector}'
set ylabel 'zwolniona pamięć [słowa maszynowe]'
set xlabel '$t$ [min]'
set xr [0:720]
set size 1.3,0.6
plot 'led_12h.csv' using 11:5 w p pt 7 ps 1 notitle

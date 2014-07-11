set terminal pdf font ",10" linewidth 3
set output 'abc-gnuplottex-fig1.pdf'
plot sin(x), cos(x)

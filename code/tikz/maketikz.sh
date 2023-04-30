cat $1.tex > which.aux
dst=../../out/fig/tikz
pdflatex main ;\
pdfcrop --margins '5 5 5 5' main.pdf $dst/$1.pdf
convert -density 600 -alpha remove -alpha off $dst/$1.pdf $dst/$1.png
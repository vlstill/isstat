ALL_MD=$(wildcard *.md)

all : $(ALL_MD:.md=.pdf)

%.pdf : %.md Makefile
	pandoc $< -o $@ -V geometry:a4paper,margin=2.5cm -V lang=czech

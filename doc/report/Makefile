# Source: http://tex.stackexchange.com/questions/40738/how-to-properly-make-a-latex-project

.PHONY: mrproper clean FORCE_MAKE

PDF=btrsync-tgc.pdf btrsync-full.pdf btrsync-tgc-single.pdf

all: $(PDF)

btrsync-tgc-single.tex: btrsync-tgc.tex results.tex btrsync-tgc.bbl single.sh
	./single.sh

btrsync-tgc.bbl: btrsync-tgc.pdf

# MAIN LATEXMK RULE

# -pdf tells latexmk to generate PDF directly (instead of DVI).
# -pdflatex="" tells latexmk to call a specific backend with specific options.
# -use-make tells latexmk to call make for generating missing files.

# -interactive=nonstopmode keeps the pdflatex backend from stopping at a
# missing file reference and interactively asking you for an alternative.

btrsync-tgc-single.pdf: btrsync-tgc-single.tex FORCE_MAKE
	perl latexmk.pl -pdf -pdflatex="pdflatex -interaction=nonstopmode" -bibtex- $<

%.pdf: %.tex results.tex FORCE_MAKE
	perl latexmk.pl -pdf -pdflatex="pdflatex -interaction=nonstopmode" -bibtex $<

results.tex: results mkresults.sh si.pl
	./mkresults.sh

clean:
	perl latexmk.pl -c
	rm -f *~ *.backup *.bak

mrproper: clean
	perl latexmk.pl -C
	rm -f $(PDF)

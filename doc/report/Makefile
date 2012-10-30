# Source: http://tex.stackexchange.com/questions/40738/how-to-properly-make-a-latex-project

.PHONY: mrproper clean FORCE_MAKE

PDF=btrsync.pdf btrsync-single.pdf

all: $(PDF)

btrsync-single.tex: btrsync.tex results.tex btrsync.bbl single.sh
	./single.sh

btrsync.bbl: btrsync.pdf

# MAIN LATEXMK RULE

# -pdf tells latexmk to generate PDF directly (instead of DVI).
# -pdflatex="" tells latexmk to call a specific backend with specific options.
# -use-make tells latexmk to call make for generating missing files.

# -interactive=nonstopmode keeps the pdflatex backend from stopping at a
# missing file reference and interactively asking you for an alternative.

btrsync-single.pdf: btrsync-single.tex FORCE_MAKE
	perl latexmk.pl -pdf -pdflatex="pdflatex -interaction=nonstopmode" -bibtex- $<

btrsync.pdf: btrsync.tex results.tex FORCE_MAKE
	perl latexmk.pl -pdf -pdflatex="pdflatex -interaction=nonstopmode" -bibtex $<

results.tex: results mkresults.sh si.pl
	./mkresults.sh

clean:
	perl latexmk.pl -c
	rm -f *~ *.backup *.bak

mrproper: clean
	perl latexmk.pl -C
	rm -f $(PDF)

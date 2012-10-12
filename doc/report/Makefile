TARGETS=btrsync.pdf
DEPS_DIR = .deps
LATEXMK = latexmk -recorder -use-make -deps \
  -e 'warn qq(In Makefile, turn off custom dependencies);' \
  -e '@cus_dep_list = ();' \
  -e 'show_cus_dep();'

all : $(TARGETS)

$(foreach file,$(TARGETS),$(eval -include $(DEPS_DIR)/$(file)P))

$(DEPS_DIR) :
	mkdir $@

#results.tex: results
#	./mkresults.sh

%.pdf: %.tex
	if [ ! -e $(DEPS_DIR) ]; then mkdir $(DEPS_DIR); fi
	$(LATEXMK) -pdf -dvi- -ps- -deps-out=$(DEPS_DIR)/$@P $<


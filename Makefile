#
# vim: noet sw=4 ts=5
#

PIC =		$(wildcard *.pic)
PIC_TEX =	$(PIC:%.pic=%.tex)

TEX_PROSE = thesis.tex overlap.tex loop_control.tex tscope.tex
TEXFILES = $(TEX_PROSE) macros.tex
TABLES_TEX = mem_table.tex times_table.tex

# Results from Loop control.
TIMING_RESULTS =    results_carlton_n10_2011-11-26_01.pickle
MEM_RESULTS =       results_carlton_n10_2011-11-26_01.pickle
BENCH_ALL=          lc_bench_all

.PHONY : all
all : thesis.pdf wc

.PHONY : wc
wc : 
	wc -w $(TEX_PROSE)

thesis.dvi : $(TEXFILES) $(PIC_TEX) $(TABLES_TEX) bib.bib
	latex thesis
	bibtex thesis
	latex thesis
	latex thesis

%.pdf : %.dvi
	dvipdf $<

%.ps : %.dvi
	dvips $<

%.tex:		%.pic
	gpic -t < $< > $@

times_table.tex: $(TIMING_RESULTS) $(BENCH_ALL)
	./$(BENCH_ALL) -n0 -w -f times_table.tex -p $(TIMING_RESULTS)

mem_table.tex:  $(MEM_RESULTS) $(BENCH_ALL)
	./$(BENCH_ALL) -n0 -m -f mem_table.tex -p $(MEM_RESULTS)

.PHONY : clean
clean :
	rm -rf $(PIC_TEX) \
		$(PIC:%.pic=%.aux) \
		$(TABLES_TEX) \
		thesis.aux \
		thesis.bbl \
		thesis.blg \
		thesis.dvi \
		thesis.lof \
		thesis.log \
		thesis.lot \
		thesis.out \
		thesis.pdf \
		thesis.ps \
		thesis.toc


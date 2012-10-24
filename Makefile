#
# vim: noet sw=4 ts=4
#

PIC =		$(wildcard pics/*.pic)
PIC_TEX =	$(PIC:%.pic=%.tex)
DOT =       $(wildcard pics/*.dot)
DOT_EPS =   $(DOT:%.dot=%.eps)
LS =		lecture_support/Makefile.lectures
TALK_PICS =	$(wildcard pics/*.pic)
TALK_PSS =	$(wildcard raw_ps/*.ps)

TEX_PROSE = \
		thesis.tex \
		intro.tex \
		literature_review.tex \
		backgnd.tex \
		backgnd_mercury.tex \
		backgnd_merpar.tex \
		backgnd_deppar.tex \
		backgnd_autopar.tex \
		rts.tex \
		rts_gc.tex \
		rts_original_scheduling.tex \
		rts_original_scheduling_performance.tex \
		rts_work_stealing.tex \
		rts_reorder.tex \
		rts_work_stealing2.tex \
		overlap.tex \
		loop_control.tex \
		tscope.tex \
		conc.tex

TEXFILES = 	$(TEX_PROSE) macros.tex
TABLES_TEX = $(GEN_TABLES_TEX) \
		tab_gc.tex \
		tab_gc_amdahl.tex \
		tab_gc_heapsize_gc4.tex \
		tab_work_stealing_initial3.tex \
		tab_work_stealing_revised.tex
GEN_TABLES_TEX = \
		mem_table.tex \
		times_table.tex

SPELL_FILES = $(TEX_PROSE:%.tex=%.spell)
STYLE_FILES = $(TEX_PROSE:%.tex=%.style)
CHECK_FILES = $(TEX_PROSE:%.tex=%.check)

# Results from Loop control.
TIMING_RESULTS =    results_carlton_n10_2011-11-26_01.pickle
MEM_RESULTS =       results_carlton_n10_2011-11-26_01.pickle
BENCH_ALL=          lc_bench_all

#DETEX=untex -m -uascii -e
DETEX=detex -l -n

.PHONY : all
all : thesis.pdf thesis.ps undefined.txt talk.pdf spelling checking

.PHONY : wc
wc :
	wc -w $(TEX_PROSE)

thesis.dvi thesis.log : $(TEXFILES) $(PIC_TEX) $(DOT_EPS) $(TABLES_TEX) bib.bib
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

%.eps: %.dot
	dot -Teps < $< > $@

times_table.tex: $(TIMING_RESULTS) $(BENCH_ALL)
	./$(BENCH_ALL) -n0 -w -f times_table.tex -p $(TIMING_RESULTS)

mem_table.tex:  $(MEM_RESULTS) $(BENCH_ALL)
	./$(BENCH_ALL) -n0 -m -f mem_table.tex -p $(MEM_RESULTS)

undefined.txt: thesis.log
	cat $< | grep undefined | sort -u > undefined.txt

.PHONY : checking 
checking : $(CHECK_FILES)

%.check : %.tex check
	if [ -e .use_mercury ]; then \
		./check $< && touch $@; \
	fi

check : check.m parse_tex.m tex.m util.m
	if [ -e .use_mercury ]; then \
		mmc -O2 --intermod-opt --make check; \
	else \
		touch check; \
	fi

.PHONY : spelling
spelling : $(SPELL_FILES)

%.spell : %.tex
	$(DETEX) < $< | spell | sort -u > $@

.PHONY : style
style : $(STYLE_FILES)

%.style : %.tex
	echo "diction output" > $@
	echo "==============" >> $@
	$(DETEX) < $< | diction >> $@
	echo "" >> $@
	echo "style output" >> $@
	echo "============" >> $@
	$(DETEX) < $< | style -n >> $@

talk.pdf: talk.orig $(TALK_PICS) $(TALK_PSS)
	make -f $(LS) teacher_beamer/talk.pdf
	cp teacher_beamer/talk.pdf talk.pdf

.PHONY : clean
clean :
	rm -rf $(PIC_TEX) \
		$(PIC:%.pic=%.aux) \
		$(GEN_TABLES_TEX) \
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
		thesis.toc \
		thesis.loa \
		talk.pdf \
		.teacher_beamer \
		teacher_beamer \
		undefined.txt \
		Mercury \
		check \
		check.err \
		check.mh \
		*.style \
		*.spell \
		*.check


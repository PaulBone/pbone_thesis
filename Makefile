#
# vim: noet sw=8 ts=8
#

PIC =		$(wildcard pics/*.pic)
PIC_TEX =	$(PIC:%.pic=%.tex)
LS =		lecture_support/Makefile.lectures
TALK_PICS =	$(wildcard pics/*.pic)
TALK_PSS =	$(wildcard raw_ps/*.ps)

TEX_PROSE = \
		thesis.tex \
		intro.tex \
		literature_review.tex \
		backgnd_mercury.tex \
		backgnd_merpar.tex \
		backgnd_deppar.tex \
		backgnd_deep.tex \
		rts.tex \
		overlap.tex \
		loop_control.tex \
		tscope.tex \

TEXFILES = 	$(TEX_PROSE) macros.tex
TABLES_TEX = 	mem_table.tex times_table.tex

SPELL_FILES = $(TEX_PROSE:%.tex=%.spell)

# Results from Loop control.
TIMING_RESULTS =    results_carlton_n10_2011-11-26_01.pickle
MEM_RESULTS =       results_carlton_n10_2011-11-26_01.pickle
BENCH_ALL=          lc_bench_all

#DETEX=untex -m -uascii -e
DETEX=detex -l -n

#
# Hopefully this means that someone without mercury installed
# can't build the thesis.
#
MMC = `which mmc`

ifneq ($(MMC),)
	should_check = checked
else
	should_check =
endif

.PHONY : all
all : thesis.pdf undefined.txt talk.pdf spelling

.PHONY : wc
wc :
	wc -w $(TEX_PROSE)

thesis.dvi thesis.log : $(TEXFILES) $(PIC_TEX) $(TABLES_TEX) bib.bib \
		$(should_check)
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

undefined.txt: thesis.log
	cat $< | grep undefined | sort -u > undefined.txt

checked : $(TEX_PROSE) check
	./check $(TEX_PROSE)
	touch checked

check : check.m
	mmc --make check

.PHONY : spelling
spelling : $(SPELL_FILES)

%.spell : %.tex
	$(DETEX) < $< | spell | sort -u > $@

talk.pdf: talk.orig $(TALK_PICS) $(TALK_PSS)
	make -f $(LS) teacher_beamer/talk.pdf
	cp teacher_beamer/talk.pdf talk.pdf

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
		thesis.toc \
		talk.pdf \
		.teacher_beamer \
		teacher_beamer \
		undefined.txt \
		checked \
		Mercury \
		check \
		check.err \
		check.mh


TEXI2HTML = 	$(MAKEINFO) --html --number-sections --no-split
MAKEINFO = 	makeinfo
DVIPS = 	dvips

.SUFFIXES: .in .texi .dvi_log .ps .pdf .text

all:		lecture_support.ps lecture_support.pdf lecture_support.html

%.dvi: 		%.texi
		-$(TEXI2DVI) $< > $*.dvi_log

%.ps: 		%.dvi
		$(DVIPS) -f < $< > $@

%.pdf: 		%.ps
		ps2pdf $*.ps $*.pdf

%.html: 	%.texi
		$(MAKEINFO) --html --no-headers --no-split -o $@ $<

clean:
		-/bin/rm -f *.aux *.cp *.cps *.dvi_log *.fn *.ky \
			*.log *.pdf_log *.pg *.toc *.tp *.vr

clobber:	clean
		-/bin/rm -f *.html *.info *.pdf *.ps

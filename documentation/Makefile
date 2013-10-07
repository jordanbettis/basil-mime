TEX               = latex
PDFTEX            = pdflatex
TSWITCHES         = 

documentation:
	$(TEX) $(TSWITCHES) MRCY.SPC.002-Basil.tex
	$(TEX) $(TSWITCHES) MRCY.SPC.002-Basil.tex
	$(PDFTEX) $(TSWITCHES) MRCY.SPC.002-Basil.tex

clean:
	rm *.aux
	rm *.log
	rm *.toc
	rm *.dvi
	rm *.pdf


%.pdf : %.tex
	pdflatex $<
%.tex : %.lhs
	lhs2tex $< -o $@
%.tex : %.lagda
	lhs2tex --agda $< -o $@
defquotients.pdf : defquotients.tex Appendix/AgdaQuotients.tex Appendix/SeqEnum.tex
Appendix/AgdaQuotients.tex : Appendix/AgdaQuotients.lagda
Appendix/SeqEnum.tex : Appendix/SeqEnum.lhs


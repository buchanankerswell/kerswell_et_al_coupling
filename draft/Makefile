# Makefile for compiling RMarkdown documents
# Document name (no file extension)
PAPER = kerswell-et-al-coupling

# pdflatex command
LTXFLAGS = "--shell-escape -interaction=nonstopmode -file-line-error -synctex=1 %O %S"
# Strings to search for in log file for rerunning pdflatex until all
# cross references and other issues are resolved
RERUN := "(Rerun to get (cross-references|the bars|point totals) right|Table widths have changed. Rerun LaTeX.|Linenumber reference failed|has been referenced but does not)"
# Same for bibliography
RERUNBIB := "No file.*\.bbl|Citation.*undefined"

# Top level
all: paper

# Doc requires the pdf file
paper: $(PAPER).pdf
	@open $<

upload: $(PAPER).Rmd $(PAPER).pdf
	@printf "\nUploading %s to google docs trackdown/%s\n" "$(PAPER)"
	@Rscript assets/r/upload_google.R $(PAPER).Rmd $(PAPER).pdf upload

update: $(PAPER).Rmd $(PAPER).pdf
	@printf "\nUploading %s to google docs trackdown/%s\n" "$(PAPER)"
	@Rscript assets/r/upload_google.R $(PAPER).Rmd $(PAPER).pdf update

download: $(PAPER).Rmd
	@printf "\nDownloading %s from google docs trackdown/%s\n" "$(PAPER)"
	@Rscript assets/r/upload_google.R $(PAPER).Rmd $(PAPER).pdf download

# Main level
# pdf file requires compiling the tex file
# using pdflatex and bibtex
$(PAPER).pdf: $(PAPER).tex
	@printf "\nPDFLaTeX for %s ..." "$(PAPER)"
	@pdflatex $(LTXFLAGS) $< > /dev/null 2>&1
	@while(egrep -q $(RERUNBIB) $(PAPER).log > /dev/null 2>&1 && bibtex $(PAPER) > /dev/null 2>&1 && pdflatex $(LTXFLAGS) $< > /dev/null 2>&1) do \
		echo "\nRerunning BibTeX ..." ; done
	@while(egrep $(RERUN) $(PAPER).log > /dev/null 2>&1 && pdflatex $(LTXFLAGS) $< > /dev/null 2>&1) do \
		echo "\nRerunning PDFLaTeX ..." ; done

# Lowest level
# tex document requires knitting by calling
# rmarkdown::render in R
$(PAPER).tex: $(PAPER).Rmd
	@printf "\nRmarkdown Render for %s ..." "$(PAPER)"
	@R -q -e 'rmarkdown::render("$(PAPER).Rmd", quiet = T, clean = F)' > /dev/null 2>&1
	@rm -f $(PAPER).pdf

# purge auxillary files
purge:
	@rm -f *.{html,acr,alg,gz,glg,gls,ilg,nls,acn,glo,ist,lof,lot,nlo,aux,dvi,log,bbl,blg,brf,fls,toc,thm,out,fdb_latexmk,tex,md,pdf}

# clean directory (excluding .Rmd file)
clean: purge

.PHONY: all paper upload update download html purge clean
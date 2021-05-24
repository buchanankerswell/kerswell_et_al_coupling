#!/bin/zsh
echo 'making diff.tex ...'
latexdiff -t CFONT ../g3_v2/coupling_v2.tex ../g3_v3/coupling_v3.tex > diff.tex
echo 'cleaning up diff.tex ...'
sed -i '' 's:\\DIFaddbeginFL \\addlinespace:\\DIFaddbeginFL %DIFDELCMD < \\addlinespace:g' diff.tex
sed -i '' 's:\\DIFdelendFL \\addlinespace:\\DIFdelendFL %DIFDELCMD < \\addlinespace:g' diff.tex
echo 'making dff.pdf ...'
pdflatex -interaction scrollmode diff.tex xpdf diff.pdf
open diff.pdf
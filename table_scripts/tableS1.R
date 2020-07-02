# Load libraries
library(tidyverse)
library(huxtable)

# Set working directory to the location of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('regression_data.RData')
load('reduced_coupling_data.RData')

# Post-hoc table ----
pstHocTbl <-  as.data.frame(TukeyHSD(oneWayAnovaLith)[[1]])
colnames(pstHocTbl) <-
  c('Difference (km)',
    'Lower Bound (km)',
    'Upper Bound (km)',
    'Adj pvalue')
pstHocTbl %>%
  as_huxtable() %>%
  huxtable::add_rownames(colname = 'Model Comparison') %>%
  huxtable::add_colnames() %>%
  set_top_border(1, , 1) %>%
  set_top_border(2, , 1) %>%
  set_number_format(, 2, '%.3g') %>%
  set_number_format(, 3, '%.3g') %>%
  set_number_format(, 4, '%.3g') %>%
  set_number_format(, 5, '%.e') %>%
  set_width(0.25) %>%
  # set_position('left') %>%
  add_footnote(
    text = "Tukey's post-hoc test for comparing the mean coupling depths between groups
               of models with different backarc lithospheric thickness. Low pvalues indicate means
               are actually different"
  ) %>%
  quick_html(file = 'tableS1.html')
# quick_pdf(file = 'tableS1.pdf')
# quick_latex(file = 'tableS1.tex')
# quick_xlsx(file = 'tableS1.xlsx')
# quick_docx(file = 'tableS1.docx')
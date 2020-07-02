# Load libraries
library(tidyverse)
library(huxtable)

# Set working directory to the location of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('regression_data.RData')
load('reduced_coupling_data.RData')

# Create dataframe
tbl2 <- reducedAllModelsDataframe[, c(4,3,1,2)]
tbl2$model <- gsub('[1-9]', '', tbl2$model)
colnames(tbl2) <- c('Experiment', 'Lithospheric Thickness (km)', 'Thermal Parameter (km/100)', 'Coupling Depth (km)')

# Build Huxtable
tbl2 %>%
  as_huxtable() %>%
  add_colnames() %>%
  set_top_border(1, , 1) %>%
  set_top_border(2, , 1) %>%
  set_bottom_border(65, , 1) %>%
  set_width(0.25) %>%
  set_position('center') %>%
  quick_html(file = 'table2.html')
  # quick_pdf(file = 'table2.pdf')
  # quick_latex(file = 'table2.tex')
  # quick_xlsx(file = 'table2.xlsx')
  # quick_docx(file = 'table2.docx')
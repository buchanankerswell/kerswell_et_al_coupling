# Load libraries
library(tidyverse)
library(huxtable)

# Set working directory to the location of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('regression_data.RData')
load('reduced_coupling_data.RData')

# Create dataframe
tbl3 <- segs[, c(1, 3, 5, 6, 4, 2, 7)]
tbl3$z1100.reference <- c(1, 1, 1, 2, 2, 1, 3, 2, 1, 2, 1, 2, 2, 2, 1, 2, 1)
colnames(tbl3) <- c('Segment', 'Lithospheric Thickness (km)', 'Thermal Parameter (km/100)',
                    'Coupling Depth (km)', 'Thermal Parameter* (km/100)', 'Surface Heat Flow (mW/m^2)',
                    'Heat Flow Ref')
# Build Huxtable
tbl3 %>%
  as_huxtable() %>%
  add_colnames() %>%
  set_top_border(1, , 1) %>%
  set_top_border(2, , 1) %>%
  set_bottom_border(18, , 1) %>%
  add_footnote(
    text = "* These thermal parameter values, from Wada & Wang (2009), are equivalent to the values in column three multiplied by the sine of the dip angle. Only values in column three should be applied to equation (5).
    1 - Currie & Hyndman (2006)
    2 - Global average from Currie & Hyndman (2006)
    3 - Wada & Wang (2009)"
  ) %>%
  set_number_format(, 2, '%.3g') %>%
  set_number_format(, 3, '%.3g') %>%
  set_number_format(, 4, '%.3g') %>%
  set_number_format(, 5, '%.3g') %>%
  set_number_format(19, 1, NA) %>%
  set_width(1) %>%
  set_position('center') %>%
  # quick_html(file = 'table3.html')
  # quick_pdf(file = 'table3.pdf')
  # quick_latex(file = 'table3.tex')
  # quick_xlsx(file = 'table3.xlsx')
  quick_docx(file = 'table3.docx')
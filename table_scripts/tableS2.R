# Load libraries
library(tidyverse)
library(huxtable)

# Set working directory to the location of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('regression_data.RData')
load('reduced_coupling_data.RData')

# Regression summary table ----
huxreg(
  regModelAll,
  linRegLith,
  cubicRegLith,
  quadRegLith,
  linearMultivariateReg,
  cubicMultivariateReg,
  quadMultivariateReg,
  coefs = c(
    'Intercept (km)' = '(Intercept)',
    'tparam/100 (km/100)' = 'tparam',
    'z1100 (km)' = 'z1100',
    # '$z_{1100}$ (km)' = 'z1100',
    'z1100 (km)' = 'poly(z1100, 2)1',
    # '$z_{1100}$ (km)' = 'poly(z1100, 2)1',
    'z1100^2 (1/km)' = 'I(z1100^2)',
    # '$z_{1100}^{2}$ (1/km)' = 'I(z1100^2)',
    'z1100^2 (1/km)' = 'poly(z1100, 2)2'
    # '$z_{1100}^{2}$ (1/km)' = 'poly(z1100, 2)2'
  ),
  note = '{stars} | Standard Errors in parentheses (1s.e.) | Models are:
       (1) = zc~𝛷+c
       (2) = zc~z1100+c,
       (3) = zc~z1100^2+c,
       (4) = zc~z1100+z1100^2+c,
       (5) = zc~z1100+𝛷+c,
       (6) = zc~z1100^2+𝛷+c,
       (7) = zc~z1100+z1100^2+𝛷+c',
  statistics = c(N = "nobs", R2 = "r.squared", p.value = 'p.value')) %>%
  set_position('center') %>%
  set_width(0.75) %>%
  set_escape_contents(1:11, 1, FALSE) %>%
  set_number_format(2, 2:8, '%.3g') %>%
  set_number_format(3, 2:8, '%.3g') %>%
  set_number_format(4, 2:8, '%.3g') %>%
  set_number_format(5, 2:8, '%.3g') %>%
  set_number_format(6, 2:8, '%.3g') %>%
  set_number_format(7, 2:8, '%.3g') %>%
  set_number_format(8, 2:8, '%.3g') %>%
  set_number_format(9, 2:8, '%.3g') %>%
  set_number_format(10, 2:8, '%.3g') %>%
  set_number_format(11, 2:8, '%.3g') %>%
  set_number_format(12, 2:8, '%.e') %>%
  set_number_format(13, 1, NA) %>%
  quick_html(file = 'tableS2.html')
# quick_latex(file = 'tableS2.tex')
# quick_xlsx(file = 'tableS2.xlsx')
# quick_docx(file = 'tableS2.docx')
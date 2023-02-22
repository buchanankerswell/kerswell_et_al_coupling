#!/usr/bin/env Rscript

# Capture output
sink(file = paste0('data/log-', Sys.Date()), append = T, type = 'output', split = T)

# Create directories
dir.create('figs', showWarnings = F)
dir.create('draft/assets/figs', showWarnings = F)
dir.create('draft/assets/r', showWarnings = F)

# Load functions and libraries
cat(rep('~', 80), sep='')
cat('\nLoading packages and functions ...')
source('R/functions.R')

suppressMessages({
  # Read data for antigorite stability depth thru time for cdf models
  cat('\nRead data for antigorite stability depth thru time for cdf models ...')
  antigorite.stability <-
    read_csv('data/numerical_results/antigorite-stability.csv', show_col_types = F)
  # Read Penniston-Dorland et al. (2015) dataset
  cat('\nReading Penniston-Dorland et al. (2015) dataset ...')
  pd15 <- read_csv('data/pd15.csv', show_col_types = F)
  # Read model geotherms
  cat('\nReading model geotherms ...')
  geotherms <- read_csv('data/numerical_results/geotherms.csv', show_col_types = F)
  # Combining data
  cat('\nCombining data ...')
  mods <-  read_csv('data/numerical_results/coupling.csv', show_col_types = F)
  hf <- read_csv('data/numerical_results/heat-flow.csv', show_col_types = F)
})

# Save
cat('\nSaving data to: data/preprocessed.RData ...')
save(antigorite.stability, pd15, geotherms, mods, hf, file = 'data/preprocessed.RData')

# Write log
cat('\npreprocess.R complete!\n')
sink()

#!/usr/bin/env Rscript

# Capture output
sink(file = paste0('data/log-', Sys.Date()), append = T, type = 'output', split = T)
# Set download timeout to 10min
cat(rep('~', 80), sep='')
cat('\nSetting download timeout to 10 minutes ...')
options(timeout = 600)
# Download large numerical model binary files from OSF ()
# https://osf.io/zjac3/files/osfstorage
data.url <-
  'https://files.osf.io/v1/resources/zjac3/providers/osfstorage/63750d32069fa6079377aced/?zip='
# Download .zip file
cat('\nDownloading data from osf ...')
cat('\nurl: https://osf.io/zjac3/files/osfstorage')
cat('\nThis may take > 2-3 minutes ...')
download.file(data.url, 'data.zip', quiet = T)
# Extract
unzip('data.zip', exdir = 'data')
# Remove .zip file
file.remove('data.zip')
# Write log
cat('\ndownload-data.R complete!\n')
sink()
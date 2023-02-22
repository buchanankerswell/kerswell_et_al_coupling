#!/bin/zsh

# Clock time
SECONDS=0
# Exit if any command fails
set -e
# Check for R dependencies
R/packages.R
# Check for files in data directory
FNUM=$(find data -name '*' -print | wc -l)
if [[ $FNUM -lt 49 ]]; then
  # Download data from osf
  # https://osf.io/zjac3/files/osfstorage
  R/download-data.R
fi
# Preprocess data
R/preprocess.R
# Run regressions
R/regression.R
# Visualize
R/visualize.R
# Move data and figs to draft folder for manuscript
cp data/*.RData draft/assets/r/
cp data/log* draft/assets/r/
cp figs/*.png draft/assets/figs/
# Print clock time
t=$SECONDS
printf '\nTime taken: %d days, %d minutes, %d seconds\n' \
  "$(( t/86400 ))" "$(( t/60 - 1440*(t/86400) ))" "$(( t ))"
exit 0
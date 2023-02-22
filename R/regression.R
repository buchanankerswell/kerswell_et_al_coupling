#!/usr/bin/env Rscript

# Capture output
sink(file = paste0('data/log-', Sys.Date()), append = T, type = 'output', split = T)

# Load functions and libraries
cat(rep('~', 80), sep='')
cat('\nLoading packages and functions ...')
source('R/functions.R')

# Load dataset
cat('\nLoading compiled data from preprocessed.RData')
load('data/preprocessed.RData')
ls.rm <- ls()

suppressWarnings({
  bivariate.model.thermal.parameter.linear.indv <-
    map(
      c(46, 62, 78, 94),
      ~build_linear_model(
        data = filter(mods, upper.plate.thickness == .x),
        x = 'thermal.parameter',
        y = 'coupling.depth',
        type = 'linear'
      )
    )
  bivariate.model.thermal.parameter.linear <-
    build_linear_model(
      data = mods,
      x = 'thermal.parameter',
      y = 'coupling.depth',
      type = 'linear'
    )
  bivariate.model.upper.plate.thickness.linear <-
    build_linear_model(
      data = mods,
      x = 'upper.plate.thickness',
      y = 'coupling.depth',
      type = 'linear',
      anova = T
    )
  bivariate.model.upper.plate.thickness.quadratic1 <-
    build_linear_model(
      data = mods,
      x = 'upper.plate.thickness',
      y = 'coupling.depth',
      type = 'quadratic1'
    )
  bivariate.model.upper.plate.thickness.quadratic2 <-
    build_linear_model(
      data = mods,
      x = 'upper.plate.thickness',
      y = 'coupling.depth',
      type = 'quadratic2'
    )
  multivariate.model.linear <-
    build_linear_model(
      data = mods,
      x = 'upper.plate.thickness',
      y = 'coupling.depth',
      z = 'thermal.parameter',
      type = 'linear'
    )
  multivariate.model.quadratic1 <-
    build_linear_model(
      data = mods,
      x = 'upper.plate.thickness',
      y = 'coupling.depth',
      z = 'thermal.parameter',
      type = 'quadratic1'
    )
  multivariate.model.quadratic2 <-
    build_linear_model(
      data = mods,
      x = 'upper.plate.thickness',
      y = 'coupling.depth',
      z = 'thermal.parameter',
      type = 'quadratic2'
    )
})

# Predict coupling depth for Wada & Wang (2009) segments
cat('\nPredicting coupling depth for Wada & Wang (2009) segments ...')
segs <-
  suppressMessages(read_csv('data/segments.csv', show_col_types = F)) %>%
  mutate(
    coupling.depth.linear =
      multivariate.model.linear$model$estimate[1] +
      multivariate.model.linear$model$estimate[2] *
      upper.plate.thickness +
      multivariate.model.linear$model$estimate[3] *
      thermal.parameter,
    coupling.depth.quadratic1 =
      multivariate.model.quadratic1$model$estimate[1] +
      multivariate.model.quadratic1$model$estimate[2] *
      upper.plate.thickness * upper.plate.thickness +
      multivariate.model.quadratic1$model$estimate[3] *
      thermal.parameter,
    coupling.depth.quadratic2 =
      multivariate.model.quadratic2$model$estimate[1] +
      multivariate.model.quadratic2$model$estimate[2] *
      upper.plate.thickness +
      multivariate.model.quadratic2$model$estimate[3] *
      upper.plate.thickness *
      upper.plate.thickness +
      multivariate.model.quadratic2$model$estimate[4] *
      thermal.parameter,
    .before = reference
  )
print(segs)

# Read I2VIS output
cat('\nReading raw I2VIS binary output\n')
n46.init <- read_nodes('data/numerical_results/cdf46_0.prn')
r46.init <- read_rock_nodes('data/numerical_results/cdf46_c0.txt')
n62.init <- read_nodes('data/numerical_results/cdf62_0.prn')
r62.init <- read_rock_nodes('data/numerical_results/cdf62_c0.txt')
n78.init <- read_nodes('data/numerical_results/cdf78_0.prn')
r78.init <- read_rock_nodes('data/numerical_results/cdf78_c0.txt')
n94.init <- read_nodes('data/numerical_results/cdf94_0.prn')
r94.init <- read_rock_nodes('data/numerical_results/cdf94_c0.txt')

# Read I2VIS output
cat('\nReading raw I2VIS binary output\n')
n46 <- read_nodes('data/numerical_results/cdf46_250.prn')
r46 <- read_rock_nodes('data/numerical_results/cdf46_c250.txt')
n62 <- read_nodes('data/numerical_results/cdf62_250.prn')
r62 <- read_rock_nodes('data/numerical_results/cdf62_c250.txt')
n78_50 <- read_nodes('data/numerical_results/cdf78_50.prn')
r78_50 <- read_rock_nodes('data/numerical_results/cdf78_c50.txt')
n78_130 <- read_nodes('data/numerical_results/cdf78_130.prn')
r78_130 <- read_rock_nodes('data/numerical_results/cdf78_c130.txt')
n78_250 <- read_nodes('data/numerical_results/cdf78_250.prn')
r78_250 <- read_rock_nodes('data/numerical_results/cdf78_c250.txt')
n94 <- read_nodes('data/numerical_results/cdf94_250.prn')
r94 <- read_rock_nodes('data/numerical_results/cdf94_c250.txt')

# Save
cat('\nSaving raw model data for visualization ...')
save(n46.init, r46.init, n46, r46, file = 'data/numerical_results/cdf46.RData')
save(n62.init, r62.init, n62, r62, file = 'data/numerical_results/cdf62.RData')
save(n78_50, r78_50, file = 'data/numerical_results/cdf78_50.RData')
save(n78_130, r78_130, file = 'data/numerical_results/cdf78_130.RData')
save(n78.init, r78.init, n78_250, r78_250, file = 'data/numerical_results/cdf78_250.RData')
save(n94.init, r94.init, n94, r94, file = 'data/numerical_results/cdf94.RData')

# Clean up environment
rm(
  n46.init, r46.init, n46, r46,
  n62.init, r62.init, n62, r62,
  n78.init, r78.init, n78_250, r78_250,
  n94.init, r94.init, n94, r94
)

# Clean up environment
rm(list = ls.rm)
rm(ls.rm)
rm(list=lsf.str())

# Save
cat('\nSaving regression data to data/regressions.RData')
save.image(file = 'data/regressions.RData')

# Write log
cat('\nregression.R complete!\n')
sink()
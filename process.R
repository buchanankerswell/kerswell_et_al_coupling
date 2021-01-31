source('functions.R')
  
# Load dataset
load('reduced_data.RData')
ls.rm <- ls()

suppressWarnings({
  # Bivariate regressions phi vs. zc
  bvm.phi.lin <- map(c('46', '62', '78', '94'),
                     ~m.lin(data = mods %>% filter(z1100==.x), x = 'phi', y = 'zc', type = 'lin', file = paste0('figs/lin_phi_zc_', .x)))
  
  # Bivariate regressions z1100 vs. zc
  bvm.z1100.lin <- m.lin(data = mods, x = 'z1100', y = 'zc', type = 'lin', anova = T, plot = T, file = 'figs/lin_z1100_zc_')
  bvm.z1100.quad1 <- m.lin(data = mods, x = 'z1100', y = 'zc',  type = 'quad1', file = 'figs/quad1_z1100_zc_')
  bvm.z1100.quad2 <- m.lin(data = mods, x = 'z1100', y = 'zc', type = 'quad2', file = 'figs/quad2_z1100_zc_')
  
  # Multivariate regressions z1100 + phi vs. zc
  mvm.lin <- m.lin(data = mods, x = 'z1100', y = 'zc', z = 'phi', type = 'lin', file = 'figs/lin_z1100_phi_zc_')
  mvm.quad1 <- m.lin(data = mods, x = 'z1100', y = 'zc', z = 'phi',  type = 'quad1', file = 'figs/quad1_z1100_phi_zc_')
  mvm.quad2 <- m.lin(data = mods, x = 'z1100', y = 'zc', z = 'phi', type = 'quad2', file = 'figs/quad2_z1100_phi_zc_')
})

# Predict coupling depth for Wada & Wang (2009) segments
cat('Predicting coupling depth for Wada & Wang (2009) segments:\n')
segs <- suppressMessages(read_tsv('coupling_data/segments.tsv')) %>% 
  mutate(zc.lin = mvm.lin$model$estimate[1] + mvm.lin$model$estimate[2]*z1100 + mvm.lin$model$estimate[3]*phi,
         zc.quad1 = mvm.quad1$model$estimate[1] + mvm.quad1$model$estimate[2]*z1100*z1100 + mvm.quad1$model$estimate[3]*phi,
         zc.quad2 = mvm.quad2$model$estimate[1] + mvm.quad2$model$estimate[2]*z1100 + mvm.quad2$model$estimate[3]*z1100*z1100 + mvm.quad2$model$estimate[4]*phi,
         .before = z1100.ref)
print(segs)

# Clean up environment
rm(list = ls.rm)
rm(ls.rm)

# Save
save.image(file = 'regression_data.RData')
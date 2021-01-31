source('functions.R')
  
# Load dataset
load('reduced_data.RData')
ls.rm <- ls()

# Bivariate regressions phi vs. zc
map(c('46', '62', '78', '94'),
    ~m.lin(data = mods %>% filter(z1100==.x), x = 'phi', y = 'zc', type = 'lin', file = paste0('figs/lin_phi_zc_', .x)))

# Bivariate regressions z1100 vs. zc
m.lin(data = mods, x = 'z1100', y = 'zc', type = 'lin', anova = T, plot = T, file = 'figs/lin_z1100_zc_')
m.lin(data = mods, x = 'z1100', y = 'zc',  type = 'quad1', file = 'figs/quad1_z1100_zc_')
m.lin(data = mods, x = 'z1100', y = 'zc', type = 'quad2', file = 'figs/quad2_z1100_zc_')

# Multivariate regressions z1100 + phi vs. zc
mvm.lin <- m.lin(data = mods, x = 'z1100', y = 'zc', z = 'phi', type = 'lin', file = 'figs/lin_z1100_phi_zc_')
mvm.quad1 <- m.lin(data = mods, x = 'z1100', y = 'zc', z = 'phi',  type = 'quad1', file = 'figs/quad1_z1100_phi_zc_')
mvm.quad2 <- m.lin(data = mods, x = 'z1100', y = 'zc', z = 'phi', type = 'quad2', file = 'figs/quad2_z1100_phi_zc_')

# Read data for subduction zone segments (Wada & Wang, 2009) ----
read_tsv('coupling_data/segments.tsv') %>% 
  mutate(zc.lin = mvm.lin$model$estimate[1] + mvm.lin$model$estimate[2]*z1100 + mvm.lin$model$estimate[3]*phi,
         zc.quad1 = mvm.quad1$model$estimate[1] + mvm.quad1$model$estimate[2]*z1100*z1100 + mvm.quad1$model$estimate[3]*phi,
         zc.quad2 = mvm.quad2$model$estimate[1] + mvm.quad2$model$estimate[2]*z1100*z1100 + mvm.quad2$model$estimate[3]*z1100 + mvm.quad2$model$estimate[4]*phi,
         .before = z1100.ref)

# Calculate predicted coupling depths for real world subduction zones from data in
# (Wada & Wang, 2009) based on our cubic multivariate regression
segs$z.coupling <- cubicMultivariateReg$coefficients[[1]] + 
  cubicMultivariateReg$coefficients[[2]]*segs$z1100^2 + 
  cubicMultivariateReg$coefficients[[3]]*segs$tparam

# Save dataset ----
rm(list = loaded.data)
rm(loaded.data)

save.image(file = 'regression_data.RData')
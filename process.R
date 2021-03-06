source('functions.R')

# Load dataset
cat('Loading compiled data from "data.RData"\n')
load('data/data.RData')
ls.rm <- ls()

suppressWarnings({
  # Bivariate regressions phi vs. zc
  bvm.phi.lin.indv <- map(c('46', '62', '78', '94'),
                     ~m.lin(data = mods %>% filter(z1100==.x), x = 'phi', y = 'zc', type = 'lin', file = paste0('figs/lin_phi_zc_', .x)))
  bvm.phi.lin <- m.lin(data = mods, x = 'phi', y = 'zc', type = 'lin', plot = T, file = 'figs/lin_phi_zc')
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
segs <- suppressMessages(read_tsv('data/segments.tsv')) %>%
  mutate(zc.lin = mvm.lin$model$estimate[1] + mvm.lin$model$estimate[2]*z1100 + mvm.lin$model$estimate[3]*phi,
         zc.quad1 = mvm.quad1$model$estimate[1] + mvm.quad1$model$estimate[2]*z1100*z1100 + mvm.quad1$model$estimate[3]*phi,
         zc.quad2 = mvm.quad2$model$estimate[1] + mvm.quad2$model$estimate[2]*z1100 + mvm.quad2$model$estimate[3]*z1100*z1100 + mvm.quad2$model$estimate[4]*phi,
         .before = z1100.ref)
print(segs)

# Read I2VIS output
# cat('Reading raw model data for visualization\n')
# n46.init <- read_nodes('data/46km/cdf46_0.prn')
# r46.init <- read_rock_nodes('data/46km/cdf46_c0.txt')
# n62.init <- read_nodes('data/62km/cdf62_0.prn')
# r62.init <- read_rock_nodes('data/62km/cdf62_c0.txt')
# n78.init <- read_nodes('data/78km/cdf78_0.prn')
# r78.init <- read_rock_nodes('data/78km/cdf78_c0.txt')
# n94.init <- read_nodes('data/94km/cdf94_0.prn')
# r94.init <- read_rock_nodes('data/94km/cdf94_c0.txt')
# 
# Read I2VIS output
# cat('Reading raw model data for visualization\n')
# n46 <- read_nodes('data/46km/cdf46_250.prn')
# r46 <- read_rock_nodes('data/46km/cdf46_c250.txt')
# n62 <- read_nodes('data/62km/cdf62_250.prn')
# r62 <- read_rock_nodes('data/62km/cdf62_c250.txt')
# n78_50 <- read_nodes('data/78km/cdf78_50.prn')
# r78_50 <- read_rock_nodes('data/78km/cdf78_c50.txt')
# n78_130 <- read_nodes('data/78km/cdf78_130.prn')
# r78_130 <- read_rock_nodes('data/78km/cdf78_c130.txt')
# n78_250 <- read_nodes('data/78km/cdf78_250.prn')
# r78_250 <- read_rock_nodes('data/78km/cdf78_c250.txt')
# n94 <- read_nodes('data/94km/cdf94_250.prn')
# r94 <- read_rock_nodes('data/94km/cdf94_c250.txt')
# 
# # Save
# save(n46.init, r46.init, n46, r46, file = 'data/46km/cdf46.RData')
# save(n62.init, r62.init, n62, r62, file = 'data/62km/cdf62.RData')
# save(n78_50, r78_50, file = 'data/78km/cdf78_50.RData')
# save(n78_130, r78_130, file = 'data/78km/cdf78_130.RData')
# save(n78.init, r78.init, n78_250, r78_250, file = 'data/78km/cdf78_250.RData')
# save(n94.init, r94.init, n94, r94, file = 'data/94km/cdf94.RData')
# 
# # Experiments table
# # Works only if TeX live distro is installed
# cat('Saving experiments to tables/experiments.pdf\n')
# mods %>%
#   select(model, phi, z1100, zc) %>%
#   kbl(col.names = c('', '$km/100$', '$km$', '$km$'),
#       booktabs = T,
#       longtable = T,
#       caption = 'Experimental results',
#       format = 'latex',
#       escape = F) %>%
#   add_header_above(c('Experiment', '$\\\\phi/100$', '$z_{1100}$', '$z_c$'),
#                    escape = F,
#                    line = F) %>%
#   kable_classic() %>%
#   kable_styling(full_width = F,
#                 latex_options = c('repeat_header')) %>%
#   save_kable(file = 'tables/experiments.pdf')
#
# # Segments table
# # Works only if TeX live distro is installed
# cat('Saving predicted coupling depth results to tables/predicted_zc.pdf\n')
# segs %>%
#   select(segment, qs, phi, z1100, zc.lin, zc.quad1, zc.quad2) %>%
#   mutate('phi' = round(phi, 1),
#          'zc.lin' = round(zc.lin),
#          'zc.quad1' = round(zc.quad1),
#          'zc.quad2' = round(zc.quad2),
#          ref = c(1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)) %>%
#   kbl(col.names = c('', '$mW/m^2$', '$km/100$', '$km$', '$km$', '$km$', '$km$', ''),
#       booktabs = T,
#       caption = 'Predicted coupling depth for modern subduction zone segments',
#       format = 'latex',
#       escape = F) %>%
#   add_header_above(c('Segment', '$q_s$', '$\\\\phi/100$', '$z_{1100}$', '$z_c^a$', '$z_c^b$', '$z_c^c$', 'Reference^d'),
#                    escape = F,
#                    line = F) %>%
#   kable_classic() %>%
#   kable_styling(latex_options = c('scale_down')) %>%
#   footnote(general = c('$q_s$=average backarc surface heat flow: from Currie & Hyndman (2006)',
#                        '$\\\\phi$=thermal parameter',
#                        '$z_{1100}$=lithospheric thickness'),
#            alphabet = c('Predicted coupling depth using: $z_c=z_{1100}+\\\\phi$',
#                       'Predicted coupling depth using: $z_c=z_{1100}^2+\\\\phi$',
#                       'Predicted coupling depth using: $z_c=z_{1100}+z_{1100}^2+\\\\phi$',
#                       '1=Currie \\\\& Hyndman (2006), 2=Wada \\\\& Wang (2009)'),
#            threeparttable = T,
#            escape = F) %>%
#   save_kable(file = 'tables/predicted_zc.pdf')
#
# # ANOVA table
# # Works only if TeX live distro is installed
# cat('Saving ANOVA results to tables/anova.pdf\n')
# bvm.z1100.lin$anova %>%
#   select(-null.value) %>%
#   mutate('estimate' = round(estimate, 1),
#          'conf.low' = round(conf.low, 1),
#          'conf.high' = round(conf.high, 1),
#          'adj.p.value' = scientific(adj.p.value, digits = 3),
#          'term' = '$z_{1100}$') %>%
#   kbl(col.names = c('', '', '$km$', '$km$', '$km$', ''),
#       booktabs = T,
#       caption = 'Summary of ANOVA test',
#       format = 'latex',
#       escape = F) %>%
#   add_header_above(c('Term', 'Contrast', 'Estimate', 'Upper Bound', 'Lower Bound', 'p value'),
#                    escape = F,
#                    line = F) %>%
#   kable_classic() %>%
#   kable_styling(latex_options = c('scale_down')) %>%
#   footnote(general = c("Pair-wise Tukey's test comparing means between groups",
#                        'Estimates are differences between means',
#                        'Null hypothesis is that means are not different'),
#            threeparttable = T,
#            escape = F) %>%
#   save_kable(file = 'tables/anova.pdf')
#
# # Regression summary table
# # Works only if TeX live distro is installed
# cat('Saving regression results to tables/regression.pdf\ntables/reg_fits.pdf\n')
# map_df(list(bvm.phi.lin,
#          bvm.z1100.lin,
#          bvm.z1100.quad1,
#          bvm.z1100.quad2,
#          mvm.lin,
#          mvm.quad1,
#          mvm.quad2),
#     ~.x$model, .id = 'reg') %>%
#   select(-statistic) %>%
#   mutate('estimate' = round(estimate, 1),
#          'std.error' = round(std.error, 1),
#          'p.value' = scientific(p.value, digits = 3),
#          'term' = ifelse(term == 'phi', '$\\phi$',
#                          ifelse(term == 'z1100', '$z_{1100}',
#                                 ifelse(term == 'z1100^2', 'z_{1100}^2', term)))) %>%
#   kbl(col.names = c('Model', 'Term', 'Estimate', 'Std. Error', 'p value'),
#       booktabs = T,
#       caption = 'Summary of regression models',
#       format = 'latex',
#       escape = F) %>%
#   kable_classic() %>%
#   footnote(number = c('$z_c=\\\\phi$',
#                       '$z_c=z_{1100}$',
#                       '$z_c=z_{1100}^2$',
#                       '$z_c=z_{1100}+z_{1100}^2$',
#                       '$z_c=z_{1100}+\\\\phi$',
#                       '$z_c=z_{1100}^2+\\\\phi$',
#                       '$z_c=z_{1100}+z_{1100}^2+\\\\phi$'),
#            threeparttable = T,
#            escape = F) %>%
#   save_kable(file = 'tables/regression.pdf')
#
# # Regression fits table
# # Works only if TeX live distro is installed
# map_df(list(bvm.phi.lin,
#             bvm.z1100.lin,
#             bvm.z1100.quad1,
#             bvm.z1100.quad2,
#             mvm.lin,
#             mvm.quad1,
#             mvm.quad2),
#        ~.x$fit, .id = 'reg') %>%
#   select(reg, r.squared, p.value, AIC) %>%
#   mutate('r.squared' = round(r.squared, 3),
#          'p.value' = scientific(p.value, digits = 3),
#          'AIC' = round(AIC)) %>%
#   kbl(col.names = c('Model', '$R^2$', 'p value', 'AIC^a'),
#       booktabs = T,
#       caption = 'Summary of regression model fits',
#       format = 'latex',
#       escape = F) %>%
#   kable_classic() %>%
#   footnote(alphabet = 'AIC = Akaike Information Criterion',
#            number = c('$z_c=\\\\phi$',
#                       '$z_c=z_{1100}$',
#                       '$z_c=z_{1100}^2$',
#                       '$z_c=z_{1100}+z_{1100}^2$',
#                       '$z_c=z_{1100}+\\\\phi$',
#                       '$z_c=z_{1100}^2+\\\\phi$',
#                       '$z_c=z_{1100}+z_{1100}^2+\\\\phi$'),
#            threeparttable = T,
#            escape = F) %>%
#   save_kable(file = 'tables/reg_fits.pdf')
# 
# # Clean up environment
# rm(n46.init, r46.init, n46, r46,
#    n62.init, r62.init, n62, r62,
#    n78.init, r78.init, n78_250, r78_250,
#    n94.init, r94.init, n94, r94)

# Clean up environment
rm(list = ls.rm)
rm(ls.rm)
rm(list=lsf.str())

# Save
cat('Saving regression data to data/regressions.RData')
save.image(file = 'data/regressions.RData')

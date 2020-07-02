# Performing Regressions and testing fitted models
# Load libraries
library(tidyverse)
library(mblm)
library(mosaic)
library(huxtable)
library(directlabels)
# Set working directory to the location of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Load dataset
load('reduced_coupling_data.RData')
loaded.data <- ls()
# Open pdf document to save plots
cairo_pdf(file = 'regression.plots.pdf', width = 5.5, height = 4.25, onefile = TRUE)

# Fitting linear models for predicting coupling depth from thermal parameter
# 46km lithosphere models ----

# Regular linear model
plot(
   reducedDataFrame46$tparam,
   reducedDataFrame46$z.coupling,
   xlab = expression('Thermal Parameter' ~ Phi ~ '/100 (km)'),
   ylab = 'Coupling Depth (km)'
)
regModel46 <- lm(z.coupling ~ tparam, data = reducedDataFrame46)
summary(regModel46)
abline(
   regModel46$coefficients[1],
   regModel46$coefficients[2],
   col = 'red',
   lwd = 2
)
# Checking assumption of equal residuals (histogram should be approx. normal and
# qq plots should be a straight line)
hist(regModel46$residuals)
qqnorm(regModel46$residuals)
qqline(regModel46$residuals, col = 'red', lwd = 2)

# Model equation
regModel46Eq <-
   substitute(italic(z[coupling]) == a(''%+-%''~c) + b(''%+-%''~d) %.% italic(frac(Phi, 100)),
              list(
                 a = format(regModel46$coefficients[[1]], digits = 3),
                 b = format(regModel46$coefficients[[2]], digits = 3),
                 c = format(summary(regModel46)$coefficients[[3]], digits = 3),
                 d = format(summary(regModel46)$coefficients[[4]], digits = 3)
                 )
              )

# 62km lithosphere models ----

# Regular linear model
plot(
   reducedDataFrame62$tparam,
   reducedDataFrame62$z.coupling,
   xlab = expression('Thermal Parameter' ~ Phi ~ '/100 (km)'),
   ylab = 'Coupling Depth (km)'
)
regModel62 <- lm(z.coupling ~ tparam, data = reducedDataFrame62)
summary(regModel62)
abline(
   regModel62$coefficients[1],
   regModel62$coefficients[2],
   col = 'red',
   lwd = 2
)
# Checking assumption of equal residuals (histogram should be approx. normal and
# qq plots should be a straight line)
hist(regModel62$residuals)
qqnorm(regModel62$residuals)
qqline(regModel62$residuals, col = 'red', lwd = 2)

# Model equation
regModel62Eq <-
   substitute(italic(z[coupling]) == a(''%+-%''~c) + b(''%+-%''~d) %.% italic(frac(Phi, 100)),
              list(
                 a = format(regModel62$coefficients[[1]], digits = 3),
                 b = format(regModel62$coefficients[[2]], digits = 3),
                 c = format(summary(regModel62)$coefficients[[3]], digits = 3),
                 d = format(summary(regModel62)$coefficients[[4]], digits = 3)
              ))

# 78km lithosphere models ----

# Regular linear model
plot(
   reducedDataFrame78$tparam,
   reducedDataFrame78$z.coupling,
   xlab = expression('Thermal Parameter' ~ Phi ~ '/100 (km)'),
   ylab = 'Coupling Depth (km)'
)
regModel78 <- lm(z.coupling ~ tparam, data = reducedDataFrame78)
summary(regModel78)
abline(
   regModel78$coefficients[1],
   regModel78$coefficients[2],
   col = 'red',
   lwd = 2
)
# Checking assumption of equal residuals (histogram should be approx. normal and
# qq plots should be a straight line)
hist(regModel78$residuals)
qqnorm(regModel78$residuals)
qqline(regModel78$residuals, col = 'red', lwd = 2)

# Model equation
regModel78Eq <-
   substitute(italic(z[coupling]) == a(''%+-%''~c) + b(''%+-%''~d) %.% italic(frac(Phi,100)),
              list(
                 a = format(regModel78$coefficients[[1]], digits = 3),
                 b = format(regModel78$coefficients[[2]], digits = 3),
                 c = format(summary(regModel78)$coefficients[[3]], digits = 3),
                 d = format(summary(regModel78)$coefficients[[4]], digits = 3)
              ))
# 94km lithosphere models ----

# Regular linear model
plot(
   reducedDataFrame94$tparam,
   reducedDataFrame94$z.coupling,
   xlab = expression('Thermal Parameter' ~ Phi ~ '/100 (km)'),
   ylab = 'Coupling Depth (km)'
)
regModel94 <- lm(z.coupling ~ tparam, data = reducedDataFrame94)
summary(regModel94)
abline(
   regModel94$coefficients[1],
   regModel94$coefficients[2],
   col = 'red',
   lwd = 2
)
# Checking assumption of equal residuals (histogram should be approx. normal and
# qq plots should be a straight line)
hist(regModel94$residuals)
qqnorm(regModel94$residuals)
qqline(regModel94$residuals, col = 'red', lwd = 2)

# Model equation
regModel94Eq <-
   substitute(italic(z[coupling]) == a(''%+-%''~c) + b(''%+-%''~d) %.% italic(frac(Phi, 100)),
              list(
                 a = format(regModel94$coefficients[[1]], digits = 3),
                 b = format(regModel94$coefficients[[2]], digits = 3),
                 c = format(summary(regModel94)$coefficients[[3]], digits = 3),
                 d = format(summary(regModel94)$coefficients[[4]], digits = 3)
              ))

# All data ----

# Regular linear model
plot(
   reducedAllModelsDataframe$tparam,
   reducedAllModelsDataframe$z.coupling,
   xlab = expression('Thermal Parameter' ~ Phi ~ '/100 (km)'),
   ylab = 'Coupling Depth (km)'
)
regModelAll <-
   lm(z.coupling ~ tparam, data = reducedAllModelsDataframe)
summary(regModelAll)
abline(
   regModelAll$coefficients[1],
   regModelAll$coefficients[2],
   col = 'red',
   lwd = 2
)
# Checking assumption of equal residuals (histogram should be approx. normal and
# qq plots should be a straight line)
hist(regModelAll$residuals)
qqnorm(regModelAll$residuals)
qqline(regModelAll$residuals, col = 'red', lwd = 2)

# Model equation
regModelAllEq <-
   substitute(italic(z[coupling]) == a(''%+-%''~c) + b(''%+-%''~d) %.% italic(frac(Phi, 100)),
              list(
                 a = format(regModelAll$coefficients[[1]], digits = 3),
                 b = format(regModelAll$coefficients[[2]], digits = 3),
                 c = format(summary(regModelAll)$coefficients[[3]], digits = 3),
                 d = format(summary(regModelAll)$coefficients[[4]], digits = 3)
              ))

# Linear and quadratic regression models for predicting coupling depth from ----
# Lithospheric thickness
plot(
   reducedAllModelsDataframe$z1100,
   reducedAllModelsDataframe$z.coupling,
   xlab = 'Lithospheric Thickness (km)',
   ylab = 'Coupling Depth (km)'
)
linRegLith <-
   lm(z.coupling ~ z1100, data = reducedAllModelsDataframe)
summary(linRegLith)
cubicRegLith <- lm(z.coupling ~ I(z1100^2), data = reducedAllModelsDataframe)
summary(cubicRegLith)
quadRegLith <-
   lm(z.coupling ~ poly(z1100, 2), data = reducedAllModelsDataframe)
summary(quadRegLith)
lines(
   seq(0, 100, length.out = 150),
   predict(linRegLith, list(z1100 = seq(0, 100, length.out = 150))),
   col = 'blue',
   lwd = 2
)
lines(
   seq(0, 100, length.out = 150),
   predict(cubicRegLith, list(z1100 = seq(0, 100, length.out = 150))),
   col = 'red',
   lwd = 2
)
lines(
   seq(0, 100, length.out = 150),
   predict(quadRegLith, list(z1100 = seq(0, 100, length.out = 150))),
   col = 'green',
   lwd = 2
)
# Checking assumption of equal residuals (histogram should be approx. normal and
# qq plots should be a straight line)
hist(linRegLith$residuals)
qqnorm(linRegLith$residuals)
qqline(linRegLith$residuals, col = 'blue', lwd = 2)
hist(cubicRegLith$residuals)
qqnorm(cubicRegLith$residuals)
qqline(cubicRegLith$residuals, col = 'red', lwd = 2)
hist(quadRegLith$residuals)
qqnorm(quadRegLith$residuals)
qqline(quadRegLith$residuals, col = 'red', lwd = 2)

# Model equations
cubicRegLithEq <-
   substitute(
      italic(z[coupling]) == a(''%+-%''~c) + b(''%+-%''~d) %.% italic(z[1100]^2),
      list(
         a = format(cubicRegLith$coefficients[[1]], digits = 3),
         b = format(cubicRegLith$coefficients[[2]], digits = 3),
         c = format(summary(cubicRegLith)$coefficients[[3]], digits = 3),
         d = format(summary(cubicRegLith)$coefficients[[4]], digits = 3)
      )
   )
quadRegLithEq <-
   substitute(
      italic(z[coupling]) == a(''%+-%''~d) + b(''%+-%''~e) %.% italic(z[1100]) + c(''%+-%''~f) %.% italic(z[1100]^2),
      list(
         a = format(quadRegLith$coefficients[[1]], digits = 3),
         b = format(quadRegLith$coefficients[[2]], digits = 3),
         c = format(quadRegLith$coefficients[[3]], digits = 3),
         d = format(summary(quadRegLith)$coefficients[[4]], digits = 3),
         e = format(summary(quadRegLith)$coefficients[[5]], digits = 3),
         f = format(summary(quadRegLith)$coefficients[[6]], digits = 3)
      )
   )

# ANOVA for testing if differences in coupling depths between groups of models with diff.
# lithospheric thicknesses are real (or significant)
plot(
   as.factor(reducedAllModelsDataframe$z1100),
   reducedAllModelsDataframe$z.coupling,
   xlab = 'Lithospheric Thickness (km)',
   ylab = 'Coupling Depth (km)'
)
oneWayAnovaLith <-
   lm(z.coupling ~ as.factor(z1100), data = reducedAllModelsDataframe)
summary(oneWayAnovaLith)
anova(oneWayAnovaLith)

# Checking assumption of equal residuals (histogram should be approx. normal and
# qq plots should be a straight line)
hist(oneWayAnovaLith$residuals)
qqnorm(oneWayAnovaLith$residuals)
qqline(oneWayAnovaLith$residuals, col = 'red', lwd = 2)

# Checking assumption of equal variance among groups (p-value < 0.05 means that
# group variances are *not* equal. High p-value = good)
bartlett.test(z.coupling ~ as.factor(z1100), data = reducedAllModelsDataframe)

# Post-hoc testing lithospheric thickness groups (checking if there is significant
# differences among all pair-wise groups)
TukeyHSD(oneWayAnovaLith)

# Multivariate regression models predicting coupling depth from both
# lithospheric thickness and thermal parameter ----
linearMultivariateReg <-
   lm(z.coupling ~ z1100 + tparam, data = reducedAllModelsDataframe)
summary(linearMultivariateReg)
cubicMultivariateReg <-
   lm(z.coupling ~ I(z1100 ^ 2) + tparam, data = reducedAllModelsDataframe)
summary(cubicMultivariateReg)
quadMultivariateReg <-
   lm(z.coupling ~ poly(z1100, 2) + tparam, data = reducedAllModelsDataframe)
summary(quadMultivariateReg)

# Checking assumption of equal residuals (histogram should be approx. normal and
# qq plots should be a straight line)
hist(linearMultivariateReg$residuals)
qqnorm(linearMultivariateReg$residuals)
qqline(linearMultivariateReg$residuals,
       col = 'red',
       lwd = 2)
hist(cubicMultivariateReg$residuals)
qqnorm(cubicMultivariateReg$residuals)
qqline(cubicMultivariateReg$residuals,
       col = 'red',
       lwd = 2)
hist(quadMultivariateReg$residuals)
qqnorm(quadMultivariateReg$residuals)
qqline(quadMultivariateReg$residuals,
       col = 'red',
       lwd = 2)

# Nonlinear Model equation
cubicMultivariateRegEq <-
   substitute(
      italic(z[coupling]) == b %.% italic(z[1100]) ^ 2 + c %.% italic(frac(Phi, 100)) + d,
      list(
         b = format(coef(cubicMultivariateReg)[[2]], digits = 3),
         c = format(coef(cubicMultivariateReg)[[3]], digits = 3),
         d = format(coef(cubicMultivariateReg)[[1]], digits = 3)
      )
   )
quadMultivariateRegEq <-
   substitute(
      italic(z[coupling]) == a %.% italic(z[1100]) + b %.% italic(z[1100]) ^ 2 + c %.% italic(frac(Phi, 100)) + d,
      list(
         a = format(coef(quadMultivariateReg)[[2]], digits = 3),
         b = format(coef(quadMultivariateReg)[[3]], digits = 3),
         c = format(coef(quadMultivariateReg)[[4]], digits = 3),
         d = format(coef(quadMultivariateReg)[[1]], digits = 3)
      )
   )

# Draw contour plots
x <- seq(0, 150, length.out = 100)
y <- seq(0, 150, length.out = 100)
contour.grid <-
   expand.grid(tparam = x,
               z1100 = y,
               stringsAsFactors = FALSE)
linContour <- predict(linearMultivariateReg, newdata = contour.grid)
linContourData <- cbind(contour.grid, 'z.coupling' = linContour)
linContourData <- arrange(linContourData, tparam, z1100)
cubicContour <-
   predict(cubicMultivariateReg, newdata = contour.grid)
cubicContourData <- cbind(contour.grid, 'z.coupling' = cubicContour)
cubicContourData <- arrange(cubicContourData, tparam, z1100)
quadContour <-
   predict(quadMultivariateReg, newdata = contour.grid)
quadContourData <- cbind(contour.grid, 'z.coupling' = quadContour)
quadContourData <- arrange(quadContourData, tparam, z1100)

# Clean up environment
rm(x, y, cubicContour, quadContour, linContour)

# GGplot
linContourPlot <-
   ggplot(data = linContourData, aes(x = tparam, y = z1100, z = z.coupling)) +
   geom_contour(aes(color = ..level..),
                size = 1,
                binwidth = 10) +
   geom_point(data = reducedAllModelsDataframe, aes(x = tparam, y = z1100), size = 2)
linContourPlot <-
   direct.label(linContourPlot, list(fontface = 'plain', cex = 1, 'top.bumpup'))
cubicContourPlot <-
   ggplot(data = cubicContourData, aes(x = tparam, y = z1100, z = z.coupling)) +
   geom_contour(aes(color = ..level..),
                size = 1,
                binwidth = 10) +
   geom_point(data = reducedAllModelsDataframe, aes(x = tparam, y = z1100), size = 2)
cubicContourPlot <-
   direct.label(cubicContourPlot, list(fontface = 'plain', cex = 1, 'top.bumpup'))
quadContourPlot <-
   ggplot(data = quadContourData, aes(x = tparam, y = z1100, z = z.coupling)) +
   geom_contour(aes(color = ..level..),
                size = 1,
                binwidth = 10) +
   geom_point(data = reducedAllModelsDataframe, aes(x = tparam, y = z1100), size = 2)
quadContourPlot <-
   direct.label(quadContourPlot, list(fontface = 'plain', cex = 1, 'top.bumpup'))
linContourPlot
cubicContourPlot
quadContourPlot
# Close pdf
dev.off()

# Export Tukey post-hoc table and summary table for all regression models

# Read data for subduction zone segments (Wada & Wang, 2009) ----
segs <- read.delim('segment_z1100.txt')
colnames(segs)[6] <- 'z.coupling'
# Calculate predicted coupling depths for real world subduction zones from data in
# (Wada & Wang, 2009) based on our cubic multivariate regression
segs$z.coupling <- cubicMultivariateReg$coefficients[[1]] + 
   cubicMultivariateReg$coefficients[[2]]*segs$z1100^2 + 
   cubicMultivariateReg$coefficients[[3]]*segs$tparam

# Save dataset ----
rm(list = loaded.data)
rm(loaded.data)
# Set working directory to the location of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
save.image(file = 'regression_data.RData')
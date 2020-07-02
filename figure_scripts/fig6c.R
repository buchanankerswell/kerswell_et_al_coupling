# Load library
library(ggplot2)

# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('reduced_coupling_data.RData')
load('regression_data.RData')

# Open pdf for plotting
cairo_pdf(width = 5.5, height = 4.25, file = 'Fig6c.pdf')

# Calculate difference between measured and predicted coupling depths
predicted <- predict(cubicMultivariateReg)
reducedAllModelsDataframe$predicted <- predicted
reducedAllModelsDataframe$delta <- reducedAllModelsDataframe$predicted - reducedAllModelsDataframe$z.coupling

# Calculating the two-sigma standard deviation of the deltas
twosigmaall <-
  round(sd(reducedAllModelsDataframe$delta) * 2, 2)

# Draw Plot
ggplot(data = reducedAllModelsDataframe) +
  geom_histogram(
    aes(x = delta, y = ..density..),
    bins = 15,
    color = 'white',
    alpha = 0.25
  ) +
geom_density(
             aes(x = delta, y = ..density..),
             size = 0.5) +
geom_rug(
  sides = "b",
  aes(x = delta, y = 0),
  length = unit(0.05, 'npc')
) +
  annotate(
    'rect',
    xmin = mean(reducedAllModelsDataframe$delta) - twosigmaall,
    xmax = mean(reducedAllModelsDataframe$delta) + twosigmaall,
    ymin = 0.10,
    ymax = 0.09,
    alpha = 0.5
  ) +
  annotate(
    "segment",
    x = mean(reducedAllModelsDataframe$delta),
    xend = mean(reducedAllModelsDataframe$delta),
    y = 0.11,
    yend = 0.10,
    colour = "black",
    size = 1,
    arrow = arrow(angle = 15, length = unit(0.25, 'cm'))
  ) +
  annotate(
    'text',
    x = mean(reducedAllModelsDataframe$delta),
    y = 0.11,
    vjust = -0.25,
    label = 'mean',
    size = 4
  ) +
  annotate(
    'text',
    x = mean(reducedAllModelsDataframe$delta) - 4,
    y = (0.10 + 0.09) / 2,
    vjust = 0.5,
    color = 'white',
    label = bquote(2 ~ sigma == 11),
    size = 4
  ) +
  annotate(
    'text',
    x = 12.5,
    y = 0.125,
    vjust = 1,
    label = '(c)',
    size = 6
  ) +
  xlab(expression(paste(
    Delta[predicted - measured], ' Coupling Depth (km)'
  ))) +
  ylab('Probability') +
  theme_bw(base_size = 14) +
  theme(
    axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.5, 0, 0, 0), "cm")),
    axis.text.y = element_text(margin = unit(c(0, 0.5, 0, 0), "cm")),
    panel.border = element_rect(size = 2, color = 'black'),
    axis.text = element_text(face = 'plain', color = 'black'),
    axis.ticks = element_line(size = 1, color = 'black'),
    legend.position = 'none',
    axis.title = element_text(size = 12, face = 'plain')
  )

# Close pdf device
dev.off()

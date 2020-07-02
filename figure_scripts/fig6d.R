# Load library
library(ggplot2)
# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('reduced_coupling_data.RData')
load('regression_data.RData')

# Open pdf for plotting
cairo_pdf(width = 5.5, height = 4.25, file = 'Fig6d.pdf')

# Calculate two-sigma standard deviation for the predicted coupling depths of 
# SZs from Wada & Wang (2009)
twosigma <- round(sd(segs$z.coupling), 2)

# Remove Mexico and Nankai outliers
cut_outliers_segs <- segs[-c(2, 3),]

# Draw Plot
ggplot(data = segs) +
  geom_histogram(
    aes(x = z.coupling, y = ..density..),
    bins = 15,
    color = 'white',
    alpha = 0.25
  ) +
  geom_density(aes(x = z.coupling, y = ..density..), size = 0.5) +
  geom_line(
    data = cut_outliers_segs,
    stat = 'density',
    aes(x = z.coupling, y = ..density..),
    size = 0.5,
    linetype = 2,
    alpha = 0.25
  ) +
  geom_rug(
    sides = "b",
    aes(x = z.coupling, y = 0),
    colour = "black",
    length = unit(0.05, 'npc')
  ) +
  annotate(
    'rect',
    xmin = mean(segs$z.coupling) - twosigma,
    xmax = mean(segs$z.coupling) + twosigma,
    ymin = 0.0265,
    ymax = 0.03,
    alpha = 0.5
  ) +
  annotate(
    "segment",
    x = mean(segs$z.coupling),
    xend = mean(segs$z.coupling),
    y = 0.035,
    yend = 0.03,
    colour = "black",
    size = 1,
    arrow = arrow(angle = 15, length = unit(0.25, 'cm'))
  ) +
  annotate(
    "segment",
    x = median(segs$z.coupling),
    xend = median(segs$z.coupling),
    y = 0.038,
    yend = 0.033,
    colour = "black",
    size = 1,
    arrow = arrow(angle = 15, length = unit(0.25, 'cm'))
  ) +
  annotate(
    'text',
    x = mean(segs$z.coupling),
    y = 0.035,
    vjust = -0.25,
    label = 'mean',
    size = 4
  ) +
  annotate(
    'text',
    x = median(segs$z.coupling),
    y = 0.038,
    vjust = -0.25,
    label = 'median',
    size = 4
  ) +
  annotate(
    'text',
    x = 88,
    y = 0.02825,
    vjust = 0.5,
    hjust = 0,
    color = 'white',
    label = bquote(2 ~ sigma==14),
    size = 4
  ) +
  annotate(
    'text',
    x = 110,
    y = 0.05,
    vjust = 0,
    label = '(d)',
    size = 6
  ) +
  xlab(expression(paste('Coupling Depth (km)'
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

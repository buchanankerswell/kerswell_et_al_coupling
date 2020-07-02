# Load libraries
library(ggplot2)
library(viridis)
# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('reduced_coupling_data.RData')

# Open pdf for plotting
cairo_pdf(width = 5.5, height = 4.25, file = 'Fig2a.pdf')

# Add columns repeating xvec and yvec for raster plot
reducedAllModelsDataframe$xvec = rep(seq(1,4,1),16)
reducedAllModelsDataframe$yvec = rep(c(rep(1,4), rep(2,4), rep(3,4), rep(4,4)), 4)

# Draw raster plot
ggplot(data = reducedAllModelsDataframe) +
  geom_raster(
    data = subset(reducedAllModelsDataframe, z1100 == 46),
    aes(xvec, yvec, fill = tparam),
    hjust = 0,
    vjust = 0
  ) +
  annotate(
    'text',
    label = 'cda',
    x = 0.5,
    y = 1.5,
    colour = 'black',
    size = 6
  ) +
  annotate(
    'text',
    label = 'cdb',
    x = 1.5,
    y = 1.5,
    colour = 'black',
    size = 6
  ) +
  annotate(
    'text',
    label = 'cdc',
    x = 2.5,
    y = 1.5,
    colour = 'black',
    size = 6
  ) +
  annotate(
    'text',
    label = 'cdd',
    x = 3.5,
    y = 1.5,
    colour = 'black',
    size = 6
  ) +
  annotate(
    'text',
    label = 'cde',
    x = 0.5,
    y = 2.5,
    colour = 'black',
    size = 6
  ) +
  annotate(
    'text',
    label = 'cdf',
    x = 1.5,
    y = 2.5,
    colour = 'black',
    size = 6
  ) +
  annotate(
    'text',
    label = 'cdg',
    x = 2.5,
    y = 2.5,
    colour = 'black',
    size = 6
  ) +
  annotate(
    'text',
    label = 'cdh',
    x = 3.5,
    y = 2.5,
    colour = 'black',
    size = 6
  ) +
  annotate(
    'text',
    label = 'cdi',
    x = 0.5,
    y = 3.5,
    colour = 'black',
    size = 6
  ) +
  annotate(
    'text',
    label = 'cdj',
    x = 1.5,
    y = 3.5,
    colour = 'black',
    size = 6
  ) +
  annotate(
    'text',
    label = 'cdk',
    x = 2.5,
    y = 3.5,
    colour = 'black',
    size = 6
  ) +
  annotate(
    'text',
    label = 'cdl',
    x = 3.5,
    y = 3.5,
    colour = 'black',
    size = 6
  ) +
  annotate(
    'text',
    label = 'cdm',
    x = 0.5,
    y = 4.5,
    colour = 'black',
    size = 6
  ) +
  annotate(
    'text',
    label = 'cdn',
    x = 1.5,
    y = 4.5,
    colour = 'black',
    size = 6
  ) +
  annotate(
    'text',
    label = 'cdo',
    x = 2.5,
    y = 4.5,
    colour = 'black',
    size = 6
  ) +
  annotate(
    'text',
    label = 'cdp',
    x = 3.5,
    y = 4.5,
    colour = 'grey50',
    size = 6
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 4),
    breaks = seq(0.5, 3.5, 1),
    labels = c(40, 66, 80, 100)
  ) +
  scale_y_reverse(
    expand = c(0, 0),
    limits = c(5, 1),
    breaks = seq(1.5, 4.5, 1),
    labels = c(32.6, 55, 85, 110)
  ) +
  scale_fill_continuous(limits = c(55, 115), breaks = seq(55, 115, 15)) +
  scale_fill_gradient(low = 'grey90', high = 'grey10') +
  # scale_fill_viridis_c(direction = -1, option = 'viridis') +
  ggtitle('Range of Slab Thermal States') +
  xlab('Convergence Velocity (km/Ma)') +
  ylab('Age (Ma)') +
  labs(fill = bquote(frac(Phi, 100) ~ "(km/100)")) +
  theme_bw(base_size = 14) +
  theme(
    legend.box.margin = margin(c(10, 10, 10, 10)),
    panel.border = element_rect(size = 2, color = 'black'),
    axis.text = element_text(face = 'plain', color = 'black'),
    axis.ticks = element_line(size = 1, color = 'black'),
    legend.key = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = 'plain'),
    axis.title = element_text(size = 12, face = 'plain'),
    plot.title = element_text(hjust = 0.5)
  )

# Close pdf device
dev.off()

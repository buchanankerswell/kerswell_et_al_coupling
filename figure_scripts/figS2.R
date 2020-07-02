# Load library
library(ggplot2)

# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('reduced_coupling_data.RData')

# Open pdf for plotting
pdf(width = 5.5, height = 4.25, file = 'FigS2.pdf')

# Draw Plot
ggplot(data = PD15) +
  geom_point(
    size = 1,
    aes(x = pressure, y = cumulative)
  ) +
  geom_abline(
    intercept = 0.605,
    slope = 0.10,
    color = 'grey50',
    linetype = 1,
    size = 0.5
  ) +
  geom_abline(
    intercept = -0.28,
    slope = 0.45,
    color = 'grey50',
    linetype = 1,
    size = 0.5
  ) +
  annotate(
    'text',
    x = 1,
    y = 0.325,
    hjust = 0.5,
    vjust = 1,
    fontface = 1,
    size = 4,
    label = 'Slope ~ 45%/GPa',
    angle = 52
  ) +
  annotate(
    'text',
    x = 1.5,
    y = 0.75,
    hjust = 0.5,
    vjust = -0.5,
    fontface = 1,
    size = 4,
    label = 'Slope ~ 10%/GPa',
    angle = 17
  ) +
  annotate(
    'segment',
    x = 2.5,
    xend = 2.5,
    y = 0.65,
    yend = 0.78,
    colour = "black",
    size = 1,
    arrow = arrow(angle = 15, length = unit(0.25, 'cm'))
  ) +
  annotate(
    'text',
    x = 2.5,
    y = 0.65,
    hjust = 0.5,
    vjust = 1,
    fontface = 1,
    size = 4,
    label = '~80 km'
  ) +
  xlab('Pressure (GPa)') +
  ylab('Cumulative Probability') +
  xlim(c(0,4)) +
  theme_bw(base_size = 14) +
  theme(
    legend.box.margin = margin(c(10, 10, 10, 10)),
    legend.position = 'none',
    panel.border = element_rect(size = 2, color = 'black'),
    axis.text = element_text(face = 'plain', color = 'black'),
    axis.ticks = element_line(size = 1, color = 'black'),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = 'plain'),
    axis.title = element_text(size = 12, face = 'plain'),
    plot.title = element_text(hjust = 0.5)
  )

# Close pdf device
dev.off()
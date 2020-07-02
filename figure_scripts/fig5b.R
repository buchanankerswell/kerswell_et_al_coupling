# Load library
library(ggplot2)

# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('reduced_coupling_data.RData')
load('regression_data.RData')

# Open pdf for plotting
cairo_pdf(width = 5.5, height = 4.25, file = 'Fig5b.pdf')

# Create vectors for cubic curve
cubicline <- data.frame(x = seq(0, 100, length.out = 150), 
                       y = predict(cubicRegLith, list(z1100 = seq(0, 100, length.out = 150))))
ggplot(data = reducedAllModelsDataframe) +
  geom_line(
    data = cubicline,
    aes(x = x, y = y),
    color = 'black',
    alpha = 0.2,
    size = 0.5
  ) +
  geom_boxplot(
    aes(x = z1100, y = z.coupling, group = as.factor(z1100)),
    size = 0.75,
    fill = 'grey100',
    width = 3
  ) +
  annotate(
    'text',
    x = 45,
    y = 110,
    label = cubicRegLithEq,
    size = 3.5,
    hjust = 0
  ) +
  annotate(
    'text',
    x = 100,
    y = 60,
    hjust = 1,
    vjust = 0.5,
    label = '(b)',
    size = 6
  ) +
  ylab(expression(paste('Coupling Depth, ', z[C], ' (km)'))) +
  xlab(expression(paste('Lithospheric Thickness, ', z[1100], ' (km)'))) +
  scale_x_continuous(limits = c(40, 100)) +
  theme_bw(base_size = 14) +
  theme(
    panel.border = element_rect(size = 2, color = 'black'),
    axis.text = element_text(face = 'plain', color = 'black'),
    axis.ticks = element_line(size = 1, color = 'black'),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = 'plain'),
    plot.title = element_text(hjust = 0.5)
  )

# Close pdf device
dev.off()
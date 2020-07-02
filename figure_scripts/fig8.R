# Load library
library(ggplot2)

# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('reduced_coupling_data.RData')
load('regression_data.RData')

# Open pdf for plotting
pdf(width = 5.5, height = 4.25, file = 'Fig8.pdf')

# Draw Plot
ggplot(data = allModelsDataframe) +
  
  geom_path(
    data = subset(allModelsDataframe, z1100 == 46 &
                    model == 'cdf46'),
    aes(norm.dist, smoothed.hf, color = '46'),
    size = 0.5
  ) +
  geom_path(
    data = subset(allModelsDataframe, z1100 == 62 &
                    model == 'cdf62'),
    aes(norm.dist, smoothed.hf, color = '62'),
    size = 0.5
  ) +
  geom_path(
    data = subset(allModelsDataframe, z1100 == 78 &
                    model == 'cdf78'),
    aes(norm.dist, smoothed.hf, color = '78'),
    size = 0.5
  ) +
  geom_path(
    data = subset(allModelsDataframe, z1100 == 94 &
                    model == 'cdf94'),
    aes(norm.dist, smoothed.hf, color = '94'),
    size = 0.5
  ) +
  scale_x_continuous(limits = c(-0.5, 1.5),
                     breaks = seq(-0.5, 1.5, 0.5)) +
  scale_y_continuous(limits = c(30, 90)) +
  scale_color_manual(
    name = expression(atop(
      'Lith. Thick.', paste(z[1100], ' (km)'))),
    labels = c('46', '62', '78', '94'),
    values = c('grey0', 'grey20', 'grey40', 'grey60')
  ) +
  annotate("segment", x = 1, xend = 1, y = 45, yend = 55, colour = "black", size=1, arrow=arrow(angle = 15, length = unit(0.25,'cm'))) +
  annotate("segment", x = 1.5, xend = 1.5, y = 50, yend = 55, colour = "black", size=1, arrow=arrow(angle = 15, length = unit(0.25,'cm'))) +
  annotate("segment", x = 0, xend = 0, y = 55, yend = 50, colour = "black", size=1, arrow=arrow(angle = 15, length = unit(0.25,'cm'))) +
  annotate("segment", x = 1.02, xend = 1.5, y = 50, yend = 50, colour = "black", size=1) +
  annotate('text', x = 1, y = 45, label = 'Arc', size = 4, vjust = 1.25) +
  annotate('text', x = 1.25, y = 50, label = 'Backarc', size = 4, vjust = 1.25) +
  annotate('text', x = 0, y = 55, label = 'Trench', size = 4, vjust = -0.25) +
  ylab(expression(paste(
    'Heat flow (mW/', m ^ 2, ')'
  ))) +
  xlab('Normalized Distance') +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.box.margin = margin(c(10, 10, 10, 10)),
    legend.background = element_rect(
      size = 0.5,
      linetype = 'solid',
      color = 'black'
    ),
    legend.spacing.y = unit(0, "pt"),
    legend.key.width = unit(1.2, 'cm'),
    legend.key.height = unit(0.5, 'cm'),
    panel.border = element_rect(size = 2, color = 'black'),
    axis.text = element_text(face = 'plain', color = 'black'),
    axis.ticks = element_line(size = 1, color = 'black'),
    axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.5, 0, 0, 0), "cm")),
    axis.text.y = element_text(margin = unit(c(0, 0.5, 0, 0), "cm")),
    legend.text = element_text(size = 10, face = 'plain'),
    legend.title = element_text(size = 10, face = 'plain'),
    axis.title = element_text(size = 12, face = 'plain'),
    plot.title = element_text(hjust = 0.5)
  )

# Close pdf device
dev.off()
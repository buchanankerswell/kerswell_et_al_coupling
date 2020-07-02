# Load library
library(ggplot2)

# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('reduced_coupling_data.RData')
load('regression_data.RData')

# Open pdf for plotting
pdf(width = 5.5, height = 4.25, file = 'Fig7d.pdf')

# Draw Plot
ggplot(data = allModelsDataframe) +
  geom_path(
    data = subset(allModelsDataframe, z1100 == 94),
    aes(norm.dist, smoothed.hf),
    alpha = 0.1,
    size = 0.5
  ) +
  geom_line(
    data = subset(allModelsDataframe, z1100 == 94 &
                    model == 'cda94'),
    aes(norm.dist, smoothed.hf, linetype = 'Young/Slow Slab'),
    size = 0.5
  ) +
  geom_line(
    data = subset(allModelsDataframe, z1100 == 94 &
                    model == 'cdf94'),
    aes(norm.dist, smoothed.hf, linetype = 'Standard Model'),
    size = 0.5
  ) +
  geom_line(
    data = subset(allModelsDataframe, z1100 == 94 &
                    model == 'cdp94'),
    aes(norm.dist, smoothed.hf, linetype = 'Old/Fast Slab'),
    size = 0.5
  ) +
  annotate(
    'text',
    x = -0.5,
    y = 0,
    hjust = -0.1,
    vjust = -0.6,
    label = '(d)',
    size = 6
  ) +
  annotate(
    'text',
    x = 0.5,
    y = 0,
    hjust = 0,
    vjust = 0,
    label = 'Lithospheric Thickness = 94 km',
    size = 4
  ) +
  scale_x_continuous(limits = c(-0.5, 1.5),
                     breaks = seq(-0.5, 1.5, 0.5)) +
  ylab(expression(paste(
    'Heat flow (mW/', m ^ 2, ')'
  ))) +
  xlab('Normalized Distance') +
  scale_y_continuous(limits = c(0, 150)) +
  scale_linetype_manual(breaks = c('Young/Slow Slab', 'Standard Model', 'Old/Fast Slab'),
                        values = c(1, 2, 5)) +
  theme_bw(base_size = 14) +
  theme(
    # legend.position = c(0, 1),
    legend.position = 'none',
    legend.justification = c(0, 1),
    legend.box.margin = margin(c(10, 10, 10, 10)),
    legend.background = element_rect(
      size = 0.5,
      linetype = 'solid',
      color = 'black'
    ),
    legend.spacing.y = unit(0, "pt"),
    legend.key.width = unit(1.2, 'cm'),
    panel.border = element_rect(size = 2, color = 'black'),
    axis.text = element_text(face = 'plain', color = 'black'),
    axis.ticks = element_line(size = 1, color = 'black'),
    axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.5, 0, 0, 0), "cm")),
    axis.text.y = element_text(margin = unit(c(0, 0.5, 0, 0), "cm")),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    axis.title = element_text(size = 12, face = 'plain'),
    plot.title = element_text(hjust = 0.5)
  )

# Close pdf device
dev.off()
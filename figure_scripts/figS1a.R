# Load library
library(ggplot2)

# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('reduced_coupling_data.RData')
load('regression_data.RData')

# Open pdf for plotting
cairo_pdf(width = 5.5, height = 4.25, file = 'FigS1a.pdf')

# Draw Plot
ggplot() +
  geom_point(
    data = subset(antstab, antstab$model == 'cda60'),
    aes(x = time, y = depth, color = model),
    shape = 15,
    size = 1
  ) +
  geom_smooth(
    data = subset(antstab, antstab$model == 'cda60'),
    aes(x = time, y = depth, color = model),
    se = FALSE,
    size = 0.5
  ) +
  geom_point(
    data = subset(antstab, antstab$model == 'cdf60'),
    aes(x = time, y = depth, color = model),
    shape = 16,
    size = 1
  ) +
  geom_smooth(
    data = subset(antstab, antstab$model == 'cdf60'),
    aes(x = time, y = depth, color = model),
    se = FALSE,
    size = 0.5
  ) +
  geom_point(
    data = subset(antstab, antstab$model == 'cdp60'),
    aes(x = time, y = depth, color = model),
    shape = 17,
    size = 1
  ) +
  geom_smooth(
    data = subset(antstab, antstab$model == 'cdp60'),
    aes(x = time, y = depth, color = model),
    se = FALSE,
    size = 0.5
  ) +
  annotate(
    'text',
    x = 0,
    y = 112.5,
    hjust = 0,
    vjust = 0.5,
    label = '(a)',
    size = 6
  ) +
  scale_color_manual(
    values = c('grey0', 'grey30', 'grey60'),
    labels = c('cda46', 'cdf46', 'cdp46'),
    guide = guide_legend(override.aes = list(
      linetype = rep('blank', 3),
      shape = c(15, 16, 17)
    ))
  ) +
  scale_y_reverse(limits = c(120, 25)) +
  scale_x_continuous(limits = c(0, 15)) +
  xlab('Time (Ma)') +
  ylab('Depth (km)') +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.box.margin = margin(c(10, 10, 10, 10)),
    legend.background = element_rect(
      size = 0.5,
      linetype = 'solid',
      color = 'black'
    ),
    legend.spacing.y = unit(0, "pt"),
    panel.border = element_rect(size = 2, color = 'black'),
    axis.text = element_text(face = 'plain', color = 'black'),
    axis.ticks = element_line(size = 1, color = 'black'),
    axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.5, 0, 0, 0), "cm")),
    axis.text.y = element_text(margin = unit(c(0, 0.5, 0, 0), "cm")),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = 'plain'),
    axis.title = element_text(size = 12, face = 'plain'),
    plot.title = element_text(hjust = 0.5)
  )

# Close pdf device
dev.off()
# Load library
library(ggplot2)

# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('reduced_coupling_data.RData')
load('regression_data.RData')

# Open pdf for plotting
cairo_pdf(width = 5.5, height = 4.25, file = 'Fig5a.pdf')

rsquared <- round(signif(summary(regModelAll)$adj.r.squared), 2)

# Draw Plot
ggplot(data = reducedAllModelsDataframe) +
  geom_point(
    size = 1,
    aes(x = tparam, y = z.coupling)
  ) +
  geom_abline(
    intercept = regModelAll$coefficients[[1]],
    slope = regModelAll$coefficients[[2]],
    color = 'grey0',
    linetype = 1,
    size = 0.5
  ) +
  annotate(
    'text',
    x = 110,
    y = 50,
    hjust = 1,
    vjust = 0,
    fontface = 1,
    size = 3.5,
    label = deparse(bquote(R ^ 2 == .(rsquared))),
    parse = TRUE
  ) +
  annotate(
    'text',
    x = 22,
    y = 47.5,
    hjust = 0,
    vjust = 0,
    fontface = 1,
    size = 3.5,
    label = regModelAllEq
  ) +
  annotate(
    'text',
    x = 15,
    y = 46,
    hjust = 0.5,
    vjust = 0,
    label = '(a)',
    size = 6
  ) +
  xlab(expression(
    paste('Slab Thermal Parameter ', Phi, '/100 (km/100)')
  )) +
  ylab(expression(paste(
    'Coupling Depth ', z[C], ' (km)'
  ))) +
  scale_color_manual(
    breaks = c('46', '62', '78', '94'),
    values = c('grey0', 'grey20', 'grey40', 'grey46'),
    guide = guide_legend(override.aes = list(
      linetype = rep('blank', 4),
      shape = c(15, 16, 17, 18)
    ))
  ) +
  labs(color = expression(atop(
    'Lith. Thick.', paste(z[178], ' (km)')
  ))) +
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

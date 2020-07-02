# Load library
library(ggplot2)
library(directlabels)
library(ggrepel)

# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('reduced_coupling_data.RData')
load('regression_data.Rdata')

# Open pdf for plotting
cairo_pdf(width = 5.5, height = 4.25, file = 'Fig6b.pdf')

p2 <- ggplot(data = cubicContourData, aes(x = tparam, y = z1100, z = z.coupling)) +
  geom_contour(aes(color = ..level..),
               size = 0.5,
               binwidth = 10) +
  annotate(
    'text',
    x = 0,
    y = 0,
    hjust = 0,
    vjust = -0.5,
    label = '(b)',
    size = 6
  ) +
  xlab(expression(paste('Slab Thermal Parameter ', Phi, '/100 (km/100)'))) +
  ylab(expression(paste('Lithospheric Thickness, ', z[1100], ' (km)'))) +
  scale_color_gradient(low = 'grey', high = 'black') +
  scale_x_continuous(limits = c(0,125)) +
  scale_y_continuous(limits = c(0,100)) +
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
dl.config <- list('calc.boxes', box.color = 'white', 'draw.rects')
p2 <-
  direct.label(p2, list(fontface = 'plain', cex = 0.8, 'top.pieces', vjust=0.5, 'dl.config'))
pcouple <- list(
  geom_point(
    data = segs,
    aes(x = tparam, y = z1100),
    size = 1
  ),
  geom_text_repel(data = segs,
                  aes(
                    x = tparam,
                    y = z1100,
                    label = segment
                  ),
                  nudge_x = 2,
                  nudge_y = 2,
                  # direction = "x",
                  # angle = 90,
                  vjust = 0,
                  segment.size = 0.1,
                  # ylim = c(0,25),
                  # xlim = c(0, 125),
                  segment.alpha = 0.3,
                  label.size = 0.5,
                  size = 3,
                  alpha = 0.8
                  # arrow = arrow(type = 'open', 
                  #               length = unit(0.08, 'inches'), 
                  #               angle = 20)
                  )
)
p2+pcouple

# Close pdf device
dev.off()

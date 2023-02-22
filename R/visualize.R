#!/usr/bin/env Rscript

# Capture output
sink(file = paste0('data/log-', Sys.Date()), append = T, type = 'output', split = T)

# Load functions and libraries
cat(rep('~', 80), sep='')
cat('\nLoading packages and functions ...')
source('R/functions.R')
cat('\nLoading numerical results ...')
load('data/numerical_results/cdf46.RData')
load('data/numerical_results/cdf62.RData')
load('data/numerical_results/cdf94.RData')
load('data/numerical_results/cdf78_50.RData')
load('data/numerical_results/cdf78_130.RData')
load('data/numerical_results/cdf78_250.RData')
load('data/regressions.RData')
load('data/preprocessed.RData')

# Figure 1
# Draw figure 1
cat('\nDrawing figure 1 ...')
p1.grids <-
  list(
  rocks =
    draw_grid(
      nodes = n78.init$nodes,
      rocks = r78.init$grid,
      time = round(r78.init$time/1e6, 2),
      p.type = 'rocks',
      leg.pos = 'bottom',
      iso.size = 3
    ) +
    guides(
      fill =
        guide_legend(
          nrow = 2,
          label.vjust = 0,
          title.position = 'top',
          label.position = 'right',
          title.vjust = 0,
        )
    ) +
    annotate(
      'text',
      x = 1800,
      y = 0,
      label = 'free surface',
      vjust = 1,
      color = 'black',
      size = 5
    ) +
    annotate(
      'text',
      x = 1200,
      y = Inf,
      label = 'open boundary',
      vjust = -0.2,
      color = 'white',
      size = 5
    ) +
    annotate(
      'text',
      x = -Inf,
      y = Inf,
      label = 'free slip',
      color = 'white',
      size = 5,
      angle = 90,
      vjust = 1.2,
      hjust = -0.1
    ) +
    annotate(
      'text',
      x = Inf,
      y = Inf,
      label = 'free slip',
      color = 'white',
      size = 5,
      angle = 90,
      vjust = -0.2,
      hjust = -0.1
    ),
  temperature =
    draw_grid(
      nodes = n78.init$nodes,
      rocks = r78.init$grid,
      time = round(r78.init$time/1e6, 2),
      p.type = 'temperature',
      v.pal = 'magma',
      iso.size = 3
    ) +
    guides(
      fill = 
        guide_colorbar(
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barwidth = unit(2.2, 'in')
        )
    ) +
    annotate(
      'text',
      x = -Inf,
      y = Inf,
      label = 'insulated',
      size = 5,
      angle = 90,
      vjust = 1.2,
      hjust = -0.1
    ) +
    annotate(
      'text',
      x = Inf,
      y = Inf,
      label = 'insulated',
      size = 5,
      angle = 90,
      vjust = -0.2,
      hjust = -0.1
    ) +
    annotate('text', x = 800, y = 0, label = 'constant 0 ˚C', color = 'white', size = 5) +
    annotate('text', x = 800, y = 150, label = '0.5 ˚C/km adiabat', size = 5) +
    annotate('text', x = 1800, y = 78, label = 'upper plate thickness', hjust = 1, size = 5) +
    annotate(
      'segment',
      x = 1800,
      xend = 1950,
      y = 78,
      yend = 78,
      linetype = 'twodash',
      lineend = 'round',
      linejoin = 'round'
    ) +
    annotate(
      'curve',
      x = 150,
      xend = 50,
      y = 150,
      yend = 70,
      curvature = -0.2,
      arrow = arrow(length = unit(0.1, 'in'), angle = 20)
    ) +
    annotate('text', x = 155, y = 150, label = 'mid-ocean ridge', hjust = 0, size = 5),
  viscosity =
    draw_grid(
      nodes = n78.init$nodes,
      rocks = r78.init$grid,
      time = round(r78.init$time/1e6, 2),
      p.type = 'viscosity',
      v.pal = 'viridis',
      iso.size = 3
    ) +
    guides(
      fill = 
        guide_colorbar(
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barwidth = unit(2.2, 'in')
        )
    ) +
    annotate(
      'rect',
      xmin = 490,
      xmax = 510,
      ymin = -4,
      ymax = 40,
      fill = NA,
      color = 'black'
    ) +
    annotate(
      'rect',
      xmin = 1790,
      xmax = 1810,
      ymin = -4,
      ymax = 40,
      fill = NA,
      color = 'black'
    ) +
    annotate(
      'segment',
      x = 510,
      xend = 560,
      y = 20,
      yend = 20,
      arrow = arrow(length = unit(0.1, 'in'), angle = 20),
      color = 'black',
      lineend = 'round',
      linejoin = 'round'
    ) +
    annotate(
      'segment',
      xend = 1740,
      x = 1790,
      y = 20,
      yend = 20,
      arrow = arrow(length = unit(0.1, 'in'), angle = 20),
      color = 'black',
      lineend = 'round',
      linejoin = 'round'
    ) +
    annotate('text', x = 400, y = 150, label = 'convergence region', hjust = 1, size = 5) +
    annotate(
      'curve',
      x = 405,
      xend = 500,
      y = 150,
      yend = 40,
      curvature = 0.5,
      arrow = arrow(length = unit(0.1, 'in'), angle = 20),
      lineend = 'round'
    ) +
    annotate('text', x = 1700, y = 150, label = 'weak zone', hjust = 0, size = 5) +
    annotate(
      'curve',
      x = 1695,
      xend = 1490,
      y = 150,
      yend = 75,
      curvature = -0.2,
      arrow = arrow(length = unit(0.1, 'in'), angle = 20),
      lineend = 'round'
    )
  )
# Plot figure 1
cat('\nSaving figure 1 to figs/fig1.png ...')
p1 <-
  (p1.grids[[1]] + theme(axis.text.x = element_blank(), axis.title.x = element_blank())) /
  (p1.grids[[2]] + theme(axis.text.x = element_blank(), axis.title.x = element_blank())) /
  p1.grids[[3]] +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'a') &
  theme(
    legend.position = 'bottom',
    plot.margin = margin(2, 8, 2, 2),
    legend.margin = margin(),
    legend.box.margin = margin(),
    plot.title = element_blank()
  )
suppressWarnings(
  ggsave(
    'figs/fig1.png',
    plot = p1,
    device = 'png',
    type = 'cairo',
    width = 11,
    height = 6.5
  )
)

# Figure 2
# Draw figure 2
cat('\nDrawing figure 2 ...')
# 2a
p2.a <-
  expand.grid(1:4, 1:4) %>% 
  tibble() %>% 
  mutate(
    thermal.parameter = mods$thermal.parameter[mods$upper.plate.thickness == 46],
    model = mods$model[mods$upper.plate.thickness == 46] %>% stringr::str_extract('cd.')
  ) %>% 
  rename(age = Var2, cv = Var1) %>% 
  ggplot() +
  geom_raster(aes(x = cv, y = age, fill = thermal.parameter), hjust = 0, vjust = 0) +
  geom_label(aes(x = cv-0.5, y = age+0.5, label = model), alpha = 0.6) +
  labs(
    x = 'convergence velocity (km/Ma)',
    y = 'age (Ma)',
    fill = 'thermal parameter km/100',
    title = 'slab thermal states'
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
  scale_fill_gradient(
    low = 'grey90',
    high = 'grey10',
    guide = 
      guide_colorbar(
        inside = T,
        title.position = 'top',
        title.vjust = -1,
        barwidth = unit(2.5, 'in')
      )
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(color = 'black'),
    axis.ticks = element_line(color = 'black'),
    axis.line = element_blank(),
    legend.position = 'bottom'
  )
# 2b
p2.b <-
  geotherms %>% 
  pivot_longer(-depth, names_to = 'upper.plate.thickness', values_to = 'T') %>% 
  mutate(
    'upper.plate.thickness' =
      as.factor(stringr::str_extract(upper.plate.thickness, '[0-9]+'))
  ) %>% 
  ggplot() +
  geom_line(aes(x = T, y = depth, color = upper.plate.thickness), size = 0.8) +
  scale_color_manual(
    values = c('white', 'grey80', 'grey20', 'black'),
    guide = 
      guide_legend(
        label.vjust = 0,
        title.position = 'top',
        label.position = 'bottom',
        title.vjust = 0
      )
  ) +
  labs(
    x = 'temperature (˚C)',
    y = 'depth (km)',
    title = 'model geotherms',
    color = 'upper-plate thickness km'
  ) +
  scale_y_reverse(limits=c(120,0)) +
  scale_x_continuous(limits = c(0,1300)) +
  theme_dark(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(color = 'black'),
    axis.ticks = element_line(color = 'black'),
    legend.position = 'bottom',
    legend.key.width = unit(0.55, 'in')
  )
# Composition
p2 <-
  p2.a + p2.b +
  plot_annotation(
    tag_levels = 'a',
    theme = theme(plot.margin = margin())
  ) +
  plot_layout(widths = 1) &
  theme(
    plot.margin = margin(2, 2, 2, 2),
    legend.margin = margin(),
    legend.box.margin = margin()
  )
# Plot figure 2
cat('\nSaving figure 2 to figs/fig2.png ...')
suppressWarnings(
  ggsave(
    filename = 'figs/fig2.png',
    device = 'png',
    type = 'cairo',
    width = 8,
    height = 5
  )
)

# Figure 3
cat('\nDrawing figure 3 ...')
p3.grids <-
  list(
    rocks.zoom = draw_grid(
      nodes = n78_250$nodes,
      rocks = r78_250$grid,
      time = round(r78_250$time/1e6, 2),
      p.type = 'rocks',
      leg.pos = 'bottom',
      box = c(-18, 180, 900, 1600)
    ),
    viscosity = draw_grid(
      nodes = n78_250$nodes,
      rocks = r78_250$grid,
      time = round(r78_250$time/1e6, 2),
      p.type = 'viscosity',
      v.pal = 'viridis',
      box = c(-18, 180, 900, 1600)
    ),
    strain = draw_grid(
      nodes = n78_250$nodes,
      rocks = r78_250$grid,
      time = round(r78_250$time/1e6, 2),
      p.type = 'strain',
      v.pal = 'viridis',
      box = c(-18, 180, 900, 1600)
    ),
    stream = draw_grid(
      nodes = n78_250$nodes,
      rocks = r78_250$grid,
      time = round(r78_250$time/1e6, 2),
      p.type = 'stream',
      v.pal = 'viridis',
      box = c(-18, 180, 900, 1600)
    )
)
# Plot  composition
cat('\nSaving figure 3 to figs/fig3.png ...')
p3 <- 
  (
    p3.grids$rocks.zoom +
    guides(fill = 'none') +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  ) /
  (
    p3.grids$viscosity +
    guides(
      fill = 
        guide_colorbar(
          title = bquote(log10~Pa %.% s),
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  ) /
  (
    p3.grids$strain +
    guides(
      fill = 
        guide_colorbar(
          title = bquote(log10~s^-1),
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  ) /
  (
    p3.grids$stream +
    guides(
      color = 
        guide_colorbar(
          title = bquote(cm~yr ^ -1),
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme()
  ) +
  plot_layout(guides = 'keep') +
  plot_annotation(
    tag_levels = 'a',
    theme = theme(plot.margin = margin())
  ) &
  theme(
    plot.margin = margin(2, 2, 2, 2),
    legend.margin = margin(),
    legend.box.margin = margin(),
    legend.justification = 'left',
    legend.box.just = 'left',
    plot.title = element_blank()
  )
suppressWarnings(
  ggsave(
    'figs/fig3.png',
    plot = p3,
    device = 'png',
    type = 'cairo',
    width = 8.5,
    height = 8.5
  )
)

# Figure 4
cat('\nDrawing figure 4 ...')
grid <-
  expand.grid(
    upper.plate.thickness = seq(0, 120, length.out = 100),
    thermal.parameter = seq(0, 120, length.out = 100),
    stringsAsFactors = F
  ) %>% 
  tibble()
p4.a <-
  grid %>% 
  mutate(
    coupling.depth =
      predict(
        lm(coupling.depth ~ upper.plate.thickness + thermal.parameter, mods),
        newdata = grid
      )
  ) %>% 
  ggplot() +
  geom_contour_fill(
    aes(x = thermal.parameter, y = upper.plate.thickness, z = coupling.depth),
    size = 0.3,
    color = 'black',
    show.legend = F,
    alpha = 0.75
  ) +
  geom_text_contour(
    aes(x = thermal.parameter, y = upper.plate.thickness, z = coupling.depth),
    size = 5,
    label.placer = label_placer_fraction(frac = 0.05)
  ) +
  geom_point(
    data = mods,
    aes(x = thermal.parameter, y = upper.plate.thickness),
    shape = 15,
    size = 2
  ) +
  geom_point(
    data = segs,
    aes(x = thermal.parameter, y = upper.plate.thickness),
    color = 'grey90',
    size = 2
  ) +
  coord_cartesian(expand = F) +
  labs(x = 'thermal parameter (km/100)', y = 'upper-plate thickness (km)') +
  scale_fill_gradient(low = 'grey20', high = 'grey80') +
  theme_dark(base_size = 16) +
  theme(axis.text = element_text(color = 'black'), panel.grid = element_blank())
p4.b <-
  grid %>% 
  mutate(
    coupling.depth =
      predict(
        lm(coupling.depth ~ I(upper.plate.thickness^2) + thermal.parameter, mods),
        newdata = grid
      )
  ) %>% 
  ggplot() +
  geom_contour_fill(
    aes(x = thermal.parameter, y = upper.plate.thickness, z = coupling.depth),
    size = 0.3,
    color = 'black',
    show.legend = F,
    alpha = 0.75
  ) +
  geom_text_contour(
    aes(x = thermal.parameter, y = upper.plate.thickness, z = coupling.depth),
    size = 5,
    label.placer = label_placer_fraction(frac = 0.05)
  ) +
  geom_point(
    data = mods,
    aes(x = thermal.parameter, y = upper.plate.thickness),
    shape = 15,
    size = 2
  ) +
  geom_point(
    data = segs,
    aes(x = thermal.parameter, y = upper.plate.thickness),
    color = 'grey90',
    size = 2
  ) +
  coord_cartesian(expand = F) +
  labs(x = 'thermal parameter (km/100)', y = 'upper-plate thickness (km)') +
  scale_fill_gradient(low = 'grey20', high = 'grey80') +
  theme_dark(base_size = 16) +
  theme(axis.text = element_text(color = 'black'), panel.grid = element_blank())
p4.c <-
  grid %>% 
  mutate(
    coupling.depth =
      predict(
        lm(coupling.depth ~ poly(upper.plate.thickness, 2) + thermal.parameter, mods),
        newdata = grid
      )
  ) %>% 
  ggplot() +
  geom_contour_fill(
    aes(x = thermal.parameter, y = upper.plate.thickness, z = coupling.depth),
    size = 0.3,
    color = 'black',
    show.legend = F,
    alpha = 0.75
  ) +
  geom_text_contour(
    aes(x = thermal.parameter, y = upper.plate.thickness, z = coupling.depth),
    size = 5,
    label.placer = label_placer_fraction(frac = 0.05)
  ) +
  geom_point(
    data = mods,
    aes(x = thermal.parameter, y = upper.plate.thickness),
    shape = 15,
    size = 2
  ) +
  geom_point(
    data = segs,
    aes(x = thermal.parameter, y = upper.plate.thickness),
    color = 'grey90',
    size = 2
  ) +
  coord_cartesian(expand = F) +
  labs(x = 'thermal parameter (km/100)', y = 'upper-plate thickness (km)') +
  scale_fill_gradient(low = 'grey20', high = 'grey80') +
  theme_dark(base_size = 16) +
  theme(axis.text = element_text(color = 'black'), panel.grid = element_blank())
cp <-
  segs %>% 
  select(coupling.depth.linear, coupling.depth.quadratic1, coupling.depth.quadratic2) %>% 
  rename(
    'linear (a)' = coupling.depth.linear,
    'quadratic (b)' = coupling.depth.quadratic1,
    'quadratic (c)' = coupling.depth.quadratic2
  ) %>% 
  pivot_longer(everything(), names_to = 'mod') %>% 
  group_by(mod)
p4.d <-
  cp %>% 
  ggplot() +
  geom_histogram(aes(x = value, group = mod), bins = 7, color = 'grey10', fill = 'grey10') +
  labs(x = 'coupling depth km', y = 'frequency') +
  theme_dark(base_size = 16) +
  theme(
    axis.text = element_text(color = 'black'),
    strip.text = element_text(size = 12),
    panel.grid = element_blank()
  ) +
  facet_wrap(~mod, ncol = 1)
# Composition
cat('\nSaving figure 4 to figs/fig4.png ...')
p4 <-
  p4.a + p4.b + p4.c + p4.d +
  plot_layout(nrow = 2, ncol = 2) +
  plot_annotation(tag_levels = 'a', theme = theme(plot.margin = margin())) &
  theme(plot.margin = margin(2, 10, 2, 2))
suppressWarnings(
  ggsave(
    filename = 'figs/fig4.png',
    plot = p4,
    device = 'png',
    type = 'cairo',
    width = 8,
    height = 8
  )
)

# Figure 5
cat('\nDrawing figure 5 ...')
p5 <-
  hf %>% 
  left_join(mods, by = 'model') %>% 
  filter(model %in% c('cdf46', 'cdf62', 'cdf78', 'cdf94')) %>%
  ggplot() +
  geom_path(
    aes(
      x = x.position.normalized,
      y = surface.heat.flow.smooth,
      color = factor(upper.plate.thickness)
    ),
    size = 1
  ) +
  annotate(
    "segment",
    x = 1,
    xend = 1,
    y = 20,
    yend = 55,
    colour = "black",
    size = 1,
    arrow = arrow(length = unit(0.1,'in'), angle = 20)
  ) +
  annotate(
    "segment",
    x = 0,
    xend = 0,
    y = 75,
    yend = 55,
    colour = "black",
    size = 1,
    arrow = arrow(length = unit(0.1,'in'), angle = 20)
  ) +
  annotate(
    "segment",
    x = 1.02,
    xend = 1.5,
    y = 40,
    yend = 40,
    colour = "black",
    size = 1,
    arrow = arrow(length = unit(0.1,'in'), angle = 20)
  ) +
  annotate('text', x = 1, y = 20, label = 'Arc', size = 5, vjust = 1.25) +
  annotate('text', x = 1.3, y = 38, label = 'Backarc', size = 5, vjust = 1.25) +
  annotate('text', x = 0, y = 75, label = 'Trench', size = 5, vjust = -0.25) +
  labs(
    x = 'normalized distance',
    y = bquote('surface heat flow'~(mWm^-2)),
    color = 'upper-plate thickness km'
  ) +
  coord_cartesian(xlim = c(-0.75, 1.5), ylim = c(0, 100)) +
  scale_color_viridis_d(option = 'magma') +
  guides(
    color =
      guide_legend(
        label.vjust = 0,
        title.position = 'top',
        label.position = 'bottom',
        title.vjust = 0
      )
  ) +
  theme_dark(base_size = 16) +
  theme(
    axis.text = element_text(color = 'black'),
    legend.position = 'bottom',
    legend.justification = 'top',
    legend.box.just = 'top',
    legend.margin = margin(),
    legend.box.margin = margin(),
    panel.grid = element_blank(),
    legend.key.width = unit(0.55, 'in')
  )
cat('\nSaving figure 5 to figs/fig5.png ...')
suppressWarnings(
  ggsave(
    filename = 'figs/fig5.png',
    plot = p5,
    device = 'png',
    type = 'cairo',
    width = 5,
    height = 5
  )
)

# Figure 6
cat('\nDrawing figure 6 ...')
p6.grids <-
  list(
    temperature =
      draw_grid(
        nodes = n78_250$nodes,
        rocks = r78_250$grid,
        time = round(r78_250$time/1e6, 2),
        p.type = 'temperature',
        v.pal = 'magma',
        arrows = T,
        box = c(30, 180, 1300, 1600)
      ) +
      guides(
        fill = 
          guide_colorbar(
            inside = T,
            title.position = 'top',
            title.vjust = -1,
            barwidth = unit(2.2, 'in')
          )
      ),
    viscosity =
      draw_grid(
        nodes = n78_250$nodes,
        rocks = r78_250$grid,
        time = round(r78_250$time/1e6, 2),
        p.type = 'viscosity',
        v.pal = 'viridis',
        arrows = T,
        box = c(30, 180, 1300, 1600)
      ) +
      guides(
        fill = 
          guide_colorbar(
            inside = T,
            title.position = 'top',
            title.vjust = -1,
            barwidth = unit(2.2, 'in')
          )
      ) +
      annotate('label', x = 1320, y = 70, label = 'a', size = 5) +
      annotate('label', x = 1375, y = 105, label = 'b', size = 5) +
      annotate('label', x = 1355, y = 80, label = 'c', size = 5) +
      annotate('label', x = 1345, y = 83, label = 'd', size = 5) +
      annotate('label', x = 1380, y = 85, label = 'e', size = 5)
)
p6 <-
  (p6.grids[[1]] + theme(axis.text.x = element_blank(), axis.title.x = element_blank())) /
  p6.grids[[2]] +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'a') &
  theme(
    legend.position = 'bottom',
    plot.margin = margin(2, 8, 2, 2),
    legend.margin = margin(),
    legend.box.margin = margin(),
    plot.title = element_blank()
  )
cat('\nSaving figure 6 to figs/fig6.png ...')
suppressWarnings(
  ggsave(
    filename = 'figs/fig6.png',
    plot = p6,
    device = 'png',
    type = 'cairo',
    width = 7,
    height = 8
  )
)

# Figure 7
cat('\nDrawing figure 7 ...')
p7.grids <-
  list(
    stream = draw_grid(
      nodes = n46$nodes,
      rocks = r46$grid,
      time = round(r46$time/1e6, 2),
      p.type = 'stream',
      v.pal = 'viridis',
      box = c(-18, 180, 900, 1600)
    ),
    stream =
      draw_grid(
        nodes = n62$nodes,
        rocks = r62$grid,
        time = round(r62$time/1e6, 2),
        p.type = 'stream',
        v.pal = 'viridis',
        box = c(-18, 180, 900, 1600)
      ),
    stream =
      draw_grid(
        nodes = n78_250$nodes,
        rocks = r78_250$grid,
        time = round(r78_250$time/1e6, 2),
        p.type = 'stream',
        v.pal = 'viridis',
        box = c(-18, 180, 900, 1600)
      ),
    stream =
      draw_grid(
        nodes = n94$nodes,
        rocks = r94$grid,
        time = round(r94$time/1e6, 2),
        p.type = 'stream',
        v.pal = 'viridis',
        box = c(-18, 180, 900, 1600)
      )
)
v.color.pal <- scale_color_viridis_c(option = 'viridis', limits = c(0, 10))
p7 <-
  (
    p7.grids[[1]] +
    guides(
      color = 
        guide_colorbar(
          title = 'cm/yr',
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  ) /
  (
    p7.grids[[2]] +
    guides(
      color = 
        guide_colorbar(
          title = 'cm/yr',
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  ) /
  (
    p7.grids[[3]] +
    guides(
      color = 
        guide_colorbar(
          title = 'cm/yr',
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  ) /
  (
    p7.grids[[4]] +
    guides(
      color = 
        guide_colorbar(
          title = 'cm/yr',
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme()
  ) +
  plot_layout(guides = 'keep') +
  plot_annotation(
    tag_levels = 'a',
    theme = theme(plot.margin = margin())
  ) &
  theme(
    plot.margin = margin(2, 2, 2, 2),
    legend.margin = margin(),
    legend.box.margin = margin(),
    legend.justification = 'top',
    legend.box.just = 'top',
    plot.title = element_blank()
  )
cat('\nSaving figure 7 to figs/fig7.png ...')
suppressWarnings(
  ggsave(
    filename = 'figs/fig7.png',
    plot = p7,
    device = 'png',
    type = 'cairo',
    width = 8.5,
    height = 8.5
  )
)

# Figure A1
cat('\nDrawing figure A1 ...')
pA1 <-
  antigorite.stability %>% 
  group_by(model) %>% 
  ggplot() +
  geom_point(aes(x = time, y = depth, group = model), size = 0.25) +
  geom_path(aes(x = time, y = depth, group = model)) +
  labs(x = 'time (Ma)', y = 'depth (km)') +
  scale_y_reverse() +
  theme_dark(base_size = 16) +
  theme(
    plot.margin = margin(2, 2, 2, 2),
    panel.grid = element_blank(),
    axis.text = element_text(color = 'black')
  ) +
  facet_grid(
    vars(model),
    vars(upper.plate.thickness),
    labeller =
      labeller(
        upper.plate.thickness = c('46' = '46km', '62' = '62km', '78' = '78km', '94' = '94km')
      )
  )
cat('\nSaving figure A1 to figs/figA1.png ...')
suppressWarnings(
  ggsave(
    filename = 'figs/figA1.png',
    plot = pA1,
    device = 'png',
    type = 'cairo',
    width = 8,
    height = 6.5
  )
)

# Figure A2
cat('\nDrawing figure A2 ...')
pA2.grids <-
  list(
    rocks.zoom =
      draw_grid(
        nodes = n78_50$nodes,
        rocks = r78_50$grid,
        time = round(r78_50$time/1e6, 2),
        p.type = 'rocks',
        leg.pos = 'bottom',
        box = c(-18, 180, 1050, 1750)
      ),
    viscosity =
      draw_grid(
        nodes = n78_50$nodes,
        time = round(r78_50$time/1e6, 2),
        p.type = 'viscosity',
        v.pal = 'viridis',
        box = c(-18, 180, 1050, 1750)
      ),
    strain =
      draw_grid(
        nodes = n78_50$nodes,
        time = round(r78_50$time/1e6, 2),
        p.type = 'strain',
        v.pal = 'viridis',
        box = c(-18, 180, 1050, 1750)
      ),
    stream =
      draw_grid(
        nodes = n78_50$nodes,
        time = round(r78_50$time/1e6, 2),
        p.type = 'stream',
        v.pal = 'viridis',
        box = c(-18, 180, 1050, 1750)
      )
)
pA2 <-
  (
    pA2.grids$rocks.zoom +
    guides(fill = 'none') +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  ) /
  (
    pA2.grids$viscosity +
    guides(
      fill = 
        guide_colorbar(
          title = bquote(log10~Pa %.% s),
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  ) /
  (
    pA2.grids$strain +
    guides(
      fill = 
        guide_colorbar(
          title = bquote(log10~s^-1),
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  ) /
  (
    pA2.grids$stream +
    guides(
      color = 
        guide_colorbar(
          title = bquote(cm~yr ^ -1),
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme()
  ) +
  plot_layout(guides = 'keep') +
  plot_annotation(
    tag_levels = 'a',
    theme = theme(plot.margin = margin())
  ) &
  theme(
    plot.margin = margin(2, 2, 2, 2),
    legend.margin = margin(),
    legend.box.margin = margin(),
    legend.justification = 'top',
    legend.box.just = 'top',
    plot.title = element_blank()
  )
cat('\nSaving figure A2 to figs/figA2.png ...')
suppressWarnings(
  ggsave(
    filename = 'figs/figA2.png',
    plot = pA2,
    device = 'png',
    type = 'cairo',
    width = 8.5,
    height = 8.5
  )
)

# Figure A3
cat('\nDrawing figure A3 ...')
pA3.grids <-
  list(
    rocks.zoom =
      draw_grid(
        nodes = n78_130$nodes,
        rocks = r78_130$grid,
        time = round(r78_130$time/1e6, 2),
        p.type = 'rocks',
        leg.pos = 'bottom',
        box = c(-18, 180, 1050, 1750)
      ),
    viscosity =
      draw_grid(
        nodes = n78_130$nodes,
        time = round(r78_130$time/1e6, 2),
        p.type = 'viscosity',
        v.pal = 'viridis',
        box = c(-18, 180, 1050, 1750)
      ),
    strain =
      draw_grid(
        nodes = n78_130$nodes,
        time = round(r78_130$time/1e6, 2),
        p.type = 'strain',
        v.pal = 'viridis',
        box = c(-18, 180, 1050, 1750)
      ),
    stream =
      draw_grid(
        nodes = n78_130$nodes,
        time = round(r78_130$time/1e6, 2),
        p.type = 'stream',
        v.pal = 'viridis',
        box = c(-18, 180, 1050, 1750)
    )
)
pA3 <-
  (
    pA3.grids$rocks.zoom +
    guides(fill = 'none') +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  ) /
  (
    pA3.grids$viscosity +
    guides(
      fill = 
        guide_colorbar(
          title = bquote(log10~Pa %.% s),
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  ) /
  (
    pA3.grids$strain +
    guides(
      fill = 
        guide_colorbar(
          title = bquote(log10~s^-1),
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  ) /
  (
    pA3.grids$stream +
    guides(
      color = 
        guide_colorbar(
          title = bquote(cm~yr ^ -1),
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme()
  ) +
  plot_layout(guides = 'keep') +
  plot_annotation(
    tag_levels = 'a',
    theme = theme(plot.margin = margin())
  ) &
  theme(
    plot.margin = margin(2, 2, 2, 2),
    legend.margin = margin(),
    legend.box.margin = margin(),
    legend.justification = 'top',
    legend.box.just = 'top',
    plot.title = element_blank()
  )
cat('\nSaving figure A3 to figs/figA3.png ...')
suppressWarnings(
  ggsave(
    filename = 'figs/figA3.png',
    plot = pA3,
    device = 'png',
    type = 'cairo',
    width = 8.5,
    height = 8.5
  )
)

# Figure A4
cat('\nDrawing figure A4 ...')
pA4.grids <- list(
  rocks.zoom =
    draw_grid(
      nodes = n78_250$nodes,
      rocks = r78_250$grid,
      time = round(r78_250$time/1e6, 2),
      p.type = 'rocks',
      leg.pos = 'bottom',
      box = c(-18, 180, 900, 1600)
    ),
  viscosity =
    draw_grid(
      nodes = n78_250$nodes,
      time = round(r78_250$time/1e6, 2),
      p.type = 'viscosity',
      v.pal = 'viridis',
      box = c(-18, 180, 900, 1600)
    ),
  strain =
    draw_grid(
      nodes = n78_250$nodes,
      time = round(r78_250$time/1e6, 2),
      p.type = 'strain',
      v.pal = 'viridis',
      box = c(-18, 180, 900, 1600)
    ),
  stream =
    draw_grid(
    nodes = n78_250$nodes,
    time = round(r78_250$time/1e6, 2),
    p.type = 'stream',
    v.pal = 'viridis',
    box = c(-18, 180, 900, 1600)
  )
)
pA4 <-
  (
    pA4.grids$rocks.zoom +
    guides(fill = 'none') +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  ) /
  (
    pA4.grids$viscosity +
    guides(
      fill = 
        guide_colorbar(
          title = bquote(log10~Pa %.% s),
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  ) /
  (
    pA4.grids$strain +
    guides(
      fill = 
        guide_colorbar(
          title = bquote(log10~s^-1),
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  ) /
  (
    pA4.grids$stream +
    guides(
      color = 
        guide_colorbar(
          title = bquote(cm~yr ^ -1),
          inside = T,
          title.position = 'top',
          title.vjust = -1,
          barheight = unit(0.8, 'in')
        )
    ) +
    theme()
  ) +
  plot_layout(guides = 'keep') +
  plot_annotation(
    tag_levels = 'a',
    theme = theme(plot.margin = margin())
  ) &
  theme(
    plot.margin = margin(2, 2, 2, 2),
    legend.margin = margin(),
    legend.box.margin = margin(),
    legend.justification = 'top',
    legend.box.just = 'top',
    plot.title = element_blank()
  )
cat('\nSaving figure A4 to figs/figA4.png ...')
suppressWarnings(
  ggsave(
    filename = 'figs/figA4.png',
    plot = pA4,
    device = 'png',
    type = 'cairo',
    width = 8.5,
    height = 8.5
  )
)

# Figure A5
cat('\nDrawing figure A5 ...')
pA5 <-
  mods %>% 
  arrange(thermal.parameter) %>% 
  bind_cols(
    expand.grid(1:4, 1:16) %>% 
    rename(x = Var2, y = Var1)
  ) %>% 
  ggplot() +
  geom_raster(
    aes(x = x, y = y, fill = coupling.depth),
    hjust = 0,
    vjust = 0
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 16),
    breaks = seq(0.5, 15.5, 1),
    labels = mods$thermal.parameter[1:16][order(mods$thermal.parameter[1:16])],
    sec.axis = dup_axis(
      name = 'Model',
      labels =
        mods$model[1:16][order(mods$thermal.parameter[1:16])] %>% stringr::str_extract('cd.')
    )
  ) +
  scale_y_reverse(
    expand = c(0, 0),
    limits = c(5, 1),
    breaks = seq(1.5, 4.5, 1),
    labels = mods$upper.plate.thickness %>% unique()
  ) +
  scale_fill_gradient(low = 'grey90', high = 'grey10') +
  labs(
    x = 'thermal parameter (km/100)',
    y = 'upper-plate thickness (km)',
    fill = 'coupling\ndepth\nkm'
  ) +
  coord_fixed() +
  theme_dark(base_size = 16) +
  theme(
    axis.text = element_text(color = 'black'),
    axis.ticks = element_line(color = 'black'),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin()
  ) 
cat('\nSaving figure A5 to figs/figA5.png ...')
suppressWarnings(
  ggsave(
    'figs/figA5.png',
    plot = pA5,
    device = 'png',
    type = 'cairo',
    width = 11,
    height = 3.5
  )
)

# Figure A6
cat('\nDrawing figure A6 ...')
# Draw Plot
pA6.b <-
  mods %>% 
  ggplot() +
  geom_point(aes(x = thermal.parameter, y = coupling.depth)) +
  geom_smooth(
    aes(x = thermal.parameter, y = coupling.depth),
    method = 'lm',
    formula = y ~ x,
    color = 'grey90',
    size = 1,
    se = T
  ) +
  annotate(
    'text',
    x = Inf,
    y = -Inf,
    vjust = -0.2,
    hjust = 1.1,
    label =
      bquote(
        z[c] ==
          .(scientific(bivariate.model.thermal.parameter.linear$model$estimate[2], digits = 3))
          ~ Phi +
          .(round(bivariate.model.thermal.parameter.linear$model$estimate[1], 1)) ~~~~ R^2 ==
          .(round(bivariate.model.thermal.parameter.linear$fit$r.squared, 2))
      )
  ) +
  labs(x = 'thermal parameter (km/100)', y = 'coupling depth (km)') +
  theme_dark(base_size = 16) +
  theme(
    axis.text = element_text(color = 'black'),
    panel.grid = element_blank()
  )
pA6.a <-
  mods %>% 
  ggplot() +
  geom_boxplot(
    aes(
      x = upper.plate.thickness,
      y = coupling.depth,
      group = as.factor(upper.plate.thickness)
    ),
    fill = 'grey30',
    color = 'black',
    width = 3
  ) +
  geom_smooth(
    aes(x = upper.plate.thickness, y = coupling.depth),
    method = 'lm',
    formula = y ~ poly(x, 2),
    color = 'grey90',
    size = 1,
    se = T
  ) +
  annotate(
    'text',
    x = Inf,
    y = -Inf,
    vjust = -0.2,
    hjust = 1.1,
    label =
      bquote(
        z[c] ==
          .(
            scientific(
              bivariate.model.upper.plate.thickness.quadratic1$model$estimate[2],
              digits = 3
            )
          )
          ~ z[1100]^2 +
          .(round(bivariate.model.upper.plate.thickness.quadratic1$model$estimate[1], 1))
          ~~~~ R^2 ==
          .(round(bivariate.model.upper.plate.thickness.quadratic1$fit$r.squared, 2))
      )
  ) +
  labs(x = 'upper-plate thickness (km)', y = 'coupling depth (km)') +
  theme_dark(base_size = 16) +
  theme(
    axis.text = element_text(color = 'black'),
    panel.grid = element_blank()
  )
# Composition
cat('\nSaving figure A6 to figs/figA6.png ...')
pA6 <- 
  pA6.a + pA6.b +
  plot_annotation(tag_levels = 'a') &
  theme(plot.margin = margin(2, 2, 2, 2))
suppressWarnings(
  ggsave(
    filename = 'figs/figA6.png',
    plot = pA6,
    device = 'png',
    type = 'cairo',
    width = 10,
    height = 4.5
  )
)

# Figure A7
cat('\nDrawing figure A7 ...')
pA7 <-
  hf %>% 
  left_join(mods, by = 'model') %>% 
  group_by(upper.plate.thickness) %>% 
  ggplot() +
  geom_path(
    aes(
      x = x.position.normalized,
      y = surface.heat.flow.smooth,
      group = model,
      color = thermal.parameter
    ),
    size = 0.5
  ) +
  annotate(
    "segment",
    x = 1,
    xend = 1,
    y = 20,
    yend = 55,
    colour = "black",
    size = 1,
    arrow = arrow(length = unit(0.1,'in'), angle = 20)
  ) +
  annotate(
    "segment",
    x = 0,
    xend = 0,
    y = 75,
    yend = 55,
    colour = "black",
    size = 1,
    arrow = arrow(length = unit(0.1,'in'), angle = 20)
  ) +
  annotate(
    "segment",
    x = 1.02,
    xend = 1.5,
    y = 40,
    yend = 40,
    colour = "black",
    size = 1,
    arrow = arrow(length = unit(0.1,'in'), angle = 20)
  ) +
  annotate('text', x = 1, y = 20, label = 'Arc', size = 5, vjust = 1.25) +
  annotate('text', x = 1.3, y = 38, label = 'Backarc', size = 5, vjust = 1.25) +
  annotate('text', x = 0, y = 75, label = 'Trench', size = 5, vjust = -0.25) +
  labs(
    x = 'normalized distance',
    y = bquote('surface heat flow'~(mWm^-2)),
    color = 'thermal parameter km/100'
  ) +
  coord_cartesian(xlim = c(-0.75, 1.5), ylim = c(0, 200)) +
  scale_color_viridis_c(option = 'magma') +
  facet_wrap(
    ~upper.plate.thickness,
    labeller = labeller(
      upper.plate.thickness =
        c(
          '46' = 'upper-plate: 46 km',
          '62' = 'upper-plate: 62 km',
          '78' = 'upper-plate: 78 km',
          '94' = 'upper-plate: 94 km')
      )
  ) +
  guides(
    color =
      guide_colorbar(
        inside = T,
        title.position = 'top',
        title.vjust = -1,
        barwidth = unit(3, 'in')
      )
  ) +
  theme_dark(base_size = 16) +
  theme(
    axis.text = element_text(color = 'black'),
    legend.title = element_text(vjust = 0.8),
    strip.text = element_text(face = 'bold', size = 11),
    legend.position = 'bottom',
    panel.grid = element_blank()
  )
cat('\nSaving figure A7 to figs/figA7.png ...')
suppressWarnings(
  ggsave(
    filename = 'figs/figA7.png',
    plot = pA7,
    device = 'png',
    type = 'cairo',
    width = 8,
    height = 8
  )
)

# Write log
cat('\nvisualize.R complete!\n')
sink()

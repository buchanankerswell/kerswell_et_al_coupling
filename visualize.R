source('functions.R')
load('data/46km/cdf46.RData')
load('data/62km/cdf62.RData')
load('data/94km/cdf94.RData')
load('data/78km/cdf78_50.RData')
load('data/78km/cdf78_130.RData')
load('data/78km/cdf78_250.RData')
load('data/regressions.RData')
load('data/data.RData')

# Figure 1
# Draw figure 1
cat('Drawing figure 1\n')
p1.grids <- list(
  rocks = draw_grid(nodes = n78.init$nodes,
                    rocks = r78.init$grid,
                    time = round(r78.init$time/1e6, 2),
                    p.type = 'rocks',
                    leg.pos = 'bottom') +
    annotate('text', x = 1200, y = 0, label = 'free surface', vjust = 0.2) +
    annotate('text', x = 1200, y = Inf, label = 'open boundary', vjust = -0.2, color = 'white') +
    annotate('text', x = -Inf, y = Inf, label = 'free slip', color = 'white', angle = 90, vjust = 1.2, hjust = -0.1) +
    annotate('text', x = Inf, y = Inf, label = 'free slip', color = 'white', angle = 90, vjust = -0.2, hjust = -0.1),
  temperature = draw_grid(nodes = n78.init$nodes,
                          rocks = r78.init$grid,
                          time = round(r78.init$time/1e6, 2),
                          p.type = 'temperature',
                          v.pal = 'magma') +
    annotate('text', x = -Inf, y = Inf, label = 'insulated', angle = 90, vjust = 1.2, hjust = -0.1) +
    annotate('text', x = Inf, y = Inf, label = 'insulated', angle = 90, vjust = -0.2, hjust = -0.1) +
    annotate('text', x = 500, y = 0, label = 'constant 0 ˚C', color = 'white') +
    annotate('text', x = 800, y = 150, label = '0.5 ˚C/km adiabat') +
    annotate('text', x = 1700, y = 78, label = 'upper plate thickness', hjust = 1) +
    annotate('segment', x = 1710, xend = 1950, y = 78, yend = 78, linetype = 'twodash', lineend = 'round', linejoin = 'round') +
    annotate('curve', x = 150, xend = 5, y = 150, yend = 40, curvature = -0.2, arrow = arrow(length = unit(0.1, 'in'), angle = 20)) +
    annotate('text', x = 155, y = 150, label = 'mid-ocean ridge', hjust = 0),
  viscosity = draw_grid(nodes = n78.init$nodes,
                        rocks = r78.init$grid,
                        time = round(r78.init$time/1e6, 2),
                        p.type = 'viscosity',
                        v.pal = 'viridis') +
    annotate('rect', xmin = 490, xmax = 510, ymin = -4, ymax = 40, fill = NA, color = 'black') +
    annotate('rect', xmin = 1790, xmax = 1810, ymin = -4, ymax = 40, fill = NA, color = 'black') +
    annotate('segment', x = 510, xend = 560, y = 20, yend = 20, arrow = arrow(length = unit(0.1, 'in'), angle = 20), color = 'black', lineend = 'round', linejoin = 'round') +
    annotate('segment', xend = 1740, x = 1790, y = 20, yend = 20, arrow = arrow(length = unit(0.1, 'in'), angle = 20), color = 'black', lineend = 'round', linejoin = 'round') +
    annotate('text', x = 400, y = 150, label = 'convergence region', hjust = 1) +
    annotate('curve', x = 405, xend = 500, y = 150, yend = 40, curvature = 0.5, arrow = arrow(length = unit(0.1, 'in'), angle = 20), lineend = 'round') +
    annotate('text', x = 1700, y = 150, label = 'weak zone', hjust = 0) +
    annotate('curve', x = 1695, xend = 1490, y = 150, yend = 75, curvature = -0.2, arrow = arrow(length = unit(0.1, 'in'), angle = 20), lineend = 'round')
  )

# Plot figure 1
cat('Saving figure 1 to figs/fig1.png\n')
p1 <- plot_grid(nodes = p1.grids[2:3],
                rocks = p1.grids$rocks,
                leg.dir = 'horizontal',
                rock.rows = 2,
                leg.title.pos = 'left',
                leg.title.vjust = 0.8)
ggsave('figs/fig1.png',
       plot = p1,
       device = 'png',
       type = 'cairo',
       width = 11,
       height = 6.5,
     bg = 'transparent')

# Figure 2
# Draw figure 2
cat('Drawing figure 2\n')
# 2a
p2.a <- expand.grid(1:4, 1:4) %>% 
  tibble() %>% 
  mutate(phi = mods$phi[mods$z1100 == 46],
         model = mods$model[mods$z1100 == 46] %>% stringr::str_extract('cd.')) %>% 
  rename(age = Var2, cv = Var1) %>% 
  ggplot() +
  geom_raster(aes(x = cv, y = age, fill = phi), hjust = 0, vjust = 0) +
  geom_label(aes(x = cv-0.5, y = age+0.5, label = model), alpha = 0.6) +
  labs(x = 'Convergence Velocity (km/Ma)', y = 'Age (Ma)', fill = 'km/100', title = 'Range of Slab Thermal States') +
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
  scale_fill_gradient(low = 'grey90', high = 'grey10') +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(color = 'black'),
    axis.ticks = element_line(color = 'black'),
    axis.line = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(vjust = 0.8)
  )
# 2b
p2.b <- geotherms %>% 
  pivot_longer(-depth, names_to = 'z1100', values_to = 'T') %>% 
  mutate('z1100' = stringr::str_extract(z1100, '[0-9]+') %>% as.factor()) %>% 
  ggplot() +
  geom_line(aes(x = T, y = depth, linetype = z1100), size=0.5, na.rm = T) +
  annotate('segment', x = 1100, xend = 1100, y = 30, yend = 46,
           arrow = arrow(length = unit(0.1, 'in'), angle = 20), linejoin = 'round', lineend = 'round') +
  annotate('segment', x = 1100, xend = 1100, y = 30, yend = 62,
           arrow = arrow(length = unit(0.1, 'in'), angle = 20), linejoin = 'round', lineend = 'round') +
  annotate('segment', x = 1100, xend = 1100, y = 30, yend = 78,
           arrow = arrow(length = unit(0.1, 'in'), angle = 20), linejoin = 'round', lineend = 'round') +
  annotate('segment', x = 1100, xend = 1100, y = 30, yend = 94,
           arrow = arrow(length = unit(0.1, 'in'), angle = 20), linejoin = 'round', lineend = 'round') +
  annotate('text', x = 1100, y = 28, angle = 90, label = bquote(z[1100]), hjust = 0) +
  labs(x = bquote(Temperature~(degree*C)),
       y = 'Depth (km)',
       title = 'Model Geotherms',
       linetype = bquote(z[1100])) +
  scale_y_reverse(limits=c(120,0)) +
  scale_x_continuous(limits = c(0,1300)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(color = 'black'),
    axis.ticks = element_line(color = 'black'),
    legend.position = 'bottom'
  )
# Composition
p2 <- p2.a + p2.b +
  plot_annotation(tag_levels = 'a',
                  theme = theme(plot.margin = margin(),
                                plot.background = element_rect(fill = 'transparent', color = NA),
                                panel.background = element_rect(fill = 'transparent', color = NA))) +
  plot_layout(widths = 1) &
  theme(plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA),
        legend.background = element_rect(fill = 'transparent', color = NA),
        plot.margin = margin())
# Plot figure 2
cat('Saving figure 2 to figs/fig2.png\n')
suppressWarnings(ggsave(filename = 'figs/fig2.png',
                        device = 'png',
                        type = 'cairo',
                        width = 8,
                        height = 5,
                        bg = 'transparent'))

# Figure 3
cat('Drawing figure 3\n')

p3.grids <- list(
  hf = draw_grid(hf = mods %>% filter(model == 'cdf78' && tstep == 250) %>% unnest(hf),
                 time = round(r78_250$time1/e6, 2),
                 p.type = 'hf'),
  rocks = draw_grid(nodes = n78_250$nodes,
                    rocks = r78_250$grid,
                    time = round(r78_250$time/1e6, 2),
                    p.type = 'rocks',
                    leg.pos = 'bottom'),
  rocks.zoom = draw_grid(nodes = n78_250$nodes,
                         rocks = r78_250$grid,
                         time = round(r78_250$time/1e6, 2),
                         p.type = 'rocks',
                         leg.pos = 'bottom',
                         box = c(-18, 180, 900, 1600)),
  temperature = draw_grid(nodes = n78_250$nodes,
                          rocks = r78_250$grid,
                          time = round(r78_250$time/1e6, 2),
                          p.type = 'temperature',
                          v.pal = 'magma',
                          box = c(-18, 180, 900, 1600)),
  viscosity = draw_grid(nodes = n78_250$nodes,
                        rocks = r78_250$grid,
                        time = round(r78_250$time/1e6, 2),
                        p.type = 'viscosity',
                        v.pal = 'viridis',
                        box = c(-18, 180, 900, 1600)),
  strain = draw_grid(nodes = n78_250$nodes,
                     rocks = r78_250$grid,
                     time = round(r78_250$time/1e6, 2),
                     p.type = 'strain',
                     v.pal = 'viridis',
                     box = c(-18, 180, 900, 1600)),
  shear = draw_grid(nodes = n78_250$nodes,
                    rocks = r78_250$grid,
                    time = round(r78_250$time/1e6, 2),
                    p.type = 'shear',
                    v.pal = 'viridis',
                    box = c(-18, 180, 900, 1600)),
  stream = draw_grid(nodes = n78_250$nodes,
                     rocks = r78_250$grid,
                     time = round(r78_250$time/1e6, 2),
                     p.type = 'stream',
                     v.pal = 'viridis',
                     box = c(-18, 180, 900, 1600))
)

# Plot  composition
cat('Saving figure 3 to figs/fig3.png\n')
p3 <- 
  p3.grids$hf /
  (p3.grids$rocks + guides(fill = guide_legend(nrow = 5, title.position = 'top', title.hjust = 0.5))) /
  ((p3.grids$rocks.zoom + guides(fill = guide_legend(nrow = 5, title.position = 'top', title.hjust = 0.5)) + theme(axis.text.x = element_blank(), axis.title.x = element_blank())) + (p3.grids$temperature + theme(axis.text.x = element_blank(), axis.title.x = element_blank()))) /
  ((p3.grids$viscosity + theme(axis.text.x = element_blank(), axis.title.x = element_blank())) + (p3.grids$strain + theme(axis.text.x = element_blank(), axis.title.x = element_blank()))) /
  (p3.grids$shear + (p3.grids$stream + theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_rect(fill = 'transparent', color = NA)))) +
  plot_layout(heights = c(1, 1, 1.11, 1.11, 1.11), guides = 'collect') +
  plot_annotation(tag_levels = 'a', theme = theme(plot.margin = margin(), plot.background = element_rect(fill = 'transparent', color = NA), panel.background = element_rect(fill = 'transparent', color = NA))) &
  theme(legend.position = 'bottom', legend.box = 'horizontal', legend.direction = 'vertical')
  
ggsave('figs/fig3.png',
       plot = p3,
       device = 'png',
       type = 'cairo',
       width = 11,
       height = 13,
       bg = 'transparent')

# Figure 4
cat('Drawing figure 4\n')
p4 <- mods %>% 
  arrange(phi) %>% 
  bind_cols(expand.grid(1:4, 1:16) %>% 
              rename(x = Var2, y = Var1)) %>% 
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = zc),
              hjust = 0,
              vjust = 0) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 16),
    breaks = seq(0.5, 15.5, 1),
    labels = mods$phi[1:16][order(mods$phi[1:16])],
    sec.axis = dup_axis(
      name = 'Model',
      labels = mods$model[1:16][order(mods$phi[1:16])] %>% stringr::str_extract('cd.')
    )
  ) +
  scale_y_reverse(
    expand = c(0, 0),
    limits = c(5, 1),
    breaks = seq(1.5, 4.5, 1),
    labels = mods$z1100 %>% unique()
  ) +
  scale_fill_gradient(low = 'grey90', high = 'grey10') +
  labs(x = bquote(Phi~(km/100)), y = bquote(z[1100]~(km)), fill = bquote(z[c]~(km))) +
  coord_fixed() +
  theme_classic() +
  theme(
    axis.text = element_text(color = 'black'),
    axis.ticks = element_line(color = 'black'),
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.background = element_rect(fill = 'transparent', color = NA),
    legend.background = element_rect(fill = 'transparent', color = NA),
    plot.margin = margin()
  ) 
cat('Saving figure 4 to figs/fig4.png\n')
ggsave('figs/fig4.png',
       plot = p4,
       device = 'png',
       type = 'cairo',
       width = 11,
       height = 3.5,
       bg = 'transparent')

# Figure 5
cat('Drawing figure 5\n')
# Draw Plot
p5.b <- mods %>% 
  ggplot() +
  geom_point(aes(x = phi, y = zc)) +
  geom_smooth(aes(x = phi, y = zc), method = 'lm', formula = y ~ x, color = 'black', size = 0.5, se = T) +
  annotate('text', x = Inf, y = -Inf, vjust = -0.2, hjust = 1.1,
           label = bquote(z[c] == .(scientific(bvm.phi.lin$model$estimate[2], digits = 3)) ~ Phi +
                            .(round(bvm.phi.lin$model$estimate[1], 1)) ~~~~
                            R^2 == .(round(bvm.phi.lin$fit$r.squared, 2)))) +
  labs(x = bquote(Phi~(km/100)), y = bquote(z[c]~(km))) +
  theme_classic() +
  theme(axis.text = element_text(color = 'black'),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
p5.a <- mods %>% 
  ggplot() +
  geom_boxplot(aes(x = z1100, y = zc, group = as.factor(z1100))) +
  geom_smooth(aes(x = z1100, y = zc), method = 'lm', formula = y ~ poly(x, 2), color = 'black', size = 0.5, se = T) +
  annotate('text', x = Inf, y = -Inf, vjust = -0.2, hjust = 1.1,
           label = bquote(z[c] == .(scientific(bvm.z1100.quad1$model$estimate[2], digits = 3)) ~ z[1100]^2 +
                            .(round(bvm.z1100.quad1$model$estimate[1], 1)) ~~~~
                            R^2 == .(round(bvm.z1100.quad1$fit$r.squared, 2)))) +
  labs(x = bquote(z[1100]~(km)), y = bquote(z[c]~(km))) +
  theme_classic() +
  theme(axis.text = element_text(color = 'black'),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))

# Composition
cat('Saving figure 5 to figs/fig5.png\n')
p5 <- p5.a + p5.b +
  plot_annotation(tag_levels = 'a',
                  theme = theme(plot.background = element_rect(fill = 'transparent', color = NA),
                                panel.background = element_rect(fill = 'transparent', color = NA))) &
  theme(plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
suppressWarnings(ggsave(filename = 'figs/fig5.png',
                        plot = p5,
                        device = 'png',
                        type = 'cairo',
                        width = 10,
                        height = 5,
                        bg = 'transparent'))

# Figure 6
cat('Drawing figure 6\n')
grid <- expand.grid(z1100 = seq(0, 120, length.out = 100),
            phi = seq(0, 120, length.out = 100),
            stringsAsFactors = F) %>% 
  tibble()
p6.a <- grid %>% 
  mutate(zc = predict(lm(zc ~ z1100 + phi, mods), newdata = grid)) %>% 
  ggplot() +
  geom_contour_fill(aes(x = phi, y = z1100, z = zc), size = 0.2, color = 'black', show.legend = F, alpha = 0.75) +
  geom_text_contour(aes(x = phi, y = z1100, z = zc),
                    stroke = 0.2,
                    size = 3,
                    label.placement = label_placement_fraction(frac = 0.05)) +
  geom_point(data = mods, aes(x = phi, y = z1100), shape = 15) +
  geom_point(data = segs, aes(x = phi, y = z1100), color = 'white') +
  geom_text_repel(data = segs, aes(x = phi, y = z1100, label = segment), color = 'white', size = 3) +
  coord_cartesian(expand = F) +
  labs(x = bquote(Phi~(km/100)), y = bquote(z[1100]~(km))) +
  scale_fill_gradient(low = 'grey20', high = 'grey80') +
  theme_classic() +
  theme(axis.text = element_text(color = 'black'))
p6.b <- grid %>% 
  mutate(zc = predict(lm(zc ~ I(z1100^2) + phi, mods), newdata = grid)) %>% 
  ggplot() +
  geom_contour_fill(aes(x = phi, y = z1100, z = zc), size = 0.2, color = 'black', show.legend = F, alpha = 0.75) +
  geom_text_contour(aes(x = phi, y = z1100, z = zc),
                    stroke = 0.2,
                    size = 3,
                    label.placement = label_placement_fraction(frac = 0.05)) +
  geom_point(data = mods, aes(x = phi, y = z1100), shape = 15) +
  geom_point(data = segs, aes(x = phi, y = z1100), color = 'white') +
  geom_text_repel(data = segs, aes(x = phi, y = z1100, label = segment), color = 'white', size = 3) +
  coord_cartesian(expand = F) +
  labs(x = bquote(Phi~(km/100)), y = bquote(z[1100]~(km))) +
  scale_fill_gradient(low = 'grey20', high = 'grey80') +
  theme_classic() +
  theme(axis.text = element_text(color = 'black'))
p6.c <- grid %>% 
  mutate(zc = predict(lm(zc ~ poly(z1100, 2) + phi, mods), newdata = grid)) %>% 
  ggplot() +
  geom_contour_fill(aes(x = phi, y = z1100, z = zc), size = 0.2, color = 'black', show.legend = F, alpha = 0.75) +
  geom_text_contour(aes(x = phi, y = z1100, z = zc),
                    stroke = 0.2,
                    size = 3,
                    label.placement = label_placement_fraction(frac = 0.05)) +
  geom_point(data = mods, aes(x = phi, y = z1100), shape = 15) +
  geom_point(data = segs, aes(x = phi, y = z1100), color = 'white') +
  geom_text_repel(data = segs, aes(x = phi, y = z1100, label = segment), color = 'white', size = 3) +
  coord_cartesian(expand = F) +
  labs(x = bquote(Phi~(km/100)), y = bquote(z[1100]~(km))) +
  scale_fill_gradient(low = 'grey20', high = 'grey80') +
  theme_classic() +
  theme(axis.text = element_text(color = 'black'))
cp <- segs %>% 
  select(zc.lin, zc.quad1, zc.quad2) %>% 
  rename('linear (a)' = zc.lin, 'quadratic (b)' = zc.quad1, 'quadratic (c)' = zc.quad2) %>% 
  pivot_longer(everything(), names_to = 'mod') %>% 
  group_by(mod)
p6.d <- cp %>% 
  ggplot() +
  geom_histogram(aes(x = value, group = mod, fill = mod), bins = 7, show.legend = F) +
  labs(x = bquote(z[c]~(km)), y = 'Frequency') +
  scale_fill_grey(start = 0.1, end = 0.7) +
  theme_classic() +
  theme(axis.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(size = 12)) +
  facet_wrap(~mod)

# Composition
cat('Saving figure 6 to figs/fig6.png\n')
p6 <- p6.a + p6.b + p6.c + p6.d +
  plot_layout(nrow = 2, ncol = 2) +
  plot_annotation(tag_levels = 'a', theme = theme(plot.margin = margin())) &
  theme(plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
suppressWarnings(ggsave(filename = 'figs/fig6.png',
                        plot = p6,
                        device = 'png',
                        type = 'cairo',
                        width = 8,
                        height = 8,
                        bg = 'transparent'))

# Figure 7
cat('Drawing figure 7\n')
p7 <- mods %>% 
  unnest(hf) %>% 
  group_by(z1100) %>% 
  ggplot() +
  geom_path(aes(x = xnorm, y = smooth, group = model, color = phi), size = 0.5) +
  annotate("segment", x = 1, xend = 1, y = 30, yend = 55, colour = "black", size=1, arrow=arrow(length = unit(0.1,'in'), angle = 20)) +
  annotate("segment", x = 0, xend = 0, y = 75, yend = 55, colour = "black", size=1, arrow=arrow(length = unit(0.1,'in'), angle = 20)) +
  annotate("segment", x = 1.02, xend = 1.5, y = 40, yend = 40, colour = "black", size=1, arrow=arrow(length = unit(0.1,'in'), angle = 20)) +
  annotate('text', x = 1, y = 30, label = 'Arc', size = 3, vjust = 1.25) +
  annotate('text', x = 1.25, y = 38, label = 'Backarc', size = 3, vjust = 1.25) +
  annotate('text', x = 0, y = 75, label = 'Trench', size = 3, vjust = -0.25) +
  labs(x = 'Normalized Distance', y = bquote(Surface~Heat~Flow~(mWm^-2)), color = bquote(Phi~(km/100))) +
  coord_cartesian(xlim = c(-0.75, 1.5), ylim = c(0, 200)) +
  scale_color_gradient(low = 'grey0', high = 'grey80') +
  facet_wrap(~z1100, labeller = labeller(z1100 = c('46' = '46km lithosphere', '62' = '62km lithosphere', '78' = '78km lithosphere', '94' = '94km lithosphere'))) +
  guides(color = guide_colorbar(barwidth = unit(10, 'lines'))) +
  theme_classic() +
  theme(axis.text = element_text(color = 'black'),
        legend.title = element_text(vjust = 0.8),
        strip.text = element_text(face = 'bold', size = 11),
        legend.position = 'bottom',
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA),
        legend.background = element_rect(fill = 'transparent', color = NA),
        strip.background = element_rect(fill = 'transparent', color = NA))
cat('Saving figure 7 to figs/fig7.png\n')
suppressWarnings(ggsave(filename = 'figs/fig7.png',
                        plot = p7,
                        device = 'png',
                        type = 'cairo',
                        width = 8,
                        height = 8,
                        bg = 'transparent'))

# Figure 8
cat('Drawing figure 8\n')
p8 <- mods[grepl('cdf', mods$model),] %>% 
  unnest(hf) %>% 
  ggplot() +
  geom_path(aes(x = xnorm, y = smooth, group = model, color = as.factor(z1100)), show.legend = ) +
  annotate("segment", x = 1, xend = 1, y = 30, yend = 55, colour = "black", size=1, arrow=arrow(length = unit(0.1,'in'))) +
  annotate("segment", x = 0, xend = 0, y = 75, yend = 55, colour = "black", size=1, arrow=arrow(length = unit(0.1,'in'))) +
  annotate("segment", x = 1.02, xend = 1.5, y = 40, yend = 40, colour = "black", size=1, arrow=arrow(length = unit(0.1,'in'))) +
  annotate('text', x = 1, y = 30, label = 'Arc', size = 3, vjust = 1.25) +
  annotate('text', x = 1.25, y = 38, label = 'Backarc', size = 3, vjust = 1.25) +
  annotate('text', x = 0, y = 75, label = 'Trench', size = 3, vjust = -0.25) +
  labs(x = 'Normalized Distance', y = bquote(Surface~Heat~Flow~(mWm^-2)), color = bquote(z[1100]~(km))) +
  coord_cartesian(xlim = c(-0.75, 1.5), ylim = c(0, 100)) +
  scale_color_grey(start = 0.8, end = 0.2) +
  theme_classic() +
  theme(axis.text = element_text(color = 'black'),
        legend.title = element_text(vjust = 0.8),
        legend.position = 'bottom',
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA),
        legend.background = element_rect(fill = 'transparent', color = NA),
        strip.background = element_rect(fill = 'transparent', color = NA))
cat('Saving figure 8 to figs/fig8.png\n')
suppressWarnings(ggsave(filename = 'figs/fig8.png',
                        plot = p8,
                        device = 'png',
                        type = 'cairo',
                        width = 5,
                        height = 5,
                        bg = 'transparent'))

# Figure 9
cat('Drawing figure 9\n')
p9.grids <- list(
  temperature = draw_grid(nodes = n78_250$nodes,
                          rocks = r78_250$grid,
                          time = round(r78_250$time/1e6, 2),
                          p.type = 'temperature',
                          v.pal = 'magma',
                          arrows = T,
                          box = c(30, 180, 1300, 1600)),
  viscosity = draw_grid(nodes = n78_250$nodes,
                        rocks = r78_250$grid,
                        time = round(r78_250$time/1e6, 2),
                        p.type = 'viscosity',
                        v.pal = 'viridis',
                        arrows = T,
                        box = c(30, 180, 1300, 1600)) +
    annotate('label', x = 1320, y = 70, label = 'a', size = 3) +
    annotate('label', x = 1375, y = 105, label = 'b', size = 3) +
    annotate('label', x = 1355, y = 80, label = 'c', size = 3) +
    annotate('label', x = 1345, y = 83, label = 'd', size = 3) +
    annotate('label', x = 1380, y = 85, label = 'e', size = 3)
)
p9 <- plot_grid(nodes = p9.grids,
          leg.pos = 'bottom',
          leg.box = 'horizontal',
          leg.title.vjust = 0.8,
          leg.title.pos = 'left',
          leg.dir = 'horizontal',
          leg.title.hjust = 0,
          leg.collect = T) &
  guides(fill = guide_colorbar(barwidth = unit(8, 'lines'), title.vjust = 0.8))
cat('Saving figure 9 to figs/fig9.png\n')
suppressWarnings(ggsave(filename = 'figs/fig9.png',
                        plot = p9,
                        device = 'png',
                        type = 'cairo',
                        width = 7,
                        height = 8,
                        bg = 'transparent'))

# Figure 10
cat('Drawing figure 10\n')
p10.grids <- list(
  stream = draw_grid(nodes = n46$nodes,
                     rocks = r46$grid,
                     time = round(r46$time/1e6, 2),
                     p.type = 'stream',
                     v.pal = 'viridis',
                     box = c(-18, 180, 900, 1600)),
  stream = draw_grid(nodes = n62$nodes,
                     rocks = r62$grid,
                     time = round(r62$time/1e6, 2),
                     p.type = 'stream',
                     v.pal = 'viridis',
                     box = c(-18, 180, 900, 1600)),
  stream = draw_grid(nodes = n78_250$nodes,
                     rocks = r78_250$grid,
                     time = round(r78_250$time/1e6, 2),
                     p.type = 'stream',
                     v.pal = 'viridis',
                     box = c(-18, 180, 900, 1600)),
  stream = draw_grid(nodes = n94$nodes,
                     rocks = r94$grid,
                     time = round(r94$time/1e6, 2),
                     p.type = 'stream',
                     v.pal = 'viridis',
                     box = c(-18, 180, 900, 1600))
)
v.color.pal <- scale_color_viridis_c(option = 'viridis', limits = c(0, 10))
p10 <- plot_grid(nodes = p10.grids, leg.collect = F) &
  v.color.pal &
  guides(color = guide_colorbar(barwidth = unit(8, 'lines'), title.vjust = 0.8)) &
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

cat('Saving figure 10 to figs/fig10.png\n')
suppressWarnings(ggsave(filename = 'figs/fig10.png',
                        plot = p10,
                        device = 'png',
                        type = 'cairo',
                        width = 8,
                        height = 11,
                        bg = 'transparent'))
# Figure A1
cat('Drawing figure A1\n')
pA1 <- antstab %>% 
  group_by(model) %>% 
  ggplot() +
  geom_point(aes(x = time, y = depth, group = model), size = 0.25) +
  geom_path(aes(x = time, y = depth, group = model)) +
  labs(x = 'Time (Ma)', y = 'Depth (km)') +
  scale_y_reverse() +
  theme_classic() +
  theme(plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA),
        strip.background = element_rect(fill = 'transparent', color = NA),
        strip.text = element_text(size = 11, face = 'bold'),
        axis.text = element_text(color = 'black')) +
  facet_grid(vars(model), vars(z1100), labeller = labeller(z1100 = c('46' = '46km', '62' = '62km', '78' = '78km', '94' = '94km')))
cat('Saving figure A1 to figs/figA1.png\n')
suppressWarnings(ggsave(filename = 'figs/figA1.png',
                        plot = pA1,
                        device = 'png',
                        type = 'cairo',
                        width = 8,
                        height = 8,
                        bg = 'transparent'))
# Figure A2
cat('Drawing figure A2\n')
# pA2.a <- PD15 %>% 
#   bind_rows(PD15 %>% mutate(data.group = 'all')) %>% 
#   group_by(data.group) %>% 
#   ggplot() +
#   geom_point(data = PD15 %>% select(-data.group), aes(x = temperature, y = pressure), size = 0.5, color = 'grey50', alpha = 0.5) +
#   geom_point(aes(x = temperature, y = pressure, color = data.group), size = 0.5, show.legend = F) +
#   scale_color_manual(values = c('#111111', RColorBrewer::brewer.pal(5, 'Set1')),
#                      breaks=c("all", "Alps", "CircumAtlantic", "CircumPacific", "Cont_Asia", "T-I-G"),
#                      labels=c("All", "Alps", "Circum Atlantic", "Circum Pacific", "Continental Asia", "Turkey-Greece-Iran")) +
#   labs(x = 'Temperature ˚C', y = 'Pressure (GPa)', color = NULL) +
#   theme_classic() +
#   theme(plot.background = element_rect(fill = 'transparent', color = NA),
#         panel.background = element_rect(fill = 'transparent', color = NA),
#         strip.background = element_rect(fill = 'transparent', color = NA),
#         strip.text = element_text(size = 12),
#         axis.text = element_text(color = 'black')) +
#   facet_wrap(~data.group, nrow = 2, labeller = labeller(data.group = c("all" = "All", "Alps" = "Alps", "CircumAtlantic" = "Circum Atlantic", "CircumPacific" = "Circum Pacific", "Cont_Asia" = "Continental Asia", "T-I-G" = "Turkey-Greece-Iran")))
pA2.b <- PD15 %>% 
  ggplot() +
  geom_path(aes(x = pressure, y = cumulative)) +
  geom_point(aes(x = pressure, y = cumulative), size = 0.3) +
  labs(x = 'Pressure (GPa)', y = 'Cumulative Probability') +
  scale_x_continuous(limits = c(0, 4.5)) +
  theme_classic() +
  theme(plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA),
        axis.text = element_text(color = 'black'))
pA2 <- pA2.b
  # pA2.a / pA2.b + plot_layout(heights = c(2,1)) + plot_annotation(tag_levels = 'a') & theme(plot.background = element_rect(fill = 'transparent', color = NA),
  #                                                                                           panel.background = element_rect(fill = 'transparent', color = NA),
  #                                                                                           strip.background = element_rect(fill = 'transparent', color = NA),
  #                                                                                           strip.text = element_text(size = 10),
  #                                                                                           axis.text = element_text(color = 'black'))
cat('Saving figure A2 to figs/figA2.png\n')
suppressWarnings(ggsave(filename = 'figs/figA2.png',
                        plot = pA2,
                        device = 'png',
                        type = 'cairo',
                        width = 5,
                        height = 5,
                        bg = 'transparent'))

# Figure A3
cat('Drawing figure A3\n')
pA3.grids <- list(
  rocks.zoom = draw_grid(nodes = n78_50$nodes,
                         rocks = r78_50$grid,
                         time = round(r78_50$time/1e6, 2),
                         p.type = 'rocks',
                         leg.pos = 'bottom',
                         box = c(-18, 180, 1050, 1750)),
  temperature = draw_grid(nodes = n78_50$nodes,
                          time = round(r78_50$time/1e6, 2),
                          p.type = 'temperature',
                          v.pal = 'magma',
                          box = c(-18, 180, 1050, 1750)),
  viscosity = draw_grid(nodes = n78_50$nodes,
                        time = round(r78_50$time/1e6, 2),
                        p.type = 'viscosity',
                        v.pal = 'viridis',
                        box = c(-18, 180, 1050, 1750)),
  # strain = draw_grid(nodes = n78_50$nodes,
  #                    time = round(r78_50$time/1e6, 2),
  #                    p.type = 'strain',
  #                    v.pal = 'viridis',
  #                    box = c(-18, 180, 1050, 1750)),
  # shear = draw_grid(nodes = n78_50$nodes,
  #                   time = round(r78_50$time/1e6, 2),
  #                   p.type = 'shear',
  #                   v.pal = 'viridis',
  #                   box = c(-18, 180, 1050, 1750)),
  stream = draw_grid(nodes = n78_50$nodes,
                     time = round(r78_50$time/1e6, 2),
                     p.type = 'stream',
                     v.pal = 'viridis',
                     box = c(-18, 180, 1050, 1750))
)
pA3 <- plot_grid(rocks = pA3.grids[[1]], nodes = pA3.grids[2:4])
cat('Saving figure A3 to figs/figA3.png\n')
suppressWarnings(ggsave(filename = 'figs/figA3.png',
                        plot = pA3,
                        device = 'png',
                        type = 'cairo',
                        width = 7,
                        height = 11,
                        bg = 'transparent'))

# Figure A4
cat('Drawing figure A4\n')
pA4.grids <- list(
  rocks.zoom = draw_grid(nodes = n78_130$nodes,
                         rocks = r78_130$grid,
                         time = round(r78_130$time/1e6, 2),
                         p.type = 'rocks',
                         leg.pos = 'bottom',
                         box = c(-18, 180, 1050, 1750)),
  temperature = draw_grid(nodes = n78_130$nodes,
                          time = round(r78_130$time/1e6, 2),
                          p.type = 'temperature',
                          v.pal = 'magma',
                          box = c(-18, 180, 1050, 1750)),
  viscosity = draw_grid(nodes = n78_130$nodes,
                        time = round(r78_130$time/1e6, 2),
                        p.type = 'viscosity',
                        v.pal = 'viridis',
                        box = c(-18, 180, 1050, 1750)),
  # strain = draw_grid(nodes = n78_130$nodes,
  #                    time = round(r78_130$time/1e6, 2),
  #                    p.type = 'strain',
  #                    v.pal = 'viridis',
  #                    box = c(-18, 180, 1050, 1750)),
  # shear = draw_grid(nodes = n78_130$nodes,
  #                   time = round(r78_130$time/1e6, 2),
  #                   p.type = 'shear',
  #                   v.pal = 'viridis',
  #                   box = c(-18, 180, 1050, 1750)),
  stream = draw_grid(nodes = n78_130$nodes,
                     time = round(r78_130$time/1e6, 2),
                     p.type = 'stream',
                     v.pal = 'viridis',
                     box = c(-18, 180, 1050, 1750))
)
pA4 <- plot_grid(rocks = pA4.grids[[1]], nodes = pA4.grids[2:4])
cat('Saving figure A4 to figs/figA4.png\n')
suppressWarnings(ggsave(filename = 'figs/figA4.png',
                        plot = pA4,
                        device = 'png',
                        type = 'cairo',
                        width = 7,
                        height = 11,
                        bg = 'transparent'))

# Figure A5
cat('Drawing figure A5\n')
pA5.grids <- list(
  rocks.zoom = draw_grid(nodes = n78_250$nodes,
                         rocks = r78_250$grid,
                         time = round(r78_250$time/1e6, 2),
                         p.type = 'rocks',
                         leg.pos = 'bottom',
                         box = c(-18, 180, 900, 1600)),
  temperature = draw_grid(nodes = n78_250$nodes,
                          time = round(r78_250$time/1e6, 2),
                          p.type = 'temperature',
                          v.pal = 'magma',
                          box = c(-18, 180, 900, 1600)),
  viscosity = draw_grid(nodes = n78_250$nodes,
                        time = round(r78_250$time/1e6, 2),
                        p.type = 'viscosity',
                        v.pal = 'viridis',
                        box = c(-18, 180, 900, 1600)),
  # strain = draw_grid(nodes = n78_250$nodes,
  #                    time = round(r78_250$time/1e6, 2),
  #                    p.type = 'strain',
  #                    v.pal = 'viridis',
  #                    box = c(-18, 180, 900, 1600)),
  # shear = draw_grid(nodes = n78_250$nodes,
  #                   time = round(r78_250$time/1e6, 2),
  #                   p.type = 'shear',
  #                   v.pal = 'viridis',
  #                   box = c(-18, 180, 900, 1600)),
  stream = draw_grid(nodes = n78_250$nodes,
                     time = round(r78_250$time/1e6, 2),
                     p.type = 'stream',
                     v.pal = 'viridis',
                     box = c(-18, 180, 900, 1600))
)
pA5 <- plot_grid(rocks = pA5.grids[[1]], nodes = pA5.grids[2:4])
cat('Saving figure A5 to figs/figA5.png\n')
suppressWarnings(ggsave(filename = 'figs/figA5.png',
                        plot = pA5,
                        device = 'png',
                        type = 'cairo',
                        width = 7,
                        height = 11,
                        bg = 'transparent'))

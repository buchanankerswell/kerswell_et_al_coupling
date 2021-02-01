source('functions.R')

nodes.grid <- read_nodes('/Volumes/hd/nmods/kerswell_et_al_coupling/dataset/I2VIS_raw_data/78km/cdf78', 25)
rocks.grid <- read_rock_nodes('/Volumes/hd/nmods/kerswell_et_al_coupling/dataset/I2VIS_raw_data/78km/cdf78', 25)

p0 <- rocks.grid[[1]] %>%
  ggplot() +
  geom_raster(aes(x = x/2, y = z/2, fill = as.factor(type)), show.legend = F) +
  geom_contour(data = nodes.grid$grid.cdf78_240, aes(x = x/1000, y = z/1000, z = tk-273.15), size = 0.2, color = 'white') +
  geom_text_contour(data = nodes.grid$grid.cdf78_240, aes(x = x/1000, y = z/1000, z = tk-273.15), color = 'white', size = 3) +
  labs(x = 'Distance (km)', y = 'Depth (km)', fill = bquote(kg/m^3), title = 'Rock Type') +
  coord_equal(expand = F) +
  scale_y_reverse(limits = c(200, 0)) +
  scale_x_continuous(limits = c(500, 1600)) +
  theme_minimal()

p1 <- nodes.grid$grid.cdf78_240 %>%
  ggplot() +
  geom_tile(aes(x = x/1000, y = z/1000, width = w/1000, fill = ro)) +
  geom_contour(aes(x = x/1000, y = z/1000, z = tk-273.15), size = 0.2, color = 'white') +
  geom_text_contour(aes(x = x/1000, y = z/1000, z = tk-273.15), color = 'white', size = 3) +
  labs(x = 'Distance (km)', y = 'Depth (km)', fill = bquote(kg/m^3), title = 'Density') +
  coord_equal(expand = F) +
  scale_y_reverse(limits = c(200, 0)) +
  scale_x_continuous(limits = c(500, 1600)) +
  scale_fill_viridis_c(option = 'viridis', direction = -1) +
  theme_minimal()

p2 <- nodes.grid$grid.cdf78_240 %>%
  ggplot() +
  geom_tile(aes(x = x/1000, y = z/1000, width = w/1000, fill = tk-273.15)) +
  geom_contour(aes(x = x/1000, y = z/1000, z = tk-273.15), size = 0.2, color = 'white') +
  geom_text_contour(aes(x = x/1000, y = z/1000, z = tk-273.15), color = 'white', size = 3) +
  labs(x = 'Distance (km)', y = 'Depth (km)', fill = bquote(Celcius), title = 'Temperature') +
  coord_equal(expand = F) +
  scale_y_reverse(limits = c(200, 0)) +
  scale_x_continuous(limits = c(500, 1600)) +
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  theme_minimal()

p3 <- nodes.grid$grid.cdf78_240 %>%
  ggplot() +
  geom_tile(aes(x = x/1000, y = z/1000, width = w/1000, fill = pr/1e9)) +
  geom_contour(aes(x = x/1000, y = z/1000, z = tk-273.15), size = 0.2, color = 'white') +
  geom_text_contour(aes(x = x/1000, y = z/1000, z = tk-273.15), color = 'white', size = 3) +
  labs(x = 'Distance (km)', y = 'Depth (km)', fill = bquote(GPa), title = 'Pressure') +
  coord_equal(expand = F) +
  scale_y_reverse(limits = c(200, 0)) +
  scale_x_continuous(limits = c(500, 1600)) +
  scale_fill_viridis_c(option = 'viridis') +
  theme_minimal()

p <- (p0 + theme(axis.text.x = element_blank(), axis.title.x = element_blank())) /
(p1 + theme(axis.text.x = element_blank(), axis.title.x = element_blank())) /
(p2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank())) /
 p3 + plot_annotation(tag_levels = 'a')

ggsave('figs/nodes.png', plot = p, device = 'png', type = 'cairo', height = 10, width = 10)

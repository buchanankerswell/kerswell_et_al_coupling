# Load library
library(ggplot2)
library(viridis)

# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('reduced_coupling_data.RData')

# Open pdf for plotting
pdf(width = 11, height = 8, file = 'Fig4.pdf')

# Add columns repeating xvec and yvec for raster plot
reducedAllModelsDataframe$xvec = rep(seq(1,16,1), 4)
reducedAllModelsDataframe$yvec = c(rep(1,16), rep(2,16), rep(3,16), rep(4,16))

# Draw Plot
ggplot(data = reducedAllModelsDataframe) +
  geom_raster(aes(xvec, yvec, fill = z.coupling),
              hjust = 0,
              vjust = 0) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 16),
    breaks = seq(0.5, 15.5, 1),
    labels = c(
      '13.0',
      '21.5',
      '22.0',
      '26.1',
      '32.6',
      '34.0',
      '36.3',
      '44.0',
      '44.0',
      '55.0',
      '56.1',
      '68.0',
      '72.6',
      '85.0',
      '88.0',
      '110.0'
    ),
    sec.axis = dup_axis(
      name = 'Model',
      labels = c(
        'cda',
        'cdb',
        'cde',
        'cdc',
        'cdd',
        'cdi',
        'cdf',
        'cdg',
        'cdm',
        'cdh',
        'cdj',
        'cdk',
        'cdn',
        'cdl',
        'cdo',
        'cdp'
      )
    )
  ) +
  scale_y_reverse(
    expand = c(0, 0),
    limits = c(5, 1),
    breaks = seq(1.5, 4.5, 1),
    labels = c(46, 62, 78, 94)
  ) +
  # scale_fill_viridis_c(direction = -1, option = 'viridis') +
  scale_fill_gradient(low = 'grey90', high = 'grey10') +
  xlab(expression(
    paste('Slab Thermal Parameter, ', Phi, '/100 (km/100)')
  )) +
  ylab(expression(atop(
    'Lith. Thickness', paste(z[1100], ' (km)')
  ))) +
  labs(fill = expression(atop(
    'Coupling Depth', paste(z[C], ' (km)')
  ))) +
  coord_fixed() +
  annotate(
    'text',
    label = '66',
    x = 0.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '74',
    x = 1.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '72',
    x = 2.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '69',
    x = 3.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '67',
    x = 4.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '80',
    x = 5.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '78',
    x = 6.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '78',
    x = 7.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '79',
    x = 8.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '59',
    x = 9.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '70',
    x = 10.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '58',
    x = 11.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '70',
    x = 12.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '65',
    x = 13.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '68',
    x = 14.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '64',
    x = 15.5,
    y = 1.5,
    colour = 'black',
    size = 4
  ) +
  
  annotate(
    'text',
    label = '80',
    x = 0.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '79',
    x = 1.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '87',
    x = 2.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '78',
    x = 3.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '77',
    x = 4.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '91',
    x = 5.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '82',
    x = 6.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '75',
    x = 7.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '88',
    x = 8.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '70',
    x = 9.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '77',
    x = 10.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '72',
    x = 11.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '77',
    x = 12.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '67',
    x = 13.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '74',
    x = 14.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '75',
    x = 15.5,
    y = 2.5,
    colour = 'black',
    size = 4
  ) +
  annotate(
    'text',
    label = '87',
    x = 0.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  annotate(
    'text',
    label = '94',
    x = 1.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  annotate(
    'text',
    label = '90',
    x = 2.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  annotate(
    'text',
    label = '97',
    x = 3.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  annotate(
    'text',
    label = '97',
    x = 4.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  annotate(
    'text',
    label = '97',
    x = 5.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  annotate(
    'text',
    label = '90',
    x = 6.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  annotate(
    'text',
    label = '88',
    x = 7.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  annotate(
    'text',
    label = '100',
    x = 8.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  annotate(
    'text',
    label = '85',
    x = 9.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  annotate(
    'text',
    label = '91',
    x = 10.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  annotate(
    'text',
    label = '84',
    x = 11.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  annotate(
    'text',
    label = '87',
    x = 12.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  annotate(
    'text',
    label = '77',
    x = 13.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  annotate(
    'text',
    label = '85',
    x = 14.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  annotate(
    'text',
    label = '78',
    x = 15.5,
    y = 3.5,
    colour = 'grey90',
    size = 4
  ) +
  
  annotate(
    'text',
    label = '95',
    x = 0.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  annotate(
    'text',
    label = '101',
    x = 1.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  annotate(
    'text',
    label = '100',
    x = 2.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  annotate(
    'text',
    label = '108',
    x = 3.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  annotate(
    'text',
    label = '113',
    x = 4.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  annotate(
    'text',
    label = '101',
    x = 5.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  annotate(
    'text',
    label = '104',
    x = 6.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  annotate(
    'text',
    label = '104',
    x = 7.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  annotate(
    'text',
    label = '106',
    x = 8.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  annotate(
    'text',
    label = '104',
    x = 9.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  annotate(
    'text',
    label = '102',
    x = 10.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  annotate(
    'text',
    label = '101',
    x = 11.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  annotate(
    'text',
    label = '102',
    x = 12.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  annotate(
    'text',
    label = '107',
    x = 13.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  annotate(
    'text',
    label = '98',
    x = 14.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  annotate(
    'text',
    label = '108',
    x = 15.5,
    y = 4.5,
    colour = 'grey100',
    size = 4
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.box.margin = margin(c(10,10,10,10)),
    panel.border = element_rect(size = 2, color = 'black'),
    axis.text = element_text(face = 'plain', color = 'black'),
    axis.text.x=element_text(hjust=0, vjust=0, angle=45, face='plain', color='black', size = 12),
    axis.ticks = element_line(size = 1, color = 'black'),
    legend.text = element_text(size = 10),
    legend.title=element_text(size=12, face='plain'),
    legend.title.align = 0,
    axis.title = element_text(size = 12, face = 'plain'),
    plot.title = element_text(hjust = 0.5)
  )

# Close pdf device
dev.off()

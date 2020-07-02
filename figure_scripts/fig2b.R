# Load library
library(ggplot2)

# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dataset
load('reduced_coupling_data.RData')

# Open pdf for plotting
pdf(width = 5.5, height = 4.25, file = 'Fig2b.pdf')

# Draw plot
ggplot(data=geotherms)+
  geom_line(aes(x=X60km, y=Depth), size=0.5, linetype = 1) +
  geom_line(aes(x=X80km, y=Depth), size=0.5, linetype = 2) +
  geom_line(aes(x=X100km, y=Depth), size=0.5, linetype = 5) +
  geom_line(aes(x=X120km, y=Depth), size=0.5, linetype = 4) +
  annotate('text', x = 170, y = 65, hjust = 0.5, label = 'underline(atop(Surface~Heat,Flow~(mW/m^2)))', parse = TRUE) +
  annotate('text', x = 545, y = 65, hjust = 0.5, label = 'underline(atop(Lithospheric,Thickness~(km)))', parse = TRUE) +
  annotate('text', x = 545, y = 77.5, hjust = 0.5, vjust = 1, label = '46\n62\n78\n94') +
  annotate('text', x = 170, y = 77.5, hjust = 0.5, vjust = 1, label = '79\n69\n63\n59') +
  annotate('segment', x = 265, xend = 440, y = 81, yend = 81, size = 0.5, linetype = 1) +
  annotate('segment', x = 265, xend = 440, y = 90, yend = 90, size = 0.5, linetype = 2) +
  annotate('segment', x = 265, xend = 440, y = 98.5, yend = 98.5, size = 0.5, linetype = 4) +
  annotate('segment', x = 265, xend = 440, y = 108, yend = 108, size = 0.5, linetype = 5) +
  theme_bw(base_size=14) +
  theme(panel.border=element_rect(size=2, color='black'),
        axis.text=element_text(face='plain', color='black'),
        axis.ticks=element_line(size=1),
        axis.ticks.length = unit(-0.25, "cm"),
        axis.text.x = element_text(margin = unit(c(0.5, 0, 0, 0), "cm")),
        axis.text.y = element_text(margin = unit(c(0, 0.5, 0, 0), "cm")),
        axis.title=element_text(size=12,face='plain'),
        plot.title = element_text(hjust=0.5)) +
  scale_y_reverse(limits=c(120,0))+
  scale_x_continuous(limits = c(0,1300))+
  ylab('Depth (km)') +
  xlab(expression('Temperature ('*~degree*C*')'))+
  ggtitle('Model Geotherms')

# Close pdf device
dev.off()
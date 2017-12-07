#import library
library(ggplot2)

# creat custom theme
barplot_mapinset_theme <- theme_bw() +
  
  theme(text = element_text(size=10, colour='black'),
        #legend.position="top",
        #legend.direction= 'horizontal',
        legend.key.size = unit(1.25, "mm"),
        legend.key = element_blank(),
        legend.title=element_blank(),
        legend.background = element_blank(),#element_rect(colour = NA),
        legend.text = element_text(size = 5),
        
        axis.line = element_blank(),
        axis.ticks = element_line(colour= "black", size=0.1),
        axis.text.x = element_text(colour='black', size=5), #angle=45, hjust=-.5, vjust=.5, 
        axis.text.y = element_text(colour='black', size=5), 
        strip.text.x = element_text(size = 10, face="bold"),
        strip.background = element_blank(),
        
        
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        #panel.grid.major = element_blank(),
        #panel.border = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.50),
        #panel.margin.x=unit(0.1, "lines"),
        plot.margin = unit( c(0,1,0,0) , "mm"),
        axis.title.y = element_text(margin = margin(r=0)),
        axis.title.x = element_text(margin = margin(t=2)))



# legend.position = c(.02, -.11),
# legend.direction= 'horizontal',
# text = element_text(size=8)
#plot.title = element_text(size = rel(0.8)),
#legend.position="none",
### unused expressions
#panel.grid.major = element_blank(),
#axis.text.x=element_blank(),
#axis.ticks  = element_blank(),
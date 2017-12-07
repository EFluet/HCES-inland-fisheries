
library(ggplot2)

# declare theme elements
custom_factor_line_theme <- theme_bw() +
  
  theme(text = element_text(size=9, colour='black'),
        strip.text.x = element_text(size=10, face='bold'),
        strip.background = element_blank(),

        legend.background = element_blank(), 
        legend.key.size = unit(3, "mm"),
        legend.text = element_text(size = 10),
        legend.title=element_blank(),
        #legend.position = c(0.5, 0.5),
        legend.justification = "left",
        
        axis.line = element_blank(),
        axis.text = element_text(size=9, colour='black'),
        axis.ticks = element_line(colour='black'), 
        
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.35),
        panel.background = element_blank())


#legend.title = element_blank(),
#legend.background = element_rect(colour = NA),
#title.theme = element_blank(),
#legend.key = element_blank(),
#element_rect(colour = "black", size=0.2),
#element_text(size = 8),
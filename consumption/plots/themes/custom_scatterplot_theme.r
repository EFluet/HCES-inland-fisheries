library(ggplot2)

# declare theme elements
custom_scatterplot_theme <- theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=10, colour="black"),
        axis.text = element_text(colour='black', size=10),
        legend.key = element_rect(colour = "white"),
        legend.background = element_rect(colour = "black", size=0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        #legend.title = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank())
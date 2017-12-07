



### Set limit

#lim <- c(-800,800)
lim <- c(-800, 1700)
#lim <- c(0, 1700)



### prep data for bar plots from the ca_ctry_jnd table  ---------------------

ca_ctry_jnd_p <-  ca_ctry_jnd %>% 
  filter(!is.na(prc_dif) & 
           prc_dif < 1000) %>%
  arrange(desc(prc_dif)) %>%
  mutate(Country= paste(Country, year_start, sep=',')) %>%
  mutate(Country= factor(Country, levels = Country[order(sum_catch)]))



# Draw plot for 80% ------------------------------------------------------------

ca_ctry_jnd_t80 <-  ca_ctry_jnd_p %>%
  filter(grp == 'First 80%') %>%
  arrange(desc(prc_dif))

t80_width = nrow(ca_ctry_jnd_t80) / 30

t80 <-  ggplot(ca_ctry_jnd_t80, 
               aes(x= factor(Country),
                   y= prc_dif*100,
                   ymin=prc_dif_lwr*100,
                   ymax=prc_dif_upr*100,
                   fill = continent)) + 
  
  # add to background
  geom_hline(yintercept = 100, colour= 'red') +
  
  # add bars and errorbars
  geom_bar(stat='identity', width=0.77, position='dodge') + 
  geom_errorbar(position='dodge', width=0.77, size=0.3)+#width=1.1*t80_width)+
  
  # Add text
  geom_text(x=nrow(ca_ctry_jnd_t80)/2, 
            y=lim[2]*1.15, angle=-90, label = 'First 80%') + 
  
  # set limits and breaks
  #coord_flip() + ylim(lim[1], lim[2]) +
  ### using the lim inside coord_flip works with setting the breaks
  ### but it does not remove the geometries that exceed the limits
  coord_flip(ylim=c(lim[1], lim[2])) +
  scale_y_continuous(breaks = c(c(0,100), seq(lim[1], lim[2], by = 400))) +
  
  
  ggtitle('') + xlab("") + ylab("") + 
  theme_bw() +
  theme(text = element_text(size=8),
        plot.title = element_text(size = rel(0.8)),
        legend.position="none",
        axis.line = element_line(color = 'black'),
        plot.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        axis.ticks  = element_blank(),
        axis.line   = element_line(colour=NA),
        axis.line.x = element_blank())



# Draw plot for remaining 20% ------------------------------------------------------------

ca_ctry_jnd_p20 <-  ca_ctry_jnd_p %>% 
  filter(grp == 'Remaining 20%') %>%
  arrange(desc(prc_dif))

b20_width = nrow(ca_ctry_jnd_p20) / 30


b20 <-  ggplot(ca_ctry_jnd_p20, 
               aes(x= factor(Country),
                   y= prc_dif*100,
                   ymin=prc_dif_lwr*100,
                   ymax=prc_dif_upr*100,
                   fill = continent)) + 
  
  # add line to background
  geom_hline(yintercept = 100, colour= 'red') +
  
  
  # add bars and errorbars
  geom_bar(stat='identity', width=1.1*b20_width, position='dodge') + 
  geom_errorbar(position='dodge', width=1.1*b20_width, size=0.3)+
  
  # Add text
  geom_text(x=nrow(ca_ctry_jnd_p20)/2, 
            y=lim[2]*1.15, angle=-90, label = 'Remaining 20%') + 
  
  # labels
  ggtitle('') + xlab("") + ylab("Percentage difference to FAO catch") + 
  
  # set limits and breaks
  #coord_flip() + ylim(lim[1], lim[2]) +
  ### using the lim inside coord_flip works with setting the breaks
  ### but it does not remove the geometries that exceed the limits
  coord_flip(ylim=c(lim[1], lim[2])) +
  scale_y_continuous(breaks = c(c(0,100), seq(lim[1], lim[2], by = 400))) +
  
  theme_bw() +
  theme(#legend.position = "bottom",
    legend.position = c(.02, -.095),
    legend.key.size = unit(3, "mm"),
    legend.direction= 'horizontal',
    text = element_text(size=8),
    legend.key = element_rect(colour=NULL),
    legend.title = element_blank(),
    plot.title = element_text(size = rel(0.8)),
    plot.background = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank())




### Arrange figures into a grid  -------------------------------------------------


# set tight margins so plots are close side-by-side
# Dimensions of each margin: t, r, b, l     (To remember order, think trouble).
t80 <- t80 + theme(plot.margin=unit(c(-1, 10, -2, -1), "mm"))
b20 <- b20 + theme(plot.margin=unit(c(-2, 10, 10, -1), "mm"))


# convert plots to ggtable objects
t80 <- ggplot_gtable(ggplot_build(t80))
b20 <- ggplot_gtable(ggplot_build(b20))


# set max width of either plot as the width for both
maxWidth = unit.pmax(t80$widths[2:3], b20$widths[2:3])
t80$widths[2:3] <- maxWidth
b20$widths[2:3] <- maxWidth

t80$layout$clip[t80$layout$name == "panel"] <- "off"
b20$layout$clip[b20$layout$name == "panel"] <- "off"


# arrange plots grob into layout 
p <- arrangeGrob(t80, b20, heights=c(0.7, 1.1+(t80_width/b20_width)))


#p <- arrangeGrob(t80, b20, heights=c(t80_width, p20_width))#, align = 'v')#, widths=c(1,1.45))
#p <- plot_grid(t80, b20, ncol = 1, nrow = 2, align = 'v', heights=c(1,1)) 



# save figure to output file ---------------------------------------------------
ggsave("../output/figures/survey_fao_bargraph_sep80n20_lrglim.png", p, 
       width=250, height=160, dpi=800, units="mm", type = "cairo-png")


dev.off()


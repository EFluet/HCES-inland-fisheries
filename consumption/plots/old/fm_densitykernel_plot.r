
theme_fm_densitykernel <- theme_bw() +
  theme(text = element_text(size=9, colour='black'),
        legend.position=c(0.55, 0.95),
        legend.key.size = unit(3, "mm"),
        legend.key = element_blank(),
        legend.title=element_blank(),
        legend.background = element_blank(),#element_rect(colour = NA),
        axis.line = element_blank(),
        axis.ticks = element_line(colour= "black"),
        axis.text = element_text(colour='black', size=9),
        strip.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.50))


 
labels<-c('','0-20   ','20-40  ','40-60  ','60-80  ',' 80-100 ')
breaks<- seq(0,100,20)



consump_df2 <- consump_df2 %>%
  filter(country_code != "ARM") %>%
  mutate(geo_pos_lbl= ifelse(geo_pos=='Landlocked',
                             paste(geo_pos,' (n=',
                                   sum(consump_df2$geo_pos=='Landlocked'),')',sep=''),
                             geo_pos)) %>%
  mutate(geo_pos_lbl= ifelse(geo_pos=='Coastal',
                             paste(geo_pos,' (n=',
                                   sum(consump_df2$geo_pos=='Coastal'),')',sep=''),
                             geo_pos_lbl))



ggplot() + 
  geom_histogram(data=consump_df2, 
                 aes(x=frac*100, y=..density.., fill=geo_pos_lbl),
                 breaks=breaks, alpha=.3, position="dodge",
                 size= 0.6, colour='white') +
  geom_path(data=lc, aes(x=x*100, y=y/100, colour=geo_pos), size= 0.7) +
  # geom_density(data= consump_df2,
  #              aes(x=frac*100, fill=geo_pos, color=geo_pos_lbl),
  #              size= 0.4, alpha=.2, adjust=3, kernel='gaussian') +
  theme_fm_densitykernel +
  scale_colour_discrete(guide = FALSE) +
  xlab('Percentage of inland products (%)') +
  ylab('Probability density') + 
  scale_x_continuous(breaks=breaks, labels=labels) +
  theme(axis.text.x = element_text(hjust=1.05))




### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/geo_pos_densitykernel_v5.png",
       dpi=600, width=90, height=70, units='mm', type = "cairo-png")

dev.off()



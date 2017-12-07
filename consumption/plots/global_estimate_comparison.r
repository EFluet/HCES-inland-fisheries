
### get theme 
source('./consumption/plots/themes/barplot_catchdiff_theme.r')


df <- read.csv("../data/catch_yield/catch_model_estimate_comparison.csv")


# ~~~ set legend order ----------
lo <- c("Welcomme 2010",  "McIntyre et al. 2016", "Lymer et al. 2016", "Deines et al. 2017",
        "WorldBank / WorldFish 2008", "Household surveys", "Reported to FAO")
df$source <- factor(df$source, levels = lo)


# Create barplot of difference -------------------------------------------------
barplot <- ggplot(df, 
                      aes(x=factor(source), 
                          y=val, 
                          ymin=val_min, 
                          ymax=val_max,
                          fill=water_type)) + 
  geom_bar(stat='identity', width=0.7) + 
  geom_errorbar(size=0.3, width=0.3) +
  
  coord_flip() +
  barplot_catchdiff_theme + 
  
  theme(plot.margin=unit(c(3, 1, 0, -2), "mm"),
        text = element_text(size=9, colour='black'),
        legend.text = element_text(size = 9),
        axis.text.x = element_text(colour='black', size=9),
        axis.text.y = element_text(colour='black', size=9),
        legend.position = c(0.77, 0.775),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(4, "mm"))+
  
  scale_fill_brewer(palette = "Set1") +
  
  #guides(fill = guide_legend(ncol = 1)) +
  # the labels create the slight shift at bottom of plots
  xlab('') + ylab('Inland catch (million) tonnes)')


barplot




### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/global_estimate_comparison.png", barplot,
       dpi=600, width=130, height=135, units='mm')# , type = "cairo-png")

dev.off()

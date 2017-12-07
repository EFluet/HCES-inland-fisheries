
### get theme ------------------------------------------------------------------
source('./consumption/plots/themes/barplot_catchdiff_theme.r')



# Create barplot of difference -------------------------------------------------
dif_barplot<- ggplot(surv_data, 
                   aes(x=factor(country_label), 
                       y=mean_dif, ymin=p025_dif, ymax=p975_dif)) + 
  geom_bar(stat='identity', width=0.6, aes(fill=prc_categ)) + 
  geom_errorbar(size=0.2, width=0.6) +
  geom_hline(yintercept=0, size=0.2, colour='grey15') +
  coord_flip() +
  barplot_catchdiff_theme + 
  theme(plot.margin=unit(c(10, -1, 2, 0), "mm"),
        legend.position = c(0.5, 1.03),
        panel.grid.major.y = element_line(size=0.1, colour='grey65')) +
  guides(fill = guide_legend(ncol = 2)) +
  # the labels create the slight shift at bottom of plots
  xlab('') + ylab('Difference of HCES-estimated \nand FAO inland catch (Million tonnes)') +
  scale_fill_manual(values =  c("Survey < FAO"                 = '#3399ff',
                                "Survey < FAO - within U.I."   = '#ffb3b3',
                                "Survey > FAO"                 = '#ff3333',
                                "Survey > FAO - within U.I."   = '#b3d9ff'))



# plot the flag count ----------------------------------------------------------
flag_ptplot <- ggplot(all_flag_tally_agg, 
                      aes(x=country_label, 
                          y=flag_perc, 
                          colour=TradeflowTradeflow)) + 
  geom_point(aes(shape = TradeflowTradeflow), 
             stroke = 1.0, size=1.8, fill=NA)+ 
  coord_flip() +
  barplot_catchdiff_theme +
  guides(fill = guide_legend(ncol = 2)) +
  
  theme(plot.margin=unit(c(10, 2, 2, -1), "mm"),
        panel.grid.major.y = element_line(size=0.1, colour='grey65'),
        axis.text.y = element_blank(),
        legend.position = c(0.45, 1.03)) +
  guides(shape = guide_legend(ncol = 2)) +
  xlab('') + 
  ylab('Percentage of FAO statistics \n weight estimated (%)') 





# arrange both plots together --------------------------------------------------
p <- grid.arrange(dif_barplot, flag_ptplot, #heights=c(1), 
                  widths=c(0.8 , 0.5), 
                  ncol=2)



### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/barplot_dif_flag_diffshapes_v5.png", p,
       dpi=600, width=210, height=210, units='mm', type = "cairo-png")

dev.off()

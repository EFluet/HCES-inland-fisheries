
### get theme ------------------------------------------------------------------
source('./consumption/plots/themes/barplot_catchdiff_theme.r')


# Source script that tallies FAO flag symbols, and preps the surv_data df
source('./consumption/data_proc/fishstatj_flag_tally/byweight/combined_fao_flag_tally_v3_byweight.r')


### Cut sum_catch into bins  --------------------------------------------------
my_breaks = c(-100, -50, -10, 10, 50, 100, 200, 500, 1000000)
surv_data$mean_dif_perc_cut <- cut(surv_data$mean_dif_perc, breaks = my_breaks,
                                        dig.lab=10)

# replace the categories stings to make them nicer in the legend
surv_data$mean_dif_perc_cut <- gsub("\\(|\\]", "", surv_data$mean_dif_perc_cut)
surv_data$mean_dif_perc_cut <- gsub("\\,", " to ", surv_data$mean_dif_perc_cut)
surv_data <- surv_data %>% mutate(mean_dif_perc_cut=ifelse(mean_dif_perc_cut=="500 to 1000000",
                                                           "over 500",mean_dif_perc_cut))

### Cut the flags with % threshold
my_breaks = c(0, 0.4, 1)
all_flag_tally_agg$flag_perc_cut <- cut(all_flag_tally_agg$flag_perc, 
                                        breaks = my_breaks,
                                        dig.lab=10)

#surv_data$mean_dif_perc_cut <- factor(surv_data$mean_dif_perc_cut)
lengend_order <- rev(c("-100 to -50",  "-50 to -10", "-10 to 10",  "10 to 50",
                   "50 to 100", "100 to 200",  "200 to 500", "over 500"))
surv_data$mean_dif_perc_cut <- factor(surv_data$mean_dif_perc_cut, levels = lengend_order)
levels(surv_data$mean_dif_perc_cut)





# Create barplot of difference -------------------------------------------------
dif_barplot<- ggplot(surv_data, 
                   aes(x=factor(country_label), 
                       y=mean_dif *1000, 
                       ymin=p025_dif *1000, 
                       ymax=p975_dif *1000)) + 
  geom_bar(stat='identity', width=0.7, aes(fill=mean_dif_perc_cut)) + 
  geom_errorbar(size=0.2, width=0.4) +
  geom_hline(yintercept=0, size=0.2, colour='grey15') +

  ### Flag icon for Aquaculture
  geom_point(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Aquaculture' & flag_perc>0.4),
             aes(x=factor(country_label),
                y=rep(-550, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Aquaculture' & flag_perc>0.4)))),
             pch=21, fill='white', size=2.75, stroke = 0.3)+

  geom_text(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Aquaculture' & flag_perc>0.4),
               aes(x=factor(country_label),
                   y=rep(-550, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Aquaculture' & flag_perc>0.4))),
                   label='A'), size=2.25)+

  
  ### Flag icon for Catch
  geom_point(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Catch' & flag_perc>0.4), 
             aes(x=factor(country_label),
                 y=rep(-475, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Catch' & flag_perc>0.4)))),
             pch=21, fill='white', size=2.75, stroke = 0.3)+
  
  geom_text(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Catch' & flag_perc>0.4),
               aes(x=factor(country_label),
                   y=rep(-475, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Catch' & flag_perc>0.4))),
                   label='C'), size=2.25)+

  

  ### Flag icon for Imports
  geom_point(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Import' & flag_perc>0.4),
             aes(x=factor(country_label),
                 y=rep(-400, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Import' & flag_perc>0.4)))),
             pch=21, fill='white', size=2.75, stroke = 0.3)+

  geom_text(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Import' & flag_perc>0.4),
            aes(x=factor(country_label),
                y=rep(-400, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Import' & flag_perc>0.4))),
                label='I'), size=2.25)+



  ### Flag icon for Exports
  geom_point(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Export' & flag_perc>0.4),
             aes(x=factor(country_label),
                 y=rep(-325, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Export' & flag_perc>0.4)))),
             pch=21, fill='white', size=2.75, stroke = 0.3)+

  geom_text(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Export' & flag_perc>0.4),
            aes(x=factor(country_label),
                y=rep(-325, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Export' & flag_perc>0.4))),
                label='E'), size=2.25)+ #fontface = "bold"

  
  coord_flip() +
  barplot_catchdiff_theme + 

  scale_y_continuous(breaks = seq(-500, 1000, 250),
                     sec.axis = sec_axis(~ ., breaks = seq(-500, 1000, 250)))+
  
  theme(plot.margin=unit(c(3, 1, 0, -2), "mm"),
        legend.position = c(0.77, 0.735),
        panel.grid.major.y = element_line(size=0.1, colour='grey65')) +
  guides(fill = guide_legend(ncol = 1)) +
  # the labels create the slight shift at bottom of plots
  xlab('') + ylab('Difference of HCES-estimated \nand FAO inland catch (×1000 tonnes)') +
  scale_fill_manual(values =  c("-100 to -50" = '#1a1aff',
                                "-50 to -10" = '#ccccff',
                                "-10 to 10" = '#ffff99',
                                "10 to 50" = '#ffcccc',
                                "50 to 100" = '#ff6666',
                                "100 to 200" = '#ff0000',
                                "200 to 500" = '#990000',
                                "over 500" = '#330000'),
                    name = "Difference as\npercentage of\nreported statistics")








### Make inset barplot ---------------------------------------------------------
surv_data_forinsetbar<- surv_data %>%
  filter(mean_dif > -0.01 & mean_dif < 0.05)



dif_barplot_inset <- ggplot(surv_data_forinsetbar, 
                     aes(x=factor(country_label), 
                         y=mean_dif *1000, 
                         ymin=p025_dif *1000, 
                         ymax=p975_dif *1000)) + 
  geom_bar(stat='identity', width=0.7, aes(fill=mean_dif_perc_cut)) + 
  geom_errorbar(size=0.2, width=0.4) +
  geom_hline(yintercept=0, size=0.2, colour='grey15') +
  coord_flip() +
  barplot_catchdiff_theme + 
  theme(plot.margin=unit(c(3, 1, 0, -2), "mm"),
        legend.position = "none",
        panel.grid.major.y = element_line(size=0.1, colour='grey65'),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  #guides(fill = guide_legend(ncol = 1)) +
  # the labels create the slight shift at bottom of plots
  xlab('') + ylab('') +
  scale_fill_manual(values =  c("-100 to -50" = '#1a1aff',
                                "-50 to -10" = '#ccccff',
                                "-10 to 10" = '#ffff99',
                                "10 to 50" = '#ffcccc',
                                "50 to 100" = '#ff6666',
                                "100 to 200" = '#ff0000',
                                "200 to 500" = '#990000',
                                "over 500" = '#330000'))
  

# arrange both plots together --------------------------------------------------
# Combine nb.prod map & inset 
g3 = ggplotGrob(dif_barplot_inset)
dif_barplot_inset_winset = dif_barplot + annotation_custom(grob = g3, 
                                                           xmin= 6.8, xmax= 26.61, 
                                                           ymin= 250,  ymax=  1105)


### Make histogram of difference in tonnes
# #hist_dif_tonnes <- 
# ggplot(surv_data)+
#   geom_histogram(aes(mean_dif*1000), breaks= seq(-500, 1000, 100),  color='white')



#dif_barplot_inset_winset
# p <- grid.arrange(dif_barplot, flag_ptplot, #heights=c(1), 
#                   widths=c(0.8 , 0.5), 
#                   ncol=2)



### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/fig2_barplot_dif_wpercdiff_v3.png", dif_barplot_inset_winset,
       dpi=1000, width=90, height=120, units='mm', type = "cairo-png")

dev.off()

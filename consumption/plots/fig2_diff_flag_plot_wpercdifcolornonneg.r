
### get theme 
source('./consumption/plots/themes/barplot_catchdiff_theme.r')


# get tally FAO flag symbols, and preps the surv_data df ---------------------
source('./consumption/data_proc/fishstatj_flag_tally/byweight/combined_fao_flag_tally_v3_byweight.r')


surv_data<- surv_data %>%
  mutate(hces_surv_asprcfao = tr_corr_consump/sum_catch *100) 


# ~~~ Cut ca diff into bins  --------------------------------------------------
#my_breaks = c(-10000, 0, 50, 75, 125, 150, 200, 400, 500000)
my_breaks = c(0, 50, 100, 150, 200, 400, 500000)
surv_data$hces_surv_asprcfao_cut <- cut(surv_data$hces_surv_asprcfao, breaks = my_breaks,
                                        dig.lab=10)

# replace the categories stings to make them nicer in the legend
surv_data$hces_surv_asprcfao_cut <- gsub("\\(|\\]", "", surv_data$hces_surv_asprcfao_cut)
surv_data$hces_surv_asprcfao_cut <- gsub("\\,", " to ", surv_data$hces_surv_asprcfao_cut)
surv_data <- surv_data %>% mutate(hces_surv_asprcfao_cut=ifelse(hces_surv_asprcfao_cut=="400 to 500000",
                                                                "over 400",hces_surv_asprcfao_cut))

# ~~~ set legend order ----------
lengend_order <- rev(c("0 to 50", "50 to 100", "100 to 150", "150 to 200","200 to 400", "over 400"))      
surv_data$hces_surv_asprcfao_cut <- factor(surv_data$hces_surv_asprcfao_cut, levels = lengend_order)
levels(surv_data$hces_surv_asprcfao_cut)


# ~~~ Threshold flags  ----------------------------
all_flag_tally_agg_toplot <- all_flag_tally_agg %>%
  select(country_label, TradeflowTradeflow, flag_perc) %>%
  filter(flag_perc >= 0.4) %>%
  spread(TradeflowTradeflow, flag_perc)


# Create barplot of difference -------------------------------------------------
dif_barplot <- ggplot(surv_data, 
                     aes(x=factor(country_label), 
                         y=mean_dif *1000, 
                         ymin=p025_dif *1000, 
                         ymax=p975_dif *1000)) + 
  geom_bar(stat='identity', width=0.7, aes(fill=hces_surv_asprcfao_cut)) + 
  geom_errorbar(size=0.2, width=0.4) +
  geom_hline(yintercept=0, size=0.2, colour='grey15') +
  
  # # ~~~ Flag icon for Aquaculture ----------------
  # geom_point(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Aquaculture' & flag_perc>0.4),
  #            aes(x=factor(country_label),
  #                y=rep(-550, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Aquaculture' & flag_perc>0.4)))),
  #            pch=21, fill='white', size=2.75, stroke = 0.3)+
  # 
  # geom_text(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Aquaculture' & flag_perc>0.4),
  #           aes(x=factor(country_label),
  #               y=rep(-550, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Aquaculture' & flag_perc>0.4))),
  #               label='A'), size=2.25)+
  # 
  # 
  # # ~~~ Flag icon for Catch --------------
  # geom_point(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Catch' & flag_perc>0.4), 
  #            aes(x=factor(country_label),
  #                y=rep(-475, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Catch' & flag_perc>0.4)))),
  #            pch=21, fill='white', size=2.75, stroke = 0.3)+
  # 
  # geom_text(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Catch' & flag_perc>0.4),
  #           aes(x=factor(country_label),
  #               y=rep(-475, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Catch' & flag_perc>0.4))),
  #               label='C'), size=2.25)+
  # 
  # 
  # 
  # # ~~~ Flag icon for Imports --------
  # geom_point(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Import' & flag_perc>0.4),
  #            aes(x=factor(country_label),
  #                y=rep(-400, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Import' & flag_perc>0.4)))),
  #            pch=21, fill='white', size=2.75, stroke = 0.3)+
  # 
  # geom_text(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Import' & flag_perc>0.4),
  #           aes(x=factor(country_label),
  #               y=rep(-400, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Import' & flag_perc>0.4))),
  #               label='I'), size=2.25)+
  # 
  # 
  # 
  # # ~~~ Flag icon for Exports ------
  # geom_point(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Export' & flag_perc>0.4),
  #            aes(x=factor(country_label),
  #                y=rep(-325, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Export' & flag_perc>0.4)))),
  #            pch=21, fill='white', size=2.75, stroke = 0.3)+
  # 
  # geom_text(data=subset(all_flag_tally_agg, TradeflowTradeflow=='Export' & flag_perc>0.4),
  #           aes(x=factor(country_label),
  #               y=rep(-325, nrow(subset(all_flag_tally_agg, TradeflowTradeflow=='Export' & flag_perc>0.4))),
  #               label='E'), size=2.25)+ #fontface = "bold"
  
  
  coord_flip() +
  barplot_catchdiff_theme + 
  
  scale_y_continuous(breaks = seq(-500, 1000, 250),
                     sec.axis = sec_axis(~ ., breaks = seq(-500, 1000, 250)),
                     limits = c(-600, 1100), 
                     expand = c(0,0))+


  theme(plot.margin=unit(c(3, 1, 0, -2), "mm"),
        #legend.position = c(0.77, 0.735),
        legend.position = c(-0.35, 1.12),
        legend.background = element_blank(),
        #legend.direction="horizontal",
        #legend.box = "horizontal",
        panel.grid.major.y = element_line(size=0.1, colour='grey65', linetype=3)) +
  
  guides(fill = guide_legend(ncol = 1)) +
  # the labels create the slight shift at bottom of plots
  xlab('') + ylab('Difference of HCES-estimated \nand FAO inland catch (×1000 tonnes)') +

  scale_fill_manual(values =
                      c(#"Negative"    = '#999999',
                        "0 to 50"     = '#6666ff',
                        "50 to 100"    = '#ccccff',
                        #"75 to 125"   = '#ffff99',
                        "100 to 150"  = '#ffcccc',
                        "150 to 200"  = '#ff6666',
                        "200 to 400"  = '#ff0000',
                        "over 400"  = '#990000'),
                    name = "Survey estimate\nas percentage\nof reported catch") 




### Inset country diff barplot -------------------------------------------------

# ~~~ subset countries for inset ----------
surv_data_forinsetbar<- surv_data %>%
                        filter(mean_dif > -0.04 & mean_dif < 0.05)

# ~~~ plot inset --------
dif_barplot_inset <- ggplot(surv_data_forinsetbar, 
                            aes(x=factor(country_label), 
                                y=mean_dif *1000, 
                                ymin=p025_dif *1000, 
                                ymax=p975_dif *1000)) + 
  geom_bar(stat='identity', width=0.7, aes(fill=hces_surv_asprcfao_cut)) + 
  geom_errorbar(size=0.2, width=0.4) +
  geom_hline(yintercept=0, size=0.2, colour='grey15') +
  coord_flip() +
  barplot_catchdiff_theme + 
  theme(plot.margin=unit(c(3, 1, 0, -2), "mm"),
        legend.position = "none",
        panel.grid.major.y = element_line(size=0.1, colour='grey65', linetype=3),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  #guides(fill = guide_legend(ncol = 1)) +
  # the labels create the slight shift at bottom of plots
  xlab('') + ylab('') +
  scale_fill_manual(values =
                      c("0 to 50"     = '#6666ff',
                        "50 to 100"   = '#ccccff',
                        "100 to 150"  = '#ffcccc',
                        "150 to 200"  = '#ff6666',
                        "200 to 400"  = '#ff0000',
                        "over 400"    = '#990000'))



# Combine barplot & inset together --------------------------------------------------
g3 = ggplotGrob(dif_barplot_inset)
dif_barplot_inset_winset = dif_barplot + annotation_custom(grob = g3, 
                                                           # x,y coords are flipped
                                                           xmin= 3.4, xmax= 26.9, 
                                                           ymin= 220,  ymax=  1050)



# histogram of abs diff -----------------------------------

### Make numeric breaks for the histogram
my_breaks = seq(-500, 1000, 125)
surv_data$mean_dif_cut <- cut(surv_data$mean_dif*1000, breaks = my_breaks, dig.lab=10)
# replace the categories stings to make them nicer in the legend
surv_data$mean_dif_cut <- gsub("\\(|\\]", "", surv_data$mean_dif_cut)
surv_data$mean_dif_cut <- gsub("\\,", " to ", surv_data$mean_dif_cut)

surv_data_summarize_for_histogram<- surv_data %>%
                                    group_by(hces_surv_asprcfao_cut, mean_dif_cut) %>%
                                    summarize(cut_count = n()) %>%
                                    ungroup 

surv_data_summarize_for_histogram$mean_dif_cut_num <- strsplit(surv_data_summarize_for_histogram$mean_dif_cut, " ")

# write start and end year to columns
surv_data_summarize_for_histogram$mean_dif_cut_num <- as.numeric(lapply(surv_data_summarize_for_histogram$mean_dif_cut_num, 
                                                             function(l) if (length(l[l]) > 0 ) {l[[1]]} else {NA}))
  
surv_data_summarize_for_histogram$mean_dif_cut_num <- surv_data_summarize_for_histogram$mean_dif_cut_num + 62.5




# plot histogram of difference in tonnes -------------------------------------
mean_dif_histogram  <- 
  ggplot(data=surv_data_summarize_for_histogram)+
  geom_bar(aes(x=mean_dif_cut_num, y=cut_count, fill=hces_surv_asprcfao_cut), 
           stat='identity', width=124, color='white', size=0.2) +

  geom_vline(xintercept=0, size=0.2, colour='grey15') +
  barplot_catchdiff_theme + 
  scale_x_continuous(breaks = seq(-500, 1000, 250),
                     labels = seq(-500, 1000, 250),
                     limits = c(-600, 1100), 
                     expand = c(0,0))+
  
  scale_fill_manual(values =
                      c("0 to 50"     = '#6666ff',
                        "50 to 100"   = '#ccccff',
                        "100 to 150"  = '#ffcccc',
                        "150 to 200"  = '#ff6666',
                        "200 to 400"  = '#ff0000',
                        "over 400"    = '#990000')) +

  xlab('') + ylab('Nb. countries')+ #panel.border = element_blank())
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        plot.margin=unit(c(3, 1, -6, 0), "mm"),
        axis.title.y = element_text(margin=margin(0,-155,0,0)))



### Combine maps and plots -------------------------------------
require("gridExtra")
require("grid")


g3 <- ggplotGrob(mean_dif_histogram)
g4 <- ggplotGrob(dif_barplot_inset_winset)

t3 = arrangeGrob(g3, ncol=1)
t4 = arrangeGrob(g4, ncol=1)


fig2_final_plot<- plot_grid(g3, g4, align = "v", nrow = 2, rel_heights = c(1/8, 7/8))
fig2_final_plot


### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/fig2_barplot_dif_wpercdiff_gamfm_whist125bins_v8_noflags.pdf", fig2_final_plot,
       dpi=1000, width=87, height=135, units='mm')# , type = "cairo-png")

dev.off()

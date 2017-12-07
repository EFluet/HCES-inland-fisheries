


### get theme ------------------------------------------------------------------
source('./consumption/plots/themes/barplot_catchdiff_theme.r')





### Run after script:  combined_fao_Flag_tally_v3_byweight.r
source('./consumption/data_proc/fishstatj_flag_tally/byweight/combined_fao_flag_tally_v3_byweight.r')



# Write the table to csv file ------------------------------------------------
# surv_data <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_diff.csv')
# surv_data<- surv_data %>% mutate(country_label= ifelse(is.na(year_end), 
#                              paste(country.x,' (', year_start,')', sep=''),
#                              paste(country.x,' (', year_start, '-', 
#                                    str_sub(year_end,-2,-1),')', sep='')))
surv_data <- surv_data %>%
             mutate(diff_sign = ifelse(mean_dif>0, 'positive', 'negative'))




tot_pos <- with(surv_data, sum(surv_data[diff_sign=='positive', "mean_dif"]))

# Create barplot of difference -------------------------------------------------
#dif_barplot<- 
  
positive_bar_stacked <- ggplot(subset(surv_data, diff_sign=='positive'), 
                   aes(x=diff_sign, 
                       y=mean_dif, ymin=p025_dif, ymax=p975_dif)) + 
  geom_bar(aes(fill=mean_dif_perc), stat='identity', position='stack', width=0.8, color='black')+ 
  #geom_errorbar(size=0.2, width=0.6) +
  geom_hline(yintercept=0, size=0.2, colour='grey15') +

  barplot_catchdiff_stacked_minimalist_theme + 

  guides(fill = guide_legend(ncol = 2)) +
  # the labels create the slight shift at bottom of plots
  xlab('') + ylab('') + #ylab('Difference of HCES-estimated \nand FAO inland catch (Million tonnes)') +

  # coord_cartesian(ylim = c(0, tot_pos)) +
  # scale_y_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, tot_pos))+
  coord_flip() +
  theme(plot.margin=unit(c(10, 4, 2, 0), "mm"),
        legend.position = c(0.5, 1.03),
        panel.grid.major.y = element_line(size=0.1, colour='grey65'))



negative_bar_stacked <- ggplot(subset(surv_data, diff_sign=='negative'), 
       aes(x=diff_sign, 
           y=mean_dif, ymin=p025_dif, ymax=p975_dif)) + 
  geom_bar(aes(fill=mean_dif_perc), stat='identity', 
           position='stack', width=0.8, color='black') + 
  #geom_errorbar(size=0.2, width=0.6) +
  geom_hline(yintercept=0, size=0.2, colour='grey15') +

  barplot_catchdiff_stacked_minimalist_theme + 
  theme(plot.margin=unit(c(10, 4, 2, 0), "mm"),
        legend.position = 'none',
        panel.grid.major.y = element_line(size=0.1, colour='grey65')) +
   #+ scale_y_reverse()
  scale_y_continuous(limits = c(-tot_pos, 0))+
  #coord_cartesian(limits = c(-tot_pos, 0)) +
  coord_flip()
  #scale_y_continuous(expand = c(-tot_pos, 0)) + coord_flip() 




# Try and make a weird combination of plots -------------------------------------
require("gridExtra")
require("grid")



g1 <- ggplotGrob(positive_bar_stacked)
g2 <- ggplotGrob(negative_bar_stacked)




t1 = arrangeGrob(g1,ncol=1, left = textGrob("A", y = 0.95, vjust=1, gp=gpar(fontsize=16)))
t2 = arrangeGrob(g2,ncol=1, left = textGrob("B", y = 0.95, vjust=1, gp=gpar(fontsize=16)))

final = arrangeGrob(t1,t2, ncol=1)
final_plot <- grid.arrange(final)







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

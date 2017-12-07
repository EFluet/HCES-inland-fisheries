# MAKE SCATTERPLOT OF MODEL PREDICTIONS %DIFF AND CATCH



surv_data_sel_forplot <- surv_data_sel %>%
                         filter(psmulti < 1500)



# scatterplot of perc diff -------------------------------
source('./consumption/plots/themes/custom_scatterplot_theme.r')

pred_percdiff_scatterplot <- ggplot() +
  
  # 1:1 line
  geom_abline(slope=1, intercept=0, color='grey70', size=0.2) +
  
  # points
  geom_point(data=surv_data_sel_forplot,
             aes(x=hces_surv_asprcfao, y=psmulti), color='dodgerblue', alpha=0.5) +   
  #, size=sum_catch
  
  # add labels
  geom_text_repel(data= subset(surv_data_sel_forplot, 
                               hces_surv_asprcfao/psmulti > 5 | 
                                 hces_surv_asprcfao/psmulti < 0.2 |
                                 psmulti > 500),
                  
                  aes(label=country_label.x, x=hces_surv_asprcfao, y=psmulti), 
                  size = 3, 
                  colour='gray15', 
                  segment.size = 0.25, 
                  segment.color='gray55',
                  force = 8,
                  box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines')) +
  
  #xlim(0, 1350) + ylim(0, 1350) +
  xlab('Ratio of survey-estimated over reported catch') +
  ylab('Predicted ratio') +
  
  #scale_x_log10() + scale_y_log10() +
  scale_x_continuous(expand=c(0, 0), breaks=c(0, 250, 500, 1000, 1500), limits=c(0,1500)) +  
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 250, 500, 1000, 1500), limits=c(0,1500)) +
  
  custom_scatterplot_theme +
  theme(axis.line = element_line(colour = "black"),
        legend.position = 'none', #c(0.115, .83), 
        text = element_text(size=9),
        legend.key = element_rect(colour = "white"),
        legend.background = element_rect(colour = "black", size=0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank())


# scatter of catch diff w/ pred-------------
catch_comp_scatter <- 
  ggplot() +
  
  geom_abline(slope=1, intercept=0, color='grey70', size=0.2) +
  
  geom_point(data=surv_data_sel_forplot,
             aes(x=tr_corr_consump, y=psmulti_catch), color='dodgerblue', alpha=0.5) +   #, size=psmulti_catch
  
  # add labels
  geom_text_repel(data= subset(surv_data_sel_forplot,
                               tr_corr_consump/psmulti_catch > 10 |
                               tr_corr_consump/psmulti_catch < 0.1 |
                               psmulti_catch > 0.3),
                               
                  
                  aes(label=country_label.x, x=tr_corr_consump, y=psmulti_catch), 
                  size = 3, 
                  colour='gray15', 
                  segment.size = 0.25, 
                  segment.color='gray55',
                  force = 8,
                  box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines')) +
  
  custom_scatterplot_theme +
  
  xlab('Survey estimated catch') +
  ylab('Catch from predicted ratio') +
  
  #scale_x_continuous(breaks=c(0, 50, 100, 200, 500, 1000, 1500)) +  
  #scale_y_continuous(breaks=c(0, 50, 100, 200, 500, 1000, 1500)) +
  
  custom_scatterplot_theme +
  theme(axis.line = element_line(colour = "black"),
        legend.position = 'none', #c(0.115, .83), 
        text = element_text(size=9),
        legend.key = element_rect(colour = "white"),
        legend.background = element_rect(colour = "black", size=0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank())





### Combine maps and plots -------------------------------------
require("gridExtra")
require("grid")

g1 <- ggplotGrob(pred_percdiff_scatterplot)
g2 <- ggplotGrob(catch_comp_scatter)

final = arrangeGrob(g1,g2, ncol=2)
final_plot <- grid.arrange(final)

# save plot 
ggsave("../output/figures/fig_scatter_dif_predict_v4.png", final_plot,
       dpi=800, width=187, height=120, units='mm', type = "cairo-png")

dev.off()




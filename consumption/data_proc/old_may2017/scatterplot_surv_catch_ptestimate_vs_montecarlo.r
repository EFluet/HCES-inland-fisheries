### Read the data consumption table ----------------------------------------------
consump_nat_agg_df_ptestim <- read.csv('../output/consumption/hh_survey_consump_nat_agg_prod_fw_uncert_tradecorr_tr_fwae_prepforplotting.csv', stringsAsFactors=F)


consump_nat_agg_df_ptestim <- read.csv('../output/consumption/hh_survey_consump_nat_agg_prod_fw_uncert_tradecorr_tr_fwae.csv', stringsAsFactors=F)


joined <- left_join(consump_nat_agg_df_ptestim, consump_nat_agg_df, by=c('country_code'))



ggplot(joined,
       aes(xmin=cons_miltonspyr_fm_min_fwaes_notr,
           x=cons_miltonspyr_fm_mean_fwaes_notr,
           xmax=cons_miltonspyr_fm_max_fwaes_notr,
           ymin=surv_catch_corr_2.5,
           y=surv_catch_corr_mean,
           ymax=surv_catch_corr_97.5)) + 
  
  # add a 1:1 line
  geom_abline(intersept=0, slope=1, colour='red') +
  # add points, x and y errorbars
  geom_point(size=0.25)+ geom_errorbar(size=0.15) +  geom_errorbarh(size=0.15) +

  theme_classic() +
  # add labels
  geom_text_repel(aes(label=country), 
                  size = 2.25, 
                  colour='gray55', 
                  segment.size = 0.25, 
                  segment.color='gray55',
                  force = 8,
                  box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines')) +
  
  xlab('survey catch from point estimate method ~Oct 2016 \n (millions of tons)') +
  ylab('survey catch from monte carlo simulation method ~Jan 2017  \n (millions of tons)')



### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/scatterplot_surveycatch_ptesimtate_vs_montecarlo.png",
       dpi=300, width=210, height=180, units='mm', type = "cairo-png")

dev.off()
  
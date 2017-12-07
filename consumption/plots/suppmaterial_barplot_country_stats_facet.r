
# source the plot theme
source('./consumption/plots/themes/barplot_catchdiff_theme.r')


# read df data from file 
f<-'../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm.csv'
consump_nat_agg_df_mcperc<- read.csv(f, stringsAsFactors = FALSE)
rm(f)

# remove uncertainty rows from df
consump_nat_agg_df_mcperc_vals <- consump_nat_agg_df_mcperc %>%
      filter(datatype != 'uncert_fwae_ur',
             datatype != 'uncert_prodfm_ur',
             datatype != 'uncert_fwae_matched_mc',
             datatype != 'uncert_fwae_assumed_mc') %>%

      # rename labels
      mutate(datatype=ifelse(datatype=="cons_pretrcorr","HCES consumption (FWAE)",datatype),
             datatype=ifelse(datatype=="tr_corr_consump","HCES production (FWAE & trade)",datatype),
             datatype=ifelse(datatype=="import","Freshwater Import",datatype),
             datatype=ifelse(datatype=="export","Freshwater Export",datatype),
             datatype=ifelse(datatype=="sum_catch","Inland Catch", datatype),
             datatype=ifelse(datatype=="sum_aquacul","Freshwater aquaculture",datatype)) 



factor_levels_ordered <- c("Inland Catch", "HCES production (FWAE & trade)", 
                           "Freshwater aquaculture", "Freshwater Import",  "Freshwater Export",
                           "HCES consumption (FWAE)")


consump_nat_agg_df_mcperc_vals$datatype <- factor(consump_nat_agg_df_mcperc_vals$datatype, 
                                   levels = factor(factor_levels_ordered))




# create bar plot --------------------------------------------------------------
ggplot(consump_nat_agg_df_mcperc_vals, 
       aes(x = datatype,
           y = mean,
           ymin = p025,
           ymax = p975)) +
  
  geom_bar(aes(fill=datatype), stat="identity", position=position_dodge(), width=0.7) +
  geom_errorbar(position='dodge', width=0.7, size=0.4, colour='black') +
  geom_hline(yintercept=0, size=0.3, colour="grey") +
  
  coord_flip() +
  
  facet_wrap(~ country.x, scales='free_x', ncol=7) +
  
  xlab("") +  ylab("Catch (million tons)") + 
  
  barplot_catchdiff_theme +
  scale_y_continuous(breaks= pretty_breaks()) +
  theme(legend.position = "bottom", text = element_text(size=10),
        plot.title = element_text(size = rel(0.8))) +
  

  scale_fill_manual(values =   
                      c("Inland Catch"                        = 'purple2', 
                        "HCES consumption (FWAE)"             = 'navyblue',
                        "Freshwater aquaculture"              = 'coral1',
                        "Freshwater Import"                   = 'coral1',
                        "Freshwater Export"                   = 'springgreen4',
                        "HCES production (FWAE & trade)"     = 'dodgerblue4'))



### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/barplot_mc_consump_tr_country_facet_v5.png",
       dpi=600, width=300, height=300, units='mm', type = "cairo-png")


dev.off()




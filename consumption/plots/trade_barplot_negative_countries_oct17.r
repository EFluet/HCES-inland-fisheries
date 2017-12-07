
# create bar plot --------------------------------------------------------------
ggplot(subset(consump_nat_agg_df_mcperc_vals, country_code %in% c('CHN', 'IND', 'VNM', 'MEX')),  
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
  theme(legend.position = "none", #"bottom", text = element_text(size=10),
        plot.title = element_text(size = rel(0.8)),
        plot.margin = margin(1,1,1,1)) +
  
  
  scale_fill_manual(values =   
                      c("Inland Catch"                        = 'purple2', 
                        "HCES consumption (FWAE)"             = 'navyblue',
                        "Freshwater aquaculture"              = 'coral1',
                        "Freshwater Import"                   = 'coral1',
                        "Freshwater Export"                   = 'springgreen4',
                        "HCES production (FWAE & trade)"     = 'dodgerblue4'))



### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/barplot_mc_consump_tr_country_facet_v5_onlyneg.png",
       dpi=600, width=140, height=60, units='mm', type = "cairo-png")
dev.off()

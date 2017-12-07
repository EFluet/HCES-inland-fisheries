library(scales)

# source the plot theme
source('./consumption/plots/themes/barplot_catchdiff_theme.r')


# read the datafile
filename <- '../output/consumption/hh_survey_consump_nat_agg_prod_fw_mc.csv'
df <- read.csv(filename, stringsAsFactors = FALSE)
rm(filename)


# calc relative uncert per country  ----------------------------------------------
sel_datatype <- c('uncert_prodfm_ur', 'uncert_fwae_assumed_mc', 'uncert_fwae_matched_mc')

df <- df %>%
      group_by(country.x) %>%
      filter(datatype %in% sel_datatype,
             !is.na(p975)) %>%
      mutate(rng95 = p975 - p025) %>%
      mutate(rng95rel = (p975 - p025)/sum(rng95)) %>% 
      #mutate(sum_rng95 = sum(rng95)) %>%
      mutate(rng95rel = ifelse(rng95==sum(rng95), 1, rng95rel)) %>%
      mutate(rng95rel = rng95rel * 100) %>%
      ungroup %>%
      left_join(., large_prod_cntry_list, by='country.x')




# Format the data to long format and sort it for plotting ----------------------
# modify data forp
df_forplot <- df %>%
  filter(datatype %in% c('sum_catch','tr_corr_consump')) %>%
  select(country.x, p025, mean, p975, datatype) %>%
  left_join(., large_prod_cntry_list, by='country.x') %>%
  arrange(datatype, mean) %>% 
  mutate(datatype= ifelse(datatype=='sum_catch','FAO Catch (FishStatJ)', 'HCES estimated catch'))





# create bar plot or rel uncertainty source ------------------------------------

ggplot(df, 
       aes(x = country.x,
           y = rng95rel,
           fill = datatype)) +
  
  geom_bar(stat="identity", width=0.7) +
  coord_flip() +

  xlab("") +  ylab("Contribution to the survey catch uncertainty") + 
  
  barplot_catchdiff_theme +
  #scale_fill_grey() +
  scale_y_continuous(breaks= pretty_breaks(4)) +
  theme(legend.position = "bottom", text = element_text(size=10),
        plot.title = element_text(size = rel(0.8)))


### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/barplot_mc_uncert_contribution_v3.png",
       dpi=600, width=90, height=140, units='mm', type = "cairo-png")

dev.off()





# # remove uncertainty rows from df ----------------------------------------------
# consump_nat_agg_df_mcperc_uncert <- consump_nat_agg_df_mcperc %>%
#                                     filter(datatype %in% c('uncert_fwae_ur','uncert_prodfm_ur',
#                                                            'tr_corr_consump','cons_pretrcorr'))
# 
# # create bar plot
# ggplot(consump_nat_agg_df_mcperc_uncert, 
#        aes(x = datatype,
#            y = p975 - p025)) +
#   
#   geom_bar(stat="identity", position=position_dodge(), width=0.7, fill='grey75') +
#   #geom_errorbar(position='dodge', width=0.7, size=0.3, colour='grey10') +
#   coord_flip() +
#   
#   facet_wrap(~ country.x, scales='free_x', ncol=5) +
#   
#   xlab("") +  ylab("Catch (million tons)") + 
#   
#   barplot_catchdiff_theme +
#   #scale_fill_grey() +
#   scale_y_continuous(breaks= pretty_breaks()) +
#   theme(legend.position = "bottom", text = element_text(size=10),
#         plot.title = element_text(size = rel(0.8)))
# 
# ### save plot ------------------------------------------------------------------
# ggsave("../Output/Figures/barplot_mc_uncert_country_facet_v2.png",
#        dpi=600, width=210, height=250, units='mm', type = "cairo-png")
# 
# dev.off()


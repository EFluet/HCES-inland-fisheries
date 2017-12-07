

### read consumption data file ---------------------------------------------------
# used to selct the
consump_nat_agg_dif <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm.csv', 
                                stringsAsFactors=F)


#  make unique list of country and year of surveys 
surv_cntry_yr <- unique(consump_nat_agg_dif[c('country_code','year_start')]) 


### source scripts that count the number of flags
source('./consumption/data_proc/fishstatj_flag_tally/byweight/aq_fao_flag_tally_byweight.r')
source('./consumption/data_proc/fishstatj_flag_tally/byweight/tr_fao_flag_tally_byweight.r')
source('./consumption/data_proc/fishstatj_flag_tally/byweight/ca_fao_flag_tally_byweight.r')


# add row to aquaculture flag (because tr has one identifying import/export)
aq_flag_tally_agg$TradeflowTradeflow <- 'Aquaculture'
ca_flag_tally_agg$TradeflowTradeflow <- 'Catch'


# combine the flag df from aq and tr
all_flag_tally_agg <- rbind(aq_flag_tally_agg, tr_flag_tally_agg) %>%
                      rbind(. , ca_flag_tally_agg) %>%  #%>% select(-one_of('flag','not_flag'))
                      # Cut the flags with % threshold
                      #mutate(flag_perc_cut = ifelse(flag_perc >0.4, 1, 0))
                      right_join(., surv_cntry_yr, 
                                 by = c("country_code"="country_code","year"="year_start")) %>%
  
                      dplyr::select(country_code, TradeflowTradeflow, flag_perc) %>%
                      spread(TradeflowTradeflow, flag_perc)   
                      #%>% select(-Reexport)

all_flag_tally_agg[is.na(all_flag_tally_agg)] <- 1

all_flag_tally_agg <- all_flag_tally_agg %>%
            gather(TradeflowTradeflow, flag_perc, Aquaculture:Import)


# remove parts of the rbind
rm(consump_nat_agg_dif, surv_cntry_yr, tr_flag_tally_agg, aq_flag_tally_agg, ca_flag_tally_agg)


### Write the table to csv file
write.csv(all_flag_tally_agg, '../output/consumption/all_flag_tally_agg.csv')





### convert the data table to factor for Figure 2 ------------------------------

# Read survey to select list 
surv_data <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv', 
                      stringsAsFactors = FALSE)





#~~~ rename DRC and PNG to shorten names
surv_data <- surv_data %>%
  mutate(country_label= as.character(country_label)) %>%
  mutate(country_label= ifelse(country_label=="Congo Dem Rep, 2004-05", 'D.R.Congo, 2004-05', country_label),
         country_label= ifelse(country_label=="PapuaNewGuinea, 2001-06", "Papua N.G., 2001-06", country_label),
         country_label= ifelse(country_label=="Cote dIvoire, 2002", "Ivory Coast, 2002", country_label),
         country_label= ifelse(country_label=="Sudan former, 2009", "Sudan (former), 2009", country_label)) %>%
  mutate(country_label= as.factor(country_label))




# Sort countries by the mean difference, for order on the plot
surv_data <- surv_data %>% 
  filter(tr_corr_consump >0) %>% 
  arrange(mean_dif) %>%
  mutate(country_label= as.character(country_label))
surv_data$country_label <- factor(surv_data$country_label, levels=unique(surv_data$country_label))


all_flag_tally_agg <- all_flag_tally_agg %>% 
  left_join(., surv_data, by='country_code') %>% 
  filter(tr_corr_consump >0) %>% 
  arrange(mean_dif)

all_flag_tally_agg$country_label <- as.character(all_flag_tally_agg$country_label)
all_flag_tally_agg$country_label <- factor(all_flag_tally_agg$country_label, levels=unique(all_flag_tally_agg$country_label))




# plot the difference and flags -----------------------------
#source('./consumption/plots/diff_flag_plot_wpercdifcolor.r')

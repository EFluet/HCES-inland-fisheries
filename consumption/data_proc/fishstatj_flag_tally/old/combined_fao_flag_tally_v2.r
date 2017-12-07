
### set working directory --------------------------------------------
setwd("C:/Users/efluet/Dropbox/chap5_global_inland_fish_catch/scripts")


### read consumption data file ---------------------------------------------------
# used to selct the
consump_nat_agg_dif <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm.csv', stringsAsFactors=F)

#  make unique list of country and year of surveys 
surv_cntry_yr <- unique(consump_nat_agg_dif[c('country_code','year_start')]) 


### source scripts that count the number of flags
source('./consumption/data_proc/fishstatj_flag_tally/aq_fao_flag_tally.r')
source('./consumption/data_proc/fishstatj_flag_tally/tr_fao_flag_tally.r')
source('./consumption/data_proc/fishstatj_flag_tally/ca_fao_flag_tally.r')
rm(aq_flag_tally, tr, tr_inl_sel, tr_flag_tally, ca_flag_tally)


# add row to aquaculture flag (because tr has one identifying import/export)
aq_flag_tally_agg$TradeflowTradeflow <- 'Aquaculture'
ca_flag_tally_agg$TradeflowTradeflow <- 'Catch'


# combine the flag df from aq and tr
all_flag_tally_agg <- rbind(aq_flag_tally_agg, tr_flag_tally_agg) %>%
                      rbind(. , ca_flag_tally_agg) %>%
                      select(-one_of('flag','not_flag')) 


###  Replace missing flag entries (meaning that there was no entry )
all_flag_tally_agg <- all_flag_tally_agg %>%
                       spread(TradeflowTradeflow, flag_perc) %>%
                       select(-Reexport) 
all_flag_tally_agg[is.na(all_flag_tally_agg)] <- 1

all_flag_tally_agg <- all_flag_tally_agg %>%
                      gather(TradeflowTradeflow, flag_perc, Aquaculture:Import)


# remove parts of the rbind
rm(consump_nat_agg_dif, surv_cntry_yr, tr_flag_tally_agg, aq_flag_tally_agg, ca_flag_tally_agg)



### READ SURVEY DATA -------------------------------------------------------
surv_data <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv', 
                      stringsAsFactors = FALSE)


# sort labels last so it shows right on plot
surv_data <- surv_data %>% 
  arrange(mean_dif) %>%
  mutate(country_label= factor(surv_data$country_label,
                               levels = factor(surv_data$country_label))) %>%
  arrange(mean_dif) 




# prep data for the flag percentage --------------------------------------------
all_flag_tally_agg <- all_flag_tally_agg %>%
                      left_join(., surv_data, by='country_code') %>%
                      arrange(mean_dif)




# plot the difference and flags -----------------------------
source('./consumption/plots/diff_flag_plot.r')

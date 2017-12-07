


### read consumption data file ---------------------------------------------------
# used to selct the
consump_nat_agg_dif <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_diff.csv', 
                                stringsAsFactors=F)

#  make unique list of country and year of surveys 
surv_cntry_yr <- unique(consump_nat_agg_dif[c('country_code','year_start')]) 


# get flags for each year ---------------------------------------- 
source('./consumption/data_proc/fishstatj_flag_tally/byweight/ca_fao_flag_tally_byweight.r')
source('./consumption/data_proc/fishstatj_flag_tally/byweight/aq_fao_flag_tally_byweight.r')
source('./consumption/data_proc/fishstatj_flag_tally/byweight/tr_fao_flag_tally_byweight.r')

rm(aq, ca, tr, aq_flag_tally_agg, tr_flag_tally_agg, ca_flag_tally_agg)




# add row to aquaculture flag (because tr has one identifying import/export)
aq_inl_sel <- aq_inl_sel %>%
              mutate(TradeflowTradeflow = 'Aquaculture') %>%
              select(country_code, year, num_val, flag, TradeflowTradeflow)



ca_inl_sel <- ca_inl_sel %>%
  mutate(TradeflowTradeflow = 'Catch') %>%
  select(country_code, year, num_val, flag, TradeflowTradeflow)

tr_inl_sel <- tr_inl_sel %>%
  #mutate(TradeflowTradeflow = 'Catch') %>%
  select(country_code, year, num_val, flag, TradeflowTradeflow)



# combine the flag df from aq and tr
all_flag_tally_agg <- rbind(ca_inl_sel, tr_inl_sel) %>%
                      rbind(. , aq_inl_sel) 

all_flag_tally_agg <- all_flag_tally_agg %>%
                      filter(country_code %in% surv_cntry_yr$country_code)


all_flag_tally_agg2 <- all_flag_tally_agg %>%
                      unite(TradeflowTradeflow_flag, TradeflowTradeflow, flag) %>%
                      group_by( year, TradeflowTradeflow_flag) %>%
                      summarize(num_val = sum(num_val)) %>%
                      spread(TradeflowTradeflow_flag, num_val, fill=0) %>%
                      mutate(Aquaculture_perc_flag = ifelse(is.na(Aquaculture_flag/(Aquaculture_flag+ Aquaculture_not_flag)), 
                                                            0, 
                                                            Aquaculture_flag/(Aquaculture_flag+ Aquaculture_not_flag)),
                             
                             Catch_perc_flag = ifelse(is.na(Catch_flag/(Catch_flag + Catch_not_flag)), 
                                                            0, 
                                                      Catch_flag/(Catch_flag+ Catch_not_flag)),
                             
                             Import_perc_flag = ifelse(is.na(Import_flag/(Import_flag + Import_not_flag)), 
                                                      0, 
                                                      Import_flag/(Import_flag + Import_not_flag)),
                             
                             Export_perc_flag = ifelse(is.na(Export_flag/(Export_flag + Export_not_flag)), 
                                                       0, 
                                                       Export_flag/(Export_flag + Export_not_flag)))
                             




ggplot(all_flag_tally_agg2) +
  geom_line(aes(x=year, y=rollmean(Catch_perc_flag, 3, fill = list(NA, NULL, NA))), color='red') +
  geom_line(aes(x=year, y=Aquaculture_perc_flag), color='blue')+
  geom_line(aes(x=year, y=Export_perc_flag), color='green') +
  geom_line(aes(x=year, y=Import_perc_flag), color='orange')





ggplot(all_flag_tally_agg2) +
  geom_line(aes(x=year, y=rollmean(Catch_perc_flag, 5, fill = list(NA, NULL, NA))), color='red') +
  geom_line(aes(x=year, y=rollmean(Aquaculture_perc_flag, 5, fill = list(NA, NULL, NA))), color='blue')+
  geom_line(aes(x=year, y=rollmean(Import_perc_flag, 5, fill = list(NA, NULL, NA))), color='green') +
  geom_line(aes(x=year, y=rollmean(Export_perc_flag, 5, fill = list(NA, NULL, NA))), color='orange')






### *** save figure to file --------------------------------------------------------
ggsave('../output/figures/tot_flag_timeline.png', 
       width=90, height=90, dpi=800, units="mm", type = "cairo-png")
dev.off()





# Join imports, exports, aquaculture and catch to consumption data -------------
source('./consumption/data_proc/tr_commodity_fwae_conv_mc_v3.r')#, print.eval = TRUE)
source('./fishstatj_data/read_nat_aquaculture_data.R', print.eval = TRUE)
source('./fishstatj_data/read_nat_catch_data.R', print.eval = TRUE)
rm(aq, aq_inl, ca)


# filter aq and ca  to only freshwater catch
aq_sum_bycountry <- aq_sum_bycountry %>% 
  filter(source == 'Inland') %>% ungroup() %>% select(-c(source, type))
ca_sum_bycountry <- ca_sum_bycountry %>% 
  filter(source == 'Inland') %>% ungroup() %>% select(-c(source, type))








# aq_flag_tally_agg <- aq_flag_tally_agg %>% 
#   mutate(TradeflowTradeflow <- 'Aquaculture') %>%
#   mutate(aq_flag_tally_perc = flag_perc) %>%
#   select(country_code, year, aq_flag_tally_perc)
# 
# ca_flag_tally_agg <- ca_flag_tally_agg %>% 
#   mutate(TradeflowTradeflow <- 'Catch') %>%
#   mutate(ca_flag_tally_perc = flag_perc) %>%
#   select(country_code, year, ca_flag_tally_perc)
# 
# 
# tr_flag_tally_agg <- tr_flag_tally_agg %>%
#   spread()
#   mutate(ca_flag_tally_perc = flag_perc) %>%
#   select(country_code, year, ca_flag_tally_perc)

# combine the flag df from aq and tr
all_flag_tally_agg <- rbind(aq_flag_tally_agg, tr_flag_tally_agg) %>%
  rbind(. , ca_flag_tally_agg) %>%  #%>% select(-one_of('flag','not_flag'))
  # Cut the flags with % threshold
  #mutate(flag_perc_cut = ifelse(flag_perc >0.4, 1, 0))
  right_join(., surv_cntry_yr, 
             by = c("country_code"="country_code")) 


  select(country_code, TradeflowTradeflow, flag_perc) %>%
  spread(TradeflowTradeflow, flag_perc)   
#%>% select(-Reexport)

all_flag_tally_agg[is.na(all_flag_tally_agg)] <- 1

all_flag_tally_agg <- all_flag_tally_agg %>%
  gather(TradeflowTradeflow, flag_perc, Aquaculture:Import)


# remove parts of the rbind
rm(consump_nat_agg_dif, surv_cntry_yr, tr_flag_tally_agg, aq_flag_tally_agg, ca_flag_tally_agg)


### Write the table to csv file
write.csv(all_flag_tally_agg, '../output/consumption/all_flag_tally_agg.csv')





# read and format FW trade table
source('./fishstatj_data/read_nat_trade_data_fw.R', print.eval = TRUE)
rm(tr_sum_bycountry)


### read consumption data file ---------------------------------------------------
path <- "../output/consumption/prod_coded_n_agg/hh_survey_consump_nat_agg_prod_fw_uncert_tradecorr_diff.csv"
consump_nat_agg_dif <- read.csv(path, stringsAsFactors=F)

#  make unique list of country and year of surveys 
surv_cntry_yr <- unique(consump_nat_agg_dif[c('country_code','year_start')]) 


# select tr commodities found in survey countries-years 
tr_inl_sel <- left_join(surv_cntry_yr, tr, by = c("country_code"="country_code","year_start"="year")) %>%
  # remove production entries 
  filter(TradeflowTradeflow != 'Production')


rm(path, tr, consump_nat_agg_dif, surv_cntry_yr)






### convert product weight to live weights  -----------------------------------

# source function from script
source('./consumption/data_proc/fcn/fcn_fwae_conv.r')

# add converted factor to 
prod_conv_factors <- fwae_conversion(tr_inl_sel$CommodityCommodity)

tr_inl_sel_fwae <- cbind(tr_inl_sel, prod_conv_factors) %>%
  select(-one_of('prod_name', 'prod_name_mod'))




### sum by country  -----------------------------------------------------

# sum production by area, country and year

tr_fwae_sum_bycountry <- tr_inl_sel_fwae %>%
  
  # calculate min,mean,max 
                      mutate(tr_fwae_min = trade*min_conv_fac,
                             tr_fwae_mean= trade*mean_conv_fac,
                             tr_fwae_max = trade*max_conv_fac) %>%
                      select(-one_of('CountryCountry', 'min_conv_fac', 'mean_conv_fac', 'max_conv_fac')) %>%
                      filter(TradeflowTradeflow == "Import" | TradeflowTradeflow == "Export") %>%
                      group_by(country_code, year_start, TradeflowTradeflow) %>%
                      #summarise(sum_bycountry = sum(trade, na.rm=TRUE)) %>%
                      summarise(sum_tr_fwae_min = sum(tr_fwae_min, na.rm=T),
                                sum_tr_fwae_mean= sum(tr_fwae_mean,na.rm=T),
                                sum_tr_fwae_max = sum(tr_fwae_max, na.rm=T)) %>%
                      ungroup() %>%
                      filter(!is.na(year_start)) %>%
                      mutate(source = "All") %>%
                      
                      # this part is a clever way to spread multiple columns
                      gather(key, value, sum_tr_fwae_min:sum_tr_fwae_max) %>%
                      unite( key2, TradeflowTradeflow, key, sep='.') %>%
                      spread(key2, value) 
                      



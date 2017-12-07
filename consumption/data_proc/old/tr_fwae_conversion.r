

# read  data file --------------------------------------------------------


# read trade data file
source('./fishstatj_data/read_nat_trade_data_fw.R', print.eval = TRUE)
# delete the summed trade (because its weights are unconverted)
rm(tr_sum_bycountry)

# read consumption data file
consump_nat_agg_df <- read.csv('../output/consumption/hh_survey_consump_nat_agg_prod_fw_uncert_tradecorr.csv',
                               stringsAsFactors=F)




### subset trade to surveys -----------------------------------------------------
#  make unique list of country and year of surveys 
consump_prods_df_unique <- unique(consump_nat_agg_df[c('country_code','year_start')])

# select tr commodities found in survey countries-years  
tr_inl_sel <- left_join(consump_prods_df_unique, tr,
                        by = c("country_code"="country_code","year_start"="year")) %>%
  filter(TradeflowTradeflow != 'Production')





prod_list <- as.character(tr_inl_sel$CommodityCommodity)



# SYMBOLS USED
# ... Data not available; unobtainable; data not separately available but included in another category
# - Nil or zero
# 0 More than zero but less than half the unit used
# nei Not elsewhere included
# F FAO estimate from available sources of information 
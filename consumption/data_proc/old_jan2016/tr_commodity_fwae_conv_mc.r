


# read and format FW trade table
source('./fishstatj_data/read_nat_trade_data_fw.R', print.eval = TRUE)
rm(tr_sum_bycountry)


### filter tr data to only survey countries  -----------------------------

#  make unique list of country and year of surveys 
surv_uniq_cntry_yr <-  consump_df %>%
                  select(country_code, year_start, year_end) %>%
                  distinct() %>%
                  gather(year_type, year_val, -country_code) %>%
                  filter(!is.na(year_val))


# select data from year closes to survey
# if survey over 2 years, use average
tr_j <- inner_join(tr, surv_uniq_cntry_yr, by='country_code') %>%
        mutate(yr_diff = year-year_val) %>%
        group_by(country_code) %>%
        filter(yr_diff == min(abs(yr_diff))) %>%
        ungroup %>%
        # this averaging assumes that there is no tie between two equally distant year for a country
        # with the exception
        group_by(country_code, CommodityCommodity, TradeflowTradeflow) %>%
        mutate(trade= mean(trade)) %>%
        ungroup %>%
        select(-one_of(c('year', 'year_type', 'year_val', 'yr_diff'))) %>%
        distinct(country_code, CommodityCommodity, TradeflowTradeflow) %>%
        filter(TradeflowTradeflow != 'Production')





# select tr commodities found in survey countries-years 
tr_inl_sel <- left_join(surv_uniq_cntry_yr, tr, by = c("country_code"="country_code","year_start"="year")) %>%
  # remove production entries 
  filter(TradeflowTradeflow != 'Production')

# delete unneeded objects
rm(tr, uniq_cntry_yr, surv_uniq_cntry_yr)


### match commodities with FWAE conversion factors -----------------------------

# source function from script
source('./consumption/data_proc/fcn/fcn_fwae_conv.r')

# add converted factor to commodity 
prod_conv_factors <- fwae_conversion(tr_inl_sel$CommodityCommodity)

tr_fwae <- cbind(tr_inl_sel, prod_conv_factors) %>% 
           select(-one_of('prod_name', 'prod_name_mod'))

rm(fwae_conversion, tr_inl_sel, conv_fwae, prod_conv_factors)



# generate monte carlo estimates of trade weight -------------------------------

uniq_cntry <- c(as.character(unique(tr_fwae$country_code)))

# Sets or retrieves the default number of simulations.
ndvar(10000)
# loop through unique countries
for (j in seq(uniq_cntry)) {
  
  # make temp df only of country
  temp_tr_fwae <-  tr_fwae %>% filter(country_code == uniq_cntry[j])
  
  # loop through products of current country
  for (i in seq(nrow(temp_tr_fwae))) {
    
    # Creates a mcnode object using a random generating function.
    prod_fwae_mc <- mcstoc(rtriang, 
                           min=  temp_tr_fwae[i,'min_conv_fac'], 
                           mode= temp_tr_fwae[i,'mean_conv_fac'], 
                           max=  temp_tr_fwae[i,'max_conv_fac']) 
    
    # multiply product weight to mc factors
    # convert estimates into millions of tons
    prod_fwae_mc <- prod_fwae_mc * temp_tr_fwae[i,'trade'] / 10^6
    
    if(tr_fwae[j,'TradeflowTradeflow'] == 'Import'){
      # sum the products per country
      if (i==1) {imp_prod_fwae_mc <- prod_fwae_mc}
      else {imp_prod_fwae_mc <- imp_prod_fwae_mc + prod_fwae_mc}  
    } 
    
    if(tr_fwae[j,'TradeflowTradeflow'] == 'Export'){
      # sum the products per country
      if (i==1) {exp_prod_fwae_mc <- prod_fwae_mc}
      else {exp_prod_fwae_mc <- exp_prod_fwae_mc + prod_fwae_mc}  
    } 
  }
  
  if(tr_fwae[j,'TradeflowTradeflow'] == 'Import'){
    # append the mcnode into a mc object
    if (exists("cntry_sum_imp_fwae_mc")==0) {cntry_sum_imp_fwae_mc <- mc(imp_prod_fwae_mc)}
    else {cntry_sum_imp_fwae_mc <- mc(cntry_sum_imp_fwae_mc, imp_prod_fwae_mc)}
    
    # name the mcnode of 
    names(cntry_sum_imp_fwae_mc)[length(cntry_sum_imp_fwae_mc)] <- uniq_cntry[j]
  }
  
  if(tr_fwae[j,'TradeflowTradeflow'] == 'Export'){
    # append the mcnode into a mc object
    if (exists("cntry_sum_exp_fwae_mc")==0) {cntry_sum_exp_fwae_mc <- mc(exp_prod_fwae_mc)}
    else {cntry_sum_exp_fwae_mc <- mc(cntry_sum_exp_fwae_mc, exp_prod_fwae_mc)}
    
    # name the mcnode of 
    names(cntry_sum_exp_fwae_mc)[length(cntry_sum_exp_fwae_mc)] <- uniq_cntry[j]
    
  }
}
 
# remove temp objects 
rm(sum_prod_fwae_mc, prod_fwae_mc, uniq_cntry, j, i, exp_prod_fwae_mc, imp_prod_fwae_mc, temp_tr_fwae)


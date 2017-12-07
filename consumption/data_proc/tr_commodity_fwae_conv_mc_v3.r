# THIS SCRIPT IS CALLED BY ANOTHER SCRIPT - NOT RUN STANDALONE



# read and format FW trade table
source('./fishstatj_data/read_nat_trade_data_fw.R', print.eval = TRUE)
rm(tr_sum_bycountry)


### filter tr data to only survey countries  -----------------------------
#  make unique list of country and year of surveys 
surv_uniq_cntry_yr_wide <-  consump_df %>%
                            select(country_code, year_start, year_end) %>%
                            distinct()
                  
surv_uniq_cntry_yr_long <-  surv_uniq_cntry_yr_wide %>%
                            gather(year_type, year_val, -country_code) %>%
                            filter(!is.na(year_val))


# select data from year closes to survey if survey over 2 years, use average
tr_inl_sel<- inner_join(tr, surv_uniq_cntry_yr_long, by='country_code') %>%
            mutate(yr_diff = year-year_val) %>%
            group_by(country_code) %>%
            filter(yr_diff == min(abs(yr_diff))) %>%
            ungroup %>%
            # this averaging assumes that there is no tie between two equally distant year for a country with the exception
            group_by(country_code, CommodityCommodity, TradeflowTradeflow) %>%
            mutate(trade= mean(trade)) %>%
            ungroup %>%
            select(-one_of(c('year', 'year_type', 'year_val', 'yr_diff'))) %>%
            distinct() %>%
            #distinct(country_code, CommodityCommodity, TradeflowTradeflow) %>%
            filter(TradeflowTradeflow != 'Production') %>%
            left_join(., surv_uniq_cntry_yr_wide, by="country_code")

# delete unneeded objects
rm(tr, surv_uniq_cntry_yr_long, surv_uniq_cntry_yr_wide)


### match commodities with FWAE conversion factors -----------------------------

#~~ source function tr fwae_conversion  from script ---------------------------
source('./consumption/data_proc/fcn/fcn_fwae_conv.r')

# add converted factor to commodity 
prod_conv_factors <- fwae_conversion(tr_inl_sel$CommodityCommodity)
tr_fwae <- cbind(tr_inl_sel, prod_conv_factors)


rm(fwae_conversion, tr_inl_sel, conv_fwae, prod_conv_factors)



# generate monte carlo estimates of trade weight -------------------------------

# get counrty code of current loop
uniq_cntry <- c(as.character(unique(tr_fwae$country_code)))

# Sets or retrieves the default number of simulations.
ndvar(10000)


# create output mc objects -----------------------------------------------------
template_mc <- mc(mcstoc())
template_mc[1] <- NULL
#<- rep(0, 10000)
# create mc objects from template
cntry_sum_exp_fwae_assumed_mc <- template_mc
cntry_sum_exp_fwae_matched_mc <- template_mc
cntry_sum_imp_fwae_assumed_mc <- template_mc
cntry_sum_imp_fwae_matched_mc <- template_mc
rm(template_mc)



# loop through unique countries
for (j in seq(uniq_cntry)) {
  
  # make temp df only of country
  temp_tr_fwae <- tr_fwae %>% filter(country_code == uniq_cntry[j])
  
  # loop through products of current country
  for (i in seq(nrow(temp_tr_fwae))) {
    
    # create temp tradeflow variable
    cur_tradeflow <- temp_tr_fwae[i,'TradeflowTradeflow']
    cur_prod_type <- temp_tr_fwae[i,'product']
    
    # Creates a mcnode object using a random generating function.
    prod_fwae_mc <- mcstoc(rtriang, 
                           min=  temp_tr_fwae[i,'min_conv_fac'], 
                           mode= temp_tr_fwae[i,'mean_conv_fac'], 
                           max=  temp_tr_fwae[i,'max_conv_fac']) 
    
    # Creates a mcnode object using a random generating function.
    # if (is.na(temp_tr_fwae[i, 'product'])){
    # prod_fwae_mc <- mcstoc(runif, 
    #                        min=  temp_tr_fwae[i,'min_conv_fac'], 
    #                        max=  temp_tr_fwae[i,'max_conv_fac']) 
    # }
    
    # multiply product weight to mc factors
    # convert estimates into millions of tons
    prod_fwae_mc <- prod_fwae_mc * temp_tr_fwae[i,'trade'] / 10^6
    
    
    
    if(cur_tradeflow == 'Export' & is.na(cur_prod_type)){
      if (exists('sum_exp_fwae_assumed_mc')){
        sum_exp_fwae_assumed_mc <- sum_exp_fwae_assumed_mc + prod_fwae_mc
      }else{sum_exp_fwae_assumed_mc <- prod_fwae_mc}}
    
    
    if(cur_tradeflow == 'Export' & !is.na(cur_prod_type)){
      if (exists('sum_exp_fwae_matched_mc')){
        sum_exp_fwae_matched_mc <- sum_exp_fwae_matched_mc + prod_fwae_mc
      }else{sum_exp_fwae_matched_mc <- prod_fwae_mc}}
    
    
    if(cur_tradeflow=='Import' & is.na(cur_prod_type)){
      if (exists('sum_imp_fwae_assumed_mc')){
        sum_imp_fwae_assumed_mc <- sum_imp_fwae_assumed_mc + prod_fwae_mc
      }else{sum_imp_fwae_assumed_mc <- prod_fwae_mc}}
    
    
    if(cur_tradeflow=='Import' & !is.na(cur_prod_type)){
      if (exists('sum_imp_fwae_matched_mc')){
        sum_imp_fwae_matched_mc <- sum_imp_fwae_matched_mc + prod_fwae_mc
      }else{sum_imp_fwae_matched_mc <- prod_fwae_mc}}
    
  }
  
  
  # After product loop, add the sum to the appropriate MC object
  if (exists("sum_exp_fwae_assumed_mc")==1) {
    cntry_sum_exp_fwae_assumed_mc <- mc(cntry_sum_exp_fwae_assumed_mc, sum_exp_fwae_assumed_mc)
    names(cntry_sum_exp_fwae_assumed_mc)[length(cntry_sum_exp_fwae_assumed_mc)] <- uniq_cntry[j]
    rm(sum_exp_fwae_assumed_mc)}
  
  if (exists("sum_exp_fwae_matched_mc")==1) {
    cntry_sum_exp_fwae_matched_mc <- mc(cntry_sum_exp_fwae_matched_mc, sum_exp_fwae_matched_mc)
    names(cntry_sum_exp_fwae_matched_mc)[length(cntry_sum_exp_fwae_matched_mc)] <- uniq_cntry[j]
    rm(sum_exp_fwae_matched_mc)}
  
  if (exists("sum_imp_fwae_assumed_mc")==1) {
    cntry_sum_imp_fwae_assumed_mc <- mc(cntry_sum_imp_fwae_assumed_mc, sum_imp_fwae_assumed_mc)
    names(cntry_sum_imp_fwae_assumed_mc)[length(cntry_sum_imp_fwae_assumed_mc)] <- uniq_cntry[j]
    rm(sum_imp_fwae_assumed_mc)}
  
  if (exists("sum_imp_fwae_matched_mc")==1) {
      cntry_sum_imp_fwae_matched_mc <- mc(cntry_sum_imp_fwae_matched_mc, sum_imp_fwae_matched_mc)
      # name the mcnode of 
      names(cntry_sum_imp_fwae_matched_mc)[length(cntry_sum_imp_fwae_matched_mc)] <- uniq_cntry[j]
      # remove
      rm(sum_imp_fwae_matched_mc)}
  
  }
 
# remove temp objects 
rm(prod_fwae_mc, uniq_cntry, j, i, cur_prod_type, cur_tradeflow, temp_tr_fwae) 


# THIS SCRIPT IS CALLED BY ANOTHER SCRIPT - NOT RUN STANDALONE



# Join imports, exports, aquaculture and catch to consumption data -------------

# run script that formats the trade data 
#source('./fishstatj_data/read_nat_trade_data_fw.R', print.eval = TRUE)
source('./consumption/data_proc/tr_commodity_fwae_conv_mc_v3.r')#, print.eval = TRUE)
source('./fishstatj_data/read_nat_aquaculture_data.R', print.eval = TRUE)
source('./fishstatj_data/read_nat_catch_data.R', print.eval = TRUE)
rm(aq, aq_inl, ca)#, tr_inl_sel, tr_inl_sel_fwae)  # delete unused df from sourced scripts


# filter aq and ca  to only freshwater catch
aq_sum_bycountry <- aq_sum_bycountry %>% 
  filter(source == 'Inland') %>% ungroup() %>% select(-c(source, type))
ca_sum_bycountry <- ca_sum_bycountry %>% 
  filter(source == 'Inland') %>% ungroup() %>% select(-c(source, type))


# join summed ca, aq, tr data (only keep the consump countries)
consump_nat_agg_df <- consump_df %>%
  select(country.x, country_code, year_start, 
         year_end, grp, confidence_lvl) %>%
  distinct() %>%
  left_join(. , ca_sum_bycountry, 
            by = c("country_code"="country_code",
                   "year_start"="year")) %>%
  left_join(. , aq_sum_bycountry, 
            by = c("country_code"="country_code",
                   "year_start"="year"))  %>%
  select(-one_of('CountryCountry.x','CountryCountry.y')) %>%
  mutate(sum_aquacul = sum_aquacul / 10^6, 
         sum_catch = sum_catch / 10^6)

# change NAs to zeros
consump_nat_agg_df[is.na(consump_nat_agg_df)] <- 0

# remove temp data from env
rm(ca_sum_bycountry, aq_sum_bycountry)



# loop through the consumption rows (i.e. countries) ---------------------------
for (j in seq(nrow(consump_nat_agg_df))) {
  
  # get country_code of current loop
  temp_cntry_code <- consump_nat_agg_df[j,'country_code']
  # get scalar value for aquaculture
  aq <- consump_nat_agg_df[j,'sum_aquacul']
  # get vector of consumption
  cons <- c(cntry_sum_prod_fwae_mc[[temp_cntry_code]])
  
  
  # get Monte Carlo vector of exports 
  if (length(cntry_sum_exp_fwae_matched_mc[[temp_cntry_code]])>0){
    exp_matched <- c(cntry_sum_exp_fwae_matched_mc[[temp_cntry_code]])
  } else {exp_matched <- rep(0,10000)}
  
  # get Monte Carlo vector of exports 
  if (length(cntry_sum_exp_fwae_assumed_mc[[temp_cntry_code]])>0){
    exp_assumed <- c(cntry_sum_exp_fwae_assumed_mc[[temp_cntry_code]])
  } else {exp_assumed <- rep(0,10000)}
  
  # get Monte Carlo vector of imports 
  if (length(cntry_sum_imp_fwae_matched_mc[[temp_cntry_code]])>0){
    imp_matched <- cntry_sum_imp_fwae_matched_mc[[temp_cntry_code]]
  } else {imp_matched <- rep(0,10000)}
  
  # get Monte Carlo vector of imports 
  if (length(cntry_sum_imp_fwae_assumed_mc[[temp_cntry_code]])>0){
    imp_assumed <- cntry_sum_imp_fwae_assumed_mc[[temp_cntry_code]]
  } else {imp_assumed <- rep(0,10000)}
  
  imp <- imp_matched + imp_assumed
  exp <- exp_matched + exp_assumed
  
  
  ###  DO THE TRADE CORRECTION - FOR VALUES AND UNCERTAINTY
  # calculate the corrected of the survey catch  ------------------------------
  tr_corr_consump <- cons - (imp_matched + imp_assumed) + (exp_matched + exp_assumed) - aq
  
  # uncertainty for fm products
  uncert_prodfm_ur <- c(cntry_sum_uncert_prodfm_mc[[temp_cntry_code]])
  
  
  
  # add uncertainty of assumed FWAE from import and export to assumed FWAE uncertainty from consumption --------------
  
  # for assumed FWAE uncertainty
  if (length(cntry_sum_uncert_fwae_assumed_mc[[temp_cntry_code]])>0) {
    uncert_fwae_assumed_mc <- c(cntry_sum_uncert_fwae_assumed_mc[[temp_cntry_code]])
  } else {uncert_fwae_assumed_mc <- rep(0,10000)}
  uncert_fwae_assumed_mc <- uncert_fwae_assumed_mc - imp_assumed + exp_assumed
  
  # for matched FWAE uncertainty  
  if (length(cntry_sum_uncert_fwae_matched_mc[[temp_cntry_code]]>0)) {
    uncert_fwae_matched_mc <- c(cntry_sum_uncert_fwae_matched_mc[[temp_cntry_code]])
  } else {uncert_fwae_matched_mc <- rep(0,10000)}
  uncert_fwae_matched_mc <- uncert_fwae_matched_mc - imp_matched + exp_matched
  
  
  
  # Calc the expectation and uncertainty range ----------------------------
  
  # create column names to extract the 2.5%, mean, 97.5% 
  rng_colnames <- c("p025","mean","p975")
  
  # trade correction uncertainty interval
  tr_corr_ur <- as.data.frame(c(summary(mcdata(tr_corr_consump))))[c(4,1,8)]
  colnames(tr_corr_ur) <- rng_colnames
  tr_corr_ur$datatype <- "tr_corr_consump" 
  
  #consumption pre trade correction
  cons_pretrcorr <- as.data.frame(c(summary(mcdata(cons))))[c(4,1,8)]
  colnames(cons_pretrcorr) <- rng_colnames
  cons_pretrcorr$datatype <- "cons_pretrcorr" 
  
  # import uncertainty interval
  imp_ur <- as.data.frame(c(summary(mcdata(imp))))[c(4,1,8)]
  colnames(imp_ur) <- rng_colnames
  imp_ur$datatype <- "import"
  
  # export uncertainty interval
  exp_ur <- as.data.frame(c(summary(mcdata(exp))))[c(4,1,8)]
  colnames(exp_ur) <- rng_colnames
  exp_ur$datatype <- "export"
  
  
  
  # extract  source-specific uncertainty range and add it to the table
  uncert_prodfm_ur <- as.data.frame(c(summary(mcdata(uncert_prodfm_ur))))[c(4,1,8)]
  colnames(uncert_prodfm_ur) <- rng_colnames
  uncert_prodfm_ur$datatype <- "uncert_prodfm_ur" 
  
  
  uncert_fwae_assumed_mc <- as.data.frame(c(summary(mcdata(uncert_fwae_assumed_mc))))[c(4,1,8)]
  colnames(uncert_fwae_assumed_mc) <- rng_colnames
  uncert_fwae_assumed_mc$datatype <- "uncert_fwae_assumed_mc"
  
  
  uncert_fwae_matched_mc <- as.data.frame(c(summary(mcdata(uncert_fwae_matched_mc))))[c(4,1,8)]
  colnames(uncert_fwae_matched_mc) <- rng_colnames
  uncert_fwae_matched_mc$datatype <- "uncert_fwae_matched_mc" 
  
  
  
  temp_outdf <- rbind(cons_pretrcorr, tr_corr_ur, imp_ur, exp_ur, 
                      c(NA, consump_nat_agg_df[j,'sum_catch'], NA,'sum_catch'),
                      c(NA, consump_nat_agg_df[j,'sum_aquacul'], NA,'sum_aquacul'),
                      uncert_prodfm_ur,
                      uncert_fwae_assumed_mc, uncert_fwae_matched_mc)
  
  
  # add country code as column
  temp_outdf$country_code <- temp_cntry_code
  
  # bind output df for each country to a single output df
  if (j == 1){
    out_tr_sum_df <- temp_outdf
  } else {out_tr_sum_df <- rbind(out_tr_sum_df, temp_outdf)}
  rownames(out_tr_sum_df) <- NULL
}


rm(aq, cons, exp, imp, j, temp_cntry_code, exp_ur, imp_ur, tr_corr_ur, 
   temp_outdf, uncert_prodfm_ur, tr_corr_consump, cons_pretrcorr)



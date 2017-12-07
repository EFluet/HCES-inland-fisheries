


# set cumul mc objects to zero
tot_sum_uncert_prodfm_mc <- rep(0,10000)
tot_sum_uncert_fwae_assumed_mc <- rep(0,10000)
tot_sum_uncert_fwae_matched_mc <- rep(0,10000)




surv_data <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv')
surv_data <- surv_data %>%
             filter(tr_corr_consump>0)

# make list of country codes w/ positive survey estimate 
uniq_cntry_pos <- surv_data$country_code

# loop through countries 
for (j in uniq_cntry_pos){

  t <- c(unlist(cntry_sum_uncert_prodfm_mc[j]))
  if (length(t) > 0){tot_sum_uncert_prodfm_mc <- tot_sum_uncert_prodfm_mc + t}

  y <- c(unlist(cntry_sum_uncert_fwae_assumed_mc[j]))
  if (length(y) > 0){tot_sum_uncert_fwae_assumed_mc <- tot_sum_uncert_fwae_assumed_mc + y}
  
  u <- c(unlist(cntry_sum_uncert_fwae_matched_mc[j]))
  if (length(u) > 0){tot_sum_uncert_fwae_matched_mc <- tot_sum_uncert_fwae_matched_mc + u}
  }




# extract  source-specific uncertainty range and add it to the table
rng_uncert_prodfm_ur <- as.data.frame(c(summary(mcdata(tot_sum_uncert_prodfm_mc))))[c(4,1,8)]
colnames(rng_uncert_prodfm_ur) <- rng_colnames
rng_uncert_prodfm_ur$datatype <- "uncert_prodfm_ur" 

# extract  source-specific uncertainty range and add it to the table
rng_uncert_fwae_assumed_mc <- as.data.frame(c(summary(mcdata(tot_sum_uncert_fwae_assumed_mc))))[c(4,1,8)]
colnames(rng_uncert_fwae_assumed_mc) <- rng_colnames
rng_uncert_fwae_assumed_mc$datatype <- "uncert_fwae_assumed" 

# extract  source-specific uncertainty range and add it to the table
tot_uncert_fwae_matched_mc <- as.data.frame(c(summary(mcdata(tot_sum_uncert_fwae_matched_mc))))[c(4,1,8)]
colnames(tot_uncert_fwae_matched_mc) <- rng_colnames
tot_uncert_fwae_matched_mc$datatype <- "uncert_fwae_matched" 


# rbind uncerts together -----------------------------------------------------
tot_uncert <- rbind(rng_uncert_prodfm_ur, rng_uncert_fwae_assumed_mc) %>%
              rbind(., tot_uncert_fwae_matched_mc) %>%
              mutate(range = p975 - p025) %>%
              mutate(perc_range = range / sum(tot_uncert$range) )




# 
# 
# # AFG  # tot uncert: 0.006329092 - 0.003051387 = 0.0032777
# 
# cntry_sum_uncert_prodfm_mc[1]        # 0.00536 - 0.00322 = 0.00214
# cntry_sum_uncert_fwae_assumed_mc[1]  # 0.0199 - 0.0124 = 0.0075
# cntry_sum_uncert_fwae_matched_mc[1]  # 0.00488-0.00348 = 0.0014
# 
# tr_corr_ur <- as.data.frame(c(summary(mcdata(tr_corr_consump))))[c(4,1,8)]

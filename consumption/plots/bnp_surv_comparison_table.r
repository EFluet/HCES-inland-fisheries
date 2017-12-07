# Description: combines survey cases and BNP cases
#              outputs a table joining them for comparison

# import libraries
library(countrycode)

### read FAO fish catch  -------------------------------------------------------
source('./fishstatj_data/read_nat_catch_data.r')
ca_sum_bycountry <- ca_sum_bycountry %>%  ungroup() %>%
                      filter(source== 'Inland') %>%
                      select(country_code, year, sum_catch)
rm(ca)




### read the survey catch & join to fao catch data  ----------------------------
surv_consump_df <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc.csv', stringsAsFactors=F)

surv_means <- surv_consump_df %>%
            #filter(datatype == 'tr_corr_consump') %>%
            select(country_code, datatype, mean, year_start, year_end) %>%
            filter(datatype %in% c('sum_catch', 'tr_corr_consump')) %>%
            #unite(comb, p025, mean, p975, datatype) %>%
            spread(datatype, mean) 

surv_consump_df <-  surv_consump_df %>% 
  dplyr::select(country_code, datatype, p025, p975) %>%
  filter(datatype %in% c('tr_corr_consump')) %>%
  left_join(., surv_means, by='country_code') %>%
  mutate(mean_dif_perc = (tr_corr_consump-sum_catch)/sum_catch *100,
         p025_dif_perc = (p025-sum_catch)/sum_catch *100,
         p975_dif_perc = (p975-sum_catch)/sum_catch *100) %>%
  # select(country_code, year_start, year_end, sum_catch, tr_corr_consump, 
  #        mean_dif_perc, p025_dif_perc, p975_dif_perc) %>%
  mutate(year_end= ifelse(year_end==0,NA,year_end),
         surv_year= ifelse(is.na(year_end), year_start, 
                           paste(year_start, year_end, sep='-')))
  

rm(surv_means)




#### read Big Numbers Project data  & join in fao catch data -----------------------
bnp_cases <- read.csv('../data/catch_yield/big_num_proj/inland_catch_bnp.csv', stringsAsFactors=F)

# remove non-country and certain columns
bnp_cases  <- bnp_cases  %>% 
  # add country code to the BNP data table
  mutate(country_code= countrycode(bnp_cases$country,'country.name','iso3c',warn=TRUE)) %>%
  # so it doesn't get matched with a country_code
  filter(country != 'Lake Victoria (Kenya, Tanzania, Uganda)') %>% 
  select(-one_of('small_scale_catch','large_scale_catch','Source','Comment'))


colnames(bnp_cases) <- paste(colnames(bnp_cases), "bnp", sep = "_")

  

bnp_cases <- bnp_cases %>%  
  # join FAO catch to BNP data, for the BNP year
  left_join(., ca_sum_bycountry, 
            by=c("country_code_bnp"="country_code","year_start_bnp"="year")) %>%
  # calculate ratio
  mutate(bnp_perc_dif = (total_catch_bnp - sum_catch)/ sum_catch *100,
         bnp_year= ifelse(is.na(year_end_bnp), year_start_bnp, 
                          paste(year_start_bnp, year_end_bnp, sep='-'))) %>%
  
  left_join(., surv_consump_df, by=c("country_code_bnp"="country_code")) %>%
  filter(!is.na(mean_dif_perc)) %>%
  select(-one_of('year_start_bnp', 'year_end_bnp', 'tr_corr_consump',
  #'officially_reported_langings_bnp','country_code_bnp',
  #'# 'ratio_estimate_to_reported_bnp',
                 'year_start','year_end'))



rm(ca_sum_bycountry, surv_consump_df)



### Write output table to CSV file ----------------------------------
write.csv(bnp_cases, '../output/consumption/bnp_catch_ratio_comparison_survey_ratio.csv')





# read FishStatJ catch data 
source("./fishstatj_data/read_nat_catch_data.R")
# RENAME sum_catch  to  sum_catch_fao
c <- ca_sum_bycountry
rm(f, ca, ca_sum_bycountry)

# Match country name with code, for later joining
ca_sum_bycountry$country_code <- countrycode(ca_sum_bycountry$CountryCountry, 
                                             'country.name', 'iso3c', warn = TRUE)



# read survey data, incl. catch difference with FAO stats 
f <- '../output/consumption/hh_survey_consump_nat_agg_prod_fw_mc_diff.csv'
s <- read.csv(f, stringsAsFactors=FALSE)
s <- s %>% select(country_code, tr_corr_consump)



s <-  c %>% 
      ungroup %>%
      filter(year==2014, source=='Inland') %>%
      arrange(-sum_catch) %>%
      mutate(cumul_catch = cumsum(sum_catch)) %>%
      left_join(., sum_surv, by='country_code') %>%
      mutate(sel_cntry_catch = ifelse(!is.na(tr_corr_consump),sum_catch,0))


### Fraction of global catch in surveyed countries
sum(s$sel_cntry_catch) / sum(s$sum_catch)



####

cnt_


##### analyze countries that didn't make the cut

consump_df_unsel <- consump_df %>% 
  filter(!country.x %in% prod_src_fm_cnt_perctry$country.x) 
  # remove Afghanistan - currently missing population data
  #filter(!is.na(consump_million.tons.yr))

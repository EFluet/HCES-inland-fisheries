# tallies the number of certain flags in the aquaculture statistics of FAO
# the percentage of 


### FISH STAT J - SYMBOLS USED -----------------------------------------------------------------

# ... Data not available; unobtainable; data not separately available but included in another category
# - Nil or zero
# 0 More than zero but less than half the unit used
# nei Not elsewhere included
# F FAO estimate from available sources of information 

# from: http://www.fao.org/fishery/static/FishStatJ/FishStatJ_FAQs.pdf



###  -------------------------------------------------------------------

path <- "../data/catch_yield/FishStatJ/exported_from_fishstatj/global_aquaculture_production_all_v2.csv"
aq <- read.csv(path, stringsAsFactors=F)

colnames(aq) <- gsub("[.]|X","",colnames(aq))

aq_inl_sel <- aq %>%
  filter(EnvironmentEnvironment == 'Freshwater') %>%
  gather(year, value, 5:ncol(aq)) %>%
  select(-one_of('EnvironmentEnvironment', 'AquacultureareaFAOmajorfishingarea')) %>%
  mutate(country_code = countrycode(CountryCountry, 'country.name', 'iso3c', warn = TRUE)) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  left_join(surv_cntry_yr, . ,
                        by = c("country_code"="country_code","year_start"="year"))



aq_inl_sel[c('num_val','flag')] <- str_split_fixed(aq_inl_sel$value, " ", 2)

# filter the flag symbols and values
aq_inl_sel <- aq_inl_sel %>%
  mutate(flag = ifelse(num_val=='...', '...', flag),
         num_val = ifelse(num_val=='...', '', num_val)) %>%
  mutate(flag = ifelse(num_val=='-', '-', flag),
         num_val = ifelse(num_val=='-', '', num_val)) %>%
  mutate(num_val = as.numeric(num_val))



# calculate the number of flags  -----------------------------------------------
aq_flag_tally <- tally(group_by(aq_inl_sel, country_code, flag), sort = TRUE)

aq_flag_tally <-  aq_flag_tally %>%
                  mutate(if_flag = ifelse(flag=='F', 'flag', 'not_flag'))
                         #if_flag = ifelse(flag=='...', 'flag', 'not_flag'),
                         #if_flag = ifelse(flag=='-', 'flag', if_flag),
                         #if_flag = ifelse(flag=='0', 'flag', if_flag),



# count the groupings of flag --------------------------------------------------
aq_flag_tally_agg <- tally(group_by(aq_flag_tally, country_code, if_flag), sort = TRUE)

aq_flag_tally_agg <-  aq_flag_tally_agg %>%
                      spread(if_flag, n) %>% 
                      mutate(flag = as.numeric(flag),
                             not_flag = as.numeric(not_flag),
                             flag = ifelse(is.na(flag), 0, flag),
                             not_flag = ifelse(is.na(not_flag), 0, not_flag),
                             flag_perc = flag/(flag + not_flag))
                    

# remove temporary dfs
rm(aq, aq_inl_sel)

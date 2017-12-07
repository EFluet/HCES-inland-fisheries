# tallies the number of certain flags in the cauaculture statistics of FAO
# the percentage of 


### FISH STAT J - SYMBOLS USED -----------------------------------------------------------------

# ... Data not available; unobtainable; data not separately available but included in another category
# - Nil or zero
# 0 More than zero but less than half the unit used
# nei Not elsewhere included
# F FAO estimate from available sources of information 

# from: http://www.fao.org/fishery/static/FishStatJ/FishStatJ_Fcas.pdf



###  -------------------------------------------------------------------

path <- "../data/catch_yield/FishStatJ/exported_from_fishstatj/global_catch_production_all.csv"
ca <- read.csv(path, stringsAsFactors=F)

colnames(ca) <- gsub("[.]|X","",colnames(ca))

ca_inl_sel <- ca %>%
  mutate(source = ifelse(grepl("Inland waters", FishingareaFAOmajorfishingarea),
                         "Inland", "Marine")) %>%
  filter(source == 'Inland') %>%
  gather(year, value, 5:ncol(ca)) %>%
  select(-one_of('EnvironmentEnvironment', 'FishingareaFAOmajorfishingarea')) %>%
  mutate(country_code = countrycode(CountryCountry, 'country.name', 'iso3c', warn = TRUE)) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  left_join(surv_cntry_yr, . ,
            by = c("country_code"="country_code","year_start"="year"))



ca_inl_sel[c('num_val','flag')] <- str_split_fixed(ca_inl_sel$value, " ", 2)

ca_inl_sel <- ca_inl_sel %>%
              mutate(flag = ifelse(num_val=='...', '...', flag),
                     num_val = ifelse(num_val=='...', '', num_val)) %>%
              mutate(flag = ifelse(num_val=='-', '-', flag),
                     num_val = ifelse(num_val=='-', '', num_val)) %>%
              mutate(num_val = as.numeric(num_val))



# calculate the number of flags  -----------------------------------------------
ca_flag_tally <- tally(group_by(ca_inl_sel, country_code, flag), sort = TRUE)

ca_flag_tally <-  ca_flag_tally %>%
  mutate(if_flag = ifelse(flag=='...', 'flag', 'not_flag'),
         #if_flag = ifelse(flag=='-', 'flag', if_flag),
         #if_flag = ifelse(flag=='0', 'flag', if_flag),
         if_flag = ifelse(flag=='F', 'flag', if_flag))


# count the groupings of flag --------------------------------------------------
ca_flag_tally_agg <- tally(group_by(ca_flag_tally, country_code, if_flag), sort = TRUE)

ca_flag_tally_agg <-  ca_flag_tally_agg %>%
                      spread(if_flag, n) %>% 
                      mutate(flag = as.numeric(flag),
                             not_flag = as.numeric(not_flag),
                             flag = ifelse(is.na(flag), 0, flag),
                             not_flag = ifelse(is.na(not_flag), 0, not_flag),
                             flag_perc = flag/(flag + not_flag))


# remove temporary dfs
rm(ca, ca_inl_sel)

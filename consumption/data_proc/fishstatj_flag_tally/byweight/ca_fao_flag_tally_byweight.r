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

# remove periods from column titles
colnames(ca) <- gsub("[.]|X","",colnames(ca))


ca_inl_sel <- ca %>%
  mutate(source = ifelse(grepl("Inland waters", FishingareaFAOmajorfishingarea),
                         "Inland", "Marine")) %>%
  filter(source == 'Inland',
         MeasureMeasure == 'Quantity (tonnes)',
         SpeciesASFISspecies != 'Aquatic plants nei') %>%
  gather(year, value, 5:ncol(ca)) %>%
  select(-one_of('FishingareaFAOmajorfishingarea')) %>%   # 'EnvironmentEnvironment', 
  mutate(country_code = countrycode(CountryCountry, 'country.name', 'iso3c', warn=F)) %>%
  mutate(year = as.numeric(as.character(year))) #%>%
  #filter(year >= 1990)
  # left_join(surv_cntry_yr, . ,
  #           by = c("country_code"="country_code","year_start"="year"))


# split values and flags into two columns
ca_inl_sel[c('num_val','symbol')] <- str_split_fixed(ca_inl_sel$value, " ", 2)

# filter the flag symbols and values
ca_inl_sel <- ca_inl_sel %>%
              filter(num_val != '...') %>%
              mutate(symbol  = ifelse(num_val=='-', '-', symbol),
                     num_val = ifelse(num_val=='-', 0, num_val),
                     num_val = ifelse(value=='0 0', 0.25, num_val),
                     num_val = as.numeric(num_val),
                     # group into flags/no flags
                     flag = ifelse(symbol == 'F', 'flag', 'not_flag')) 



# sum by country and calculate percentage of weight flagged as estimated
ca_flag_tally_agg <-  ca_inl_sel %>%
                      group_by(country_code, year, flag) %>%
                      summarize(num_val = sum(num_val)) %>%
                      ungroup %>%
                      spread(flag, num_val, fill=0) %>%
                      mutate(flag_perc = flag/(flag + not_flag),
                             flag_perc = ifelse(is.na(flag_perc), 0, flag_perc))


# remove temporary dfs
#rm(ca, ca_inl_sel, path)

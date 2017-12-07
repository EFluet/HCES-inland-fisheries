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
# read datafile
path <- "../data/catch_yield/FishStatJ/exported_from_fishstatj/global_aquaculture_production_all_v2.csv"
aq <- read.csv(path, stringsAsFactors=F)

# remove periods from column titles
colnames(aq) <- gsub("[.]|X","",colnames(aq))

# filter to keep only inland, and reformat 
aq_inl_sel <- aq %>%
  filter(EnvironmentEnvironment == 'Freshwater') %>%
  gather(year, value, 5:ncol(aq)) %>%
  dplyr::select(-one_of('EnvironmentEnvironment', 'AquacultureareaFAOmajorfishingarea')) %>%
  mutate(country_code = countrycode(CountryCountry, 'country.name', 'iso3c', warn = F)) %>%
  mutate(year = as.numeric(as.character(year))) #%>%
  #filter(year >= 1990)
  #%>% left_join(surv_cntry_yr, . , by = c("country_code"="country_code","year_start"="year"))


# split values and flags into two columns
aq_inl_sel[c('num_val','symbol')] <- str_split_fixed(aq_inl_sel$value, " ", 2)



# filter the flag symbols and values
aq_inl_sel <-  aq_inl_sel %>%
                filter(num_val != '...') %>%
  
                mutate(symbol  = ifelse(num_val=='-', '-', symbol),
                       num_val = ifelse(num_val=='-', 0, num_val),
                       num_val = ifelse(value=='0 0', 0.25, num_val),
                       num_val = as.numeric(num_val),
                       # group into flags/no flags
                       flag = ifelse(symbol == 'F', 'flag', 'not_flag')) 


# sum by country and calculate percentage of weight flagged as estimated
aq_flag_tally_agg <- aq_inl_sel %>%
                     group_by(country_code, year, flag) %>%
                     summarize(num_val = sum(num_val)) %>%
                     ungroup %>%
                     spread(flag, num_val, fill=0) %>%
                     mutate(flag_perc = flag/(flag + not_flag),
                            flag_perc = ifelse(is.na(flag_perc), 0, flag_perc))
                  

# remove temporary dfs
#rm(aq, aq_inl_sel, path)

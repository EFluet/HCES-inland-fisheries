library(dplyr)



setwd("C:/Users/efluet/Dropbox/chap5_global_inland_fish_catch/scripts")


### ** read catch from fishstatj -----------------------------------------------
source('./fishstatj_data/read_nat_catch_data.R', print.eval = TRUE)

### subset catch data 
ca_inl <- ca %>%
  filter(grepl("Inland waters", FishingareaFAOmajorfishingarea)) %>%
  filter(year %in% consump_prods$year_start) %>%
  mutate(country_code= countrycode(CountryCountry,'country.name', 'iso3c', warn = TRUE)) %>%
  right_join(., consump_prods_cntry_yr, by=c('country_code'='country_code','year'='year_start')) %>%
  filter(!is.na(country_code)) %>%
  filter(!is.na(SpeciesASFISspecies)) %>%
  select(-one_of('year','country_code.x', 'FishingareaFAOmajorfishingarea','MeasureMeasure')) %>%
  mutate(SpeciesASFISspecies=as.character(SpeciesASFISspecies))


### ** read aquaculture from fishstatj -----------------------------------------------
source('./fishstatj_data/read_nat_aquaculture_data.R', print.eval = TRUE)

# subset aquaculture data
aq_inl <- aq %>%
  filter(grepl("Inland waters", AquacultureareaFAOmajorfishingarea)) %>%
  filter(year %in% consump_prods$year_start) %>%
  mutate(country_code= countrycode(CountryCountry,'country.name', 'iso3c', warn = TRUE)) %>%
  right_join(., consump_prods_cntry_yr, by=c('country_code'='country_code','year'='year_start')) %>%
  filter(!is.na(country_code)) %>%
  filter(!is.na(SpeciesASFISspecies)) %>%
  select(-one_of('year','country_code.x', 'CountryCountry','AquacultureareaFAOmajorfishingarea','EnvironmentEnvironment')) %>%
  mutate(SpeciesASFISspecies=as.character(SpeciesASFISspecies))


### ** read aquaculture from fishstatj -----------------------------------------------
source('./fishstatj_data/read_nat_trade_data_fw.R', print.eval = TRUE)

# subset aquaculture data
tr_inl <- tr %>%
  spread(TradeflowTradeflow, trade) %>%
  #filter(grepl("Inland waters", AquacultureareaFAOmajorfishingarea)) %>%
  filter(year %in% consump_prods$year_start) %>%
  mutate(country_code= countrycode(CountryCountry,'country.name', 'iso3c', warn = TRUE)) %>%
  right_join(.,consump_prods_cntry_yr, by=c('country_code'='country_code','year'='year_start')) %>%
  filter(!is.na(country_code)) %>%
  filter(!is.na(CommodityCommodity)) %>%
  select(-one_of('year','CountryCountry','country_code.x', 'AquacultureareaFAOmajorfishingarea','MeasureMeasure')) %>%
  mutate(CommodityCommodity=as.character(CommodityCommodity))

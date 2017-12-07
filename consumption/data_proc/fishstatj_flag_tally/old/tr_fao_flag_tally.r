### FISH STAT J - SYMBOLS USED -------------------------------------------------
# ... Data not available; unobtainable; data not separately available but included in another category
# - Nil or zero
# 0 More than zero but less than half the unit used
# nei Not elsewhere included
# F FAO estimate from available sources of information 
# from: http://www.fao.org/fishery/static/FishStatJ/FishStatJ_FAQs.pdf


### Read and format tr table ---------------------------------------------------
path <- "../data/catch_yield/FishStatJ/exported_from_fishstatj/global_commodities_prod_trade_FAOSTATfreshwater.csv"
tr <- read.csv(path, stringsAsFactors=F)



### reshape table from wide to long format -------------------------------------
colnames(tr) <- gsub("[.]|X","",colnames(tr))

tr <- tr %>%
  gather(year, value, 4:ncol(tr)) %>%
  # filter out empty rows
  filter(value !="" & value != 0 & TradeflowTradeflow !="") %>%
  mutate(country_code = countrycode(CountryCountry, 'country.name', 
                                    'iso3c', warn = TRUE)) %>%
  mutate(year = as.numeric(as.character(year)),
         TradeflowTradeflow = as.character(TradeflowTradeflow))



### select tr commodities found in survey countries-years ----------------------
tr_inl_sel <- left_join(surv_cntry_yr, tr,
                        by = c("country_code"="country_code", "year_start"="year")) %>%
              filter(TradeflowTradeflow != 'Production')


### split the values from the flag ---------------------------------------------

tr_inl_sel[c('num_val','flag')] <- str_split_fixed(tr_inl_sel$value, " ", 2)

tr_inl_sel <- tr_inl_sel %>%
              mutate(flag = ifelse(num_val=='...', '...', flag),
                     num_val = ifelse(num_val=='...', '', num_val)) %>%
  
              mutate(flag = ifelse(num_val=='-', '-', flag),
                     num_val = ifelse(num_val=='-', '', num_val)) %>%
  
              mutate(num_val = as.numeric(num_val))



# calculate the number of flags ------------------------------------------------


tr_flag_tally <- tally(group_by(tr_inl_sel, country_code, 
                                TradeflowTradeflow, flag), sort = TRUE)

tr_flag_tally <-  tr_flag_tally %>%
                  mutate(if_flag = ifelse(flag=='F', 'flag', 'not_flag'))
                  # mutate(if_flag = ifelse(flag=='...', 'flag', 'not_flag'),
                  #        #if_flag = ifelse(tr_flag=='-', 'flag', if_flag),
                  #        #if_flag = ifelse(tr_flag=='0', 'flag', if_flag),
                  #        if_flag = ifelse(flag=='F', 'flag', if_flag))


tr_flag_tally_agg <- tally(group_by(tr_flag_tally, country_code, TradeflowTradeflow, if_flag), sort = TRUE)

tr_flag_tally_agg <-  tr_flag_tally_agg %>%
                      spread(if_flag, n) %>% 
                      mutate(flag = as.numeric(flag),
                             not_flag = as.numeric(not_flag),
                             flag = ifelse(is.na(flag), 0, flag),
                             not_flag = ifelse(is.na(not_flag), 0, not_flag),
                             flag_perc = flag/(flag + not_flag))

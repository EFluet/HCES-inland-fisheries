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

tr_inl_sel <- tr %>%
  gather(year, value, 4:ncol(tr)) %>%
  # filter out empty rows
  filter(value !="" & value != 0 & TradeflowTradeflow !="") %>%
  filter(TradeflowTradeflow != 'Reexport', 
         TradeflowTradeflow != 'Production') %>%
  mutate(country_code = countrycode(CountryCountry, 'country.name', 'iso3c', warn = F)) %>%
  mutate(year = as.numeric(as.character(year)),
         TradeflowTradeflow = as.character(TradeflowTradeflow)) #%>%
  #filter(year >= 1990)


### select tr commodities found in survey countries-years ----------------------
# tr_inl_sel <- left_join(surv_cntry_yr, tr,
#                         by = c("country_code"="country_code", "year_start"="year")) %>%
#               filter(TradeflowTradeflow != 'Production')


# split the values from the flag ---------------------------------------------
tr_inl_sel[c('num_val','symbol')] <- str_split_fixed(tr_inl_sel$value, " ", 2)


tr_inl_sel <- tr_inl_sel %>%
              filter(num_val != '...') %>%
              
              mutate(symbol  = ifelse(num_val=='-', '-', symbol),
                     num_val = ifelse(num_val=='-', 0, num_val),
                     num_val = ifelse(value=='0 0', 0.25, num_val),
                     num_val = as.numeric(num_val),
                     # group into flags/no flags
                     flag = ifelse(symbol == 'F', 'flag', 'not_flag')) 


# calculate the number of flags ------------------------------------------------
# sum by country and calculate percentage of weight flagged as estimated
tr_flag_tally_agg <- tr_inl_sel %>%
                group_by(country_code, year, TradeflowTradeflow, flag) %>%
                summarize(num_val = sum(num_val)) %>%
                ungroup %>%
                spread(flag, num_val, fill=0) %>%
                mutate(flag_perc = flag/(flag + not_flag),
                       flag_perc = ifelse(is.na(flag_perc), 0, flag_perc))





#rm(tr, tr_inl_sel, path)

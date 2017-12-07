# Description:  This script format the consumption table from ADepT and other sources.
#               It then calculates total national consumption for countries and 
#               removes the contribution of imports, exports and aquaculture from FAO.



### Import libraries -----------------------------------------------------------
#library(plyr)
library(dplyr)
library(tidyr)
library(countrycode) # Module that matches and converts country names to codes 
# alternatively, use library(ISOcodes)
library(stringdist)
library(fuzzyjoin)
options("scipen"=100, "digits"=8)



### Read HH survey consumption data  -------------------------------------------
setwd("C:/Users/efluet/Dropbox/chap5_global_inland_fish_catch/scripts")

consump_df <- read.csv('../output/consumption/Consumption_statistics_of_fish.csv', 
               stringsAsFactors=FALSE)

# remove "." from column headers
colnames(consump_df) <- gsub("[.]", "", colnames(consump_df))

# Reduce number of columns, keep only the useful ones
consump_df <- consump_df %>% 
              dplyr::select(1:5)



### rearrange consumption data from header to columns --------------------------

# relies on the title/header rows to have NA at the 

# get the index of the rows matching the headers and a logical vector of the match.
# get a logic vector of length of rows matching the section title in first column.
na_logic_vec <- is.na(consump_df[,5])
na_idx <- which(na_logic_vec)


for (i in 1:length(na_logic_vec)) {
  
  if (na_logic_vec[i] == TRUE) {
    curr_region <- consump_df[i,"FishType"]
    consump_df[i,"Region"] <- curr_region}
  
  else {consump_df[i,"Region"] <- curr_region}
}


# remove the column rows earlier identified, as the region is now a column
consump_df <- consump_df[-na_idx,]

rm(curr_region, na_idx, na_logic_vec, i)



### Read the surveys from Asia Pacific and add them to the table ---------------
source('./consumption/format_asia_pacific_survey_data.R', print.eval = TRUE)

# Exclude records where fish type is total, to avoid duplication of 
# counts when calculating totals
c <- c %>% filter(Fish.Type != 'Total|total')

names(c) <- names(consump_df)
c$section <- 'Asia Pacific Surveys'

# add rows to bottom of dataframe, combining the two sets of surveys
consump_df <- rbind(consump_df, c)

# filter to keep only national records (not mixed up regions or prods in ctry column)
consump_df <- consump_df %>%
              filter(grepl("National", Region))

rm(c)



### Format the year column to a start-end (in YYYY format) ---------------------

# split year along hyphen 
yr_splt <- strsplit(consump_df$year, "-")

# write start and end year to columns
consump_df$year_start <- lapply(yr_splt, function(l) if (length(l[l]) > 0 ) {l[[1]]} else {NA})
consump_df$year_end <- lapply(yr_splt, function(l) if (length(l[l]) > 1 ) {l[[2]]} else {NA})


# Add digit to end year column depending of if years 1900 or 2000
consump_df$year_end <- lapply(consump_df$year_end, function(l) 
  sub("0.+?|1.+?", paste("20", l, sep=''), l))
consump_df$year_end <- lapply(consump_df$year_end, function(l) 
  sub("9.+?", paste("19", l, sep=''), l))
# \\    # .+?

# convert yrear columns to numeric
consump_df$year_start <- as.numeric(consump_df$year_start)
consump_df$year_end <- as.numeric(consump_df$year_end)


# remove unneeded env. variable
rm(yr_splt)



### keep only the latest year of consumption survey ----------------------------

# Add country code
consump_df$country_code <- countrycode(consump_df$country,'country.name', 'iso3c', warn = TRUE)
#names(consump_df)[names(consump_df) == 'country_code.x'] <- 'country_code'

consump_df <- consump_df %>%
  #select(-country_code.y) %>%
  group_by(country_code) %>%
  filter(year == max(year)) 



### Join the fish product coding -----------------------------------------------

prod_codes <- read.csv('../output/consumption/prod_codes/Consumption_statistics_of_fish_unique_fishtypes_with_codes_SFS.csv', 
                       stringsAsFactors=FALSE)

# Add country code to consumption dataframe, for joining
prod_codes$country_code <- countrycode(prod_codes$unique.country,'country.name', 'iso3c', warn = TRUE)


# Convert columns to upper/lower case to match case between tables to join
consump_df$FishType <- tolower(consump_df$FishType)
prod_codes$Freshwater..F..or.Marine..M. <- toupper(prod_codes$Freshwater..F..or.Marine..M.)


# Make assumption by filling blank codes with F, for products without a coding
prod_codes <- prod_codes %>%
  mutate(Freshwater..F..or.Marine..M. = ifelse(is.na(Freshwater..F..or.Marine..M.) |
                                                 Freshwater..F..or.Marine..M. == "", 
                                               'F', Freshwater..F..or.Marine..M.)) %>%
  filter(!is.na(unique.FishType), !is.na(country_code)) %>%
  dplyr::select(-one_of(c('X','X.1')))


consump_df <- filter(consump_df, !is.na(FishType), !is.na(country_code))


# Fuzzy join the 
consump_df <- stringdist_left_join(consump_df, prod_codes, 
                                   by = c(country_code = "country_code", FishType='unique.FishType'),
                                   max_dist=8, distance_col  = "distance")

consump_df <- consump_df %>%
            filter(country_code.distance == 0) %>%
            group_by(country_code.x, FishType) %>%
            filter(FishType.distance == min(FishType.distance)) %>%
            distinct(country_code.x, FishType)

# remove the code df from environment
rm(prod_codes)



### join the national 80-20 groupings and population  -----------------------------------

source('./fishstatj_data/join_nat_catch_pop_wat_2012.R')


prod_percap_df <- prod_percap_df %>% 
  distinct(country_code) %>%
  arrange(-sum_catch) %>%
  mutate(cumsum = cumsum(sum_catch)/sum(sum_catch)*100) %>%
  mutate(grp = ifelse(cumsum < 80, 'First 80%', 'Remaining 20%'))

consump_df <- left_join(consump_df, prod_percap_df,  by= c('country_code.y'='country_code'))

rm(prod_percap_df)



### Convert consumption units to tons/yr ---------------------------------------

# Convert the consumptions units from g/capita/day  to  tonnes/capita/yr
consump_df$consump_million.tons.yr <-
  consump_df$Averageediblequantityconsumedgpersonday *
  consump_df$tot_pop_both_sexes  * 365 / 1000000 / 1000000



### Convert consumed weights into FWAEs w/ conv.factors ------------------------


# read conversion factors
conv_fwae <- read.csv('../data/consumption/FWAE_conversion_factors.csv', 
                      stringsAsFactors=FALSE)


# only keep factors from Hortle report-until a full table is compiled  
conv_fwae <- conv_fwae %>% filter(original_source == 'Hortle et al. 2007')


# Remove word fish from the string to improve match
conv_fwae$product <- gsub('fish|Fish|   ',"", conv_fwae$product)
consump_df$FishType_mod <- gsub('fish|Fish|   ',"", consump_df$FishType)


# Fuzzy closest match betwen fish products 
# currently using 'longest common string' method. More testing needed.
consump_df$conv_fwae_m <- amatch(consump_df$FishType_mod, 
                                 conv_fwae$product, method = c("lcs"), 
                                 maxDist=20)

conv_fwae$conv_fwae_m <- as.numeric(rownames(conv_fwae))
#left_join_NA <- function(x, y, ...) {


consump_df <- left_join(consump_df, conv_fwae, by = 'conv_fwae_m') #%>% 
            #mutate_each(funs(replace(., which(is.na(.)), 1)))


# multiply the consumed quantity
consump_df <- consump_df %>%
  
  mutate(factor_cons_prod_to_fwae = ifelse(is.na(factor_cons_prod_to_fwae),
                                           1.25, factor_cons_prod_to_fwae))

consump_df$consump_million.tons.yr_fwaes <- consump_df$factor_cons_prod_to_fwae * 
  consump_df$consump_million.tons.yr

rm(conv_fwae)



### Subset freshwater capture consumption and aggregate consumption to national -------------------------------

colnames(consump_df)[colnames(consump_df) == 'country_code.x'] <- 'country_code'
colnames(consump_df)[colnames(consump_df) == 'country.x'] <- 'country'


consump_nat_agg_df <- consump_df %>%
                      ungroup() %>%
                      filter(Freshwater..F..or.Marine..M. == 'F',
                             Confidence != 'Low') %>%
                      group_by(country, country_code, year_start, grp) %>%
                      summarise(consump_million.tons.yr = sum(consump_million.tons.yr, na.rm=TRUE),
                                consump_million.tons.yr_fwaes = sum(consump_million.tons.yr_fwaes, na.rm=TRUE))



### Remove freshwater fish imports and add Exports -----------------------------

source('./fishstatj_data/read_nat_fw_trade_data.R', print.eval = TRUE)

# execute left join (only keep the consump countries)
consump_nat_agg_df <- consump_nat_agg_df %>%
  left_join(tr_sum_bycountry, 
            by = c("country_code" = "country_code",
                           "year_start" = "year"))

consump_nat_agg_df <- as.data.frame(consump_nat_agg_df)


# change the empty rows of import to 0
consump_nat_agg_df <- consump_nat_agg_df %>%
                     mutate(Import = ifelse(is.na(Import), 0, Import),
                            Export = ifelse(is.na(Export), 0, Export))



### Join FAO aquaculture  ------------------------------------------------------

source('./fishstatj_data/read_nat_aquaculture_data.R', print.eval = TRUE)

# filter to only freshwater catch
aq_sum_bycountry <- aq_sum_bycountry %>%
  filter(source == 'Inland')

# execute left join (only keep the consump countries)
consump_nat_agg_df <- consump_nat_agg_df %>%
  left_join(aq_sum_bycountry, by = c("country_code" = "country_code",
                                     "year_start" = "year"))

consump_nat_agg_df <- as.data.frame(consump_nat_agg_df)
consump_nat_agg_df <- consump_nat_agg_df %>%
  mutate(sum_aquacul = ifelse(is.na(sum_aquacul),0,sum_aquacul)) %>%
  mutate(sum_aquacult_millions = sum_aquacul / 1000000) 

# Delete duplicated columns
drop_cols <- c('country.y', 'CountryCountry', 'source.x', 'source',
               'source.y', 'CountryCountry.x', 'CountryCountry.y', 'type')
consump_nat_agg_df <- consump_nat_agg_df %>% dplyr::select(-one_of(drop_cols))

rm(drop_cols,aq, aq_inl, aq_sum_bycountry)



### Correct the consumption with trade and aquaculture  ------------------------

# reread table because the group_by created an error with country.x column
consump_nat_agg_df <- as.data.frame(consump_nat_agg_df)  
consump_nat_agg_df <- consump_nat_agg_df %>%
                  mutate(sum_import_millions = Import / 1000000,
                         sum_export_millions = Export / 1000000,
                         consump_million.tons.yr_fwaes_noimp = 
                           consump_million.tons.yr_fwaes - sum_import_millions + sum_export_millions - sum_aquacult_millions)



### Join FAO nationally reported catch for comparison ---------------------

source('./fishstatj_data/read_nat_catch_data.R', print.eval = TRUE)

# filter to only freshwater catch
ca_sum_bycountry <- ca_sum_bycountry %>%
                    filter(source == 'Inland')

# execute left join (only keep the consump countries)
consump_nat_agg_df <- consump_nat_agg_df %>%
  left_join(ca_sum_bycountry, by = c("country_code" = "country_code",
                                      "year_start" = "year"))

consump_nat_agg_df <- as.data.frame(consump_nat_agg_df)
consump_nat_agg_df <- consump_nat_agg_df %>%
  mutate(sum_catch_millions = sum_catch / 1000000)


# Delete duplicated columns
consump_nat_agg_df <- consump_nat_agg_df %>% dplyr::select(-one_of(c('country.y',
                                                      'CountryCountry',
                                                      'source.x',
                                                      'source.y',
                                                      'CountryCountry.y',
                                                      'type')))

rm(ca, ca_sum_bycountry)



### Join the LIFDC list to the table -------------------------------------------

# read the table
lifdc_df <- read.csv('../data/lifd_country_list.csv', stringsAsFactors = FALSE)

# convert the variable on LIFDC in the table
lifdc_df <- mutate(lifdc_df, lifd_2015 = ifelse(is.na(lifd_2015), 
                                                'not LIFDC', 'LIFDC'))

lifdc_df$country_code <- countrycode(lifdc_df$country, 
                                            'country.name', 'iso3c',  warn = TRUE)

# join the lfdc table to the consumption surveys
consump_nat_agg_df <-  left_join(consump_nat_agg_df, lifdc_df,
                             by='country_code')

consump_nat_agg_df$continent <- countrycode(consump_nat_agg_df$country_code, 
                                        'iso3c', 'continent', warn = TRUE)

drop_cols <- c('CountryCountry.x', 'country')
consump_nat_agg_df <- consump_nat_agg_df %>%
                      dplyr::select(-one_of(drop_cols))
rm(drop_cols)
                              
names(consump_nat_agg_df)[1] <- 'country'



### Write the table to csv file ------------------------------------------------
write.csv(consump_df, '../output/consumption/hh_survey_consump_sep_prods.csv')
write.csv(consump_nat_agg_df, '../output/consumption/hh_survey_consump_nat_agg_prod.csv')


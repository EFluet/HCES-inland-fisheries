# Description:  This script format the consumption table from ADepT and other sources.
#               It then calculates total national consumption for countries and 
#               removes the contribution of imports, exports and aquaculture from FAO.



### Import libraries -----------------------------------------------------------
#library(plyr)
library(stringr)
library(dplyr)
library(tidyr)
library(countrycode) # Module that matches and converts country names to codes 
# alternatively, use library(ISOcodes)
library(stringdist)
library(fuzzyjoin)
library(iterators)
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



### Format year column to a start-end (in YYYY format) and keep only latest ----

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


# Add country code, for group_by and later for joining 
consump_df$country_code <- countrycode(consump_df$country,'country.name', 'iso3c', warn = TRUE)

# keep only the latest year of consumption survey 
consump_df <- consump_df %>%
              group_by(country_code) %>%
              filter(year == max(year)) 

# remove unneeded env. variable
rm(yr_splt)



### Join the fish product origin expert coding -----------------------------------------------

prod_codes <- read.csv('../output/consumption/prod_codes/Consumption_statistics_of_fish_unique_fishtypes_with_codes_SFS.csv', 
                       stringsAsFactors=FALSE)

# Add country code to consumption dataframe, for joining
prod_codes$country_code <- countrycode(prod_codes$unique.country,'country.name', 'iso3c', warn = TRUE)


# Convert columns to upper/lower case to match case between tables to join
consump_df$FishType <- tolower(consump_df$FishType)
prod_codes$Freshwater..F..or.Marine..M. <- toupper(prod_codes$Freshwater..F..or.Marine..M.)


# Make assumption by filling blank codes with F/M, for products without an expert code
prod_codes <- prod_codes %>%
  mutate(Freshwater..F..or.Marine..M. = ifelse(is.na(Freshwater..F..or.Marine..M.) |
                                                 Freshwater..F..or.Marine..M. == "", 
                                               'F/M', Freshwater..F..or.Marine..M.)) %>%
  filter(!is.na(unique.FishType), !is.na(country_code)) %>%
  dplyr::select(-one_of(c('X','X.1')))

# remove rows with null Fishtype and country code
consump_df <- filter(consump_df, !is.na(FishType), !is.na(country_code))


# Fuzzy join the fish type names and countries
# countries must be identical, but some difference may exist between 
consump_df <- stringdist_left_join(consump_df, prod_codes, 
                                   by = c(country_code = "country_code", FishType='unique.FishType'),
                                   max_dist=8, distance_col  = "distance")


# for each fish product, select a single closest matching code and country
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

# read conversion factors table
conv_fwae <- read.csv('../data/consumption/FWAE_conversion_factors.csv', 
                      stringsAsFactors=FALSE)

# subset to the factors from Hortle report
# --- until a full table is compiled  
conv_fwae <- conv_fwae %>% 
             filter(original_source == 'Hortle et al. 2007') %>%
             select(-one_of(c('species','region','compiled_from','original_source'))) %>%
             gather(lang, product, prod_eng:prod_esp) %>%
             select(-lang) %>%
             mutate(product = tolower(product),
                    rowid = rownames(.))


# Remove word fish from strings of both tables to improve match
rm_str <- 'fish|Fish|poisson|Poisson|pescado|autres|autros|otros'
consump_df$FishType_mod <- gsub(rm_str," ", consump_df$unique.FishType)

# substitute some punctionation for spaces to improve splits in some cases.
consump_df$FishType_mod <- gsub("[[:punct:]]", "", consump_df$FishType_mod)


# create empty product column where matches will be written 
consump_df$product <- NA

# loop through products with conv factors
# there is probs a way to make this more efficient with a 
for (y in 1:nrow(conv_fwae)) {
  
  # declare product name of loop as string pattern to look for
  pattern <- conv_fwae[y,'product']
  
  
  # look for string pattern in each of the words of fish type name (split with spaces)
  # allow for a 1 letter change for match to be made
  consump_df$temp_match <- sapply(seq(nrow(consump_df)),
           function(i){ unlist(agrepl(pattern, 
                        strsplit(as.character(consump_df[i,'FishType_mod']), ' '), 
                        ignore.case = TRUE, max = 1), recursive = TRUE)})
  
  # for rows with match in current loop, copy the matched product name 
  # to a permanent column (these can get overwritten by later loops) 
  consump_df <- consump_df %>%
                mutate(product= 
                         as.character(ifelse(temp_match == TRUE,
                                             conv_fwae[y,'product'], 
                                             product))) %>%
                
                # remove temporary match column
                select(-temp_match)
}


# join the other columns of FWAEs conversion factors  
consump_df <- left_join(consump_df, conv_fwae, by=c('product'))


# if no match is made, use the fresh fish FWAE conversion factor (1.25)
consump_df <- consump_df %>%
  mutate(factor_cons_prod_to_fwae = ifelse(is.na(factor_cons_prod_to_fwae),
                                           1.25, factor_cons_prod_to_fwae)) %>%
  
  # convert consumed weight to FWAEs by mutliplying with consumed weight
  mutate(consump_million.tons.yr_fwaes = factor_cons_prod_to_fwae * consump_million.tons.yr)


# rename columns after join added some subfix
colnames(consump_df)[colnames(consump_df) == 'country_code.x'] <- 'country_code'
colnames(consump_df)[colnames(consump_df) == 'country.x'] <- 'country'

# remove unneeded object from environment
rm(pattern, y, rm_str, conv_fwae)



### Include uncertainty of F/M codes to consumed weights ------------------------

# create vector of column names to process and the output column names
vars <- c('consump_million.tons.yr', 'consump_million.tons.yr_fwaes')
vars_mf50 <- setNames(vars, paste0(vars, "_mf50"))
vars_mf50_rng <- setNames(vars, paste0(vars, "_mf50_rng"))

# create new columns of 50% value for products of F/M coding.
consump_df <- consump_df %>% ungroup() %>%
  mutate(code_fm = Freshwater..F..or.Marine..M.) %>%
  mutate_each_(funs(ifelse(code_fm == 'F/M', ./2, .)), vars_mf50) %>%
  mutate_each_(funs(ifelse(code_fm == 'F/M', ./2, NA)), vars_mf50_rng) 

# remove temp vector of column names
rm(vars, vars_mf50, vars_mf50_rng)




### Aggregate fish consumption to countries (fw and !=low confidence) ------

# set vector of columns to summarize
vars <- c('consump_million.tons.yr', 'consump_million.tons.yr_fwaes', 
          'consump_million.tons.yr_fwaes_mf50','consump_million.tons.yr_fwaes_mf50_rng')

# aggregate the fish products to countries
consump_nat_agg_df <- consump_df %>%
  
          # keep only freshwater and mixed source products for 
          # countries of high confidence.
          filter(Freshwater..F..or.Marine..M. != 'M',
                 Confidence != 'Low') %>%
        
          # sum the consumption per country
          group_by(country, country_code, year_start, grp) %>%
          summarise_each_(funs(sum(. , na.rm=TRUE)), vars)

# remove the variable vector
rm(vars)



### Join imports, exports, aquaculture and catch to consumption data -----------

# run script that formats the trade data 
source('./fishstatj_data/read_nat_fw_trade_data.R', print.eval = TRUE)
source('./fishstatj_data/read_nat_aquaculture_data.R', print.eval = TRUE)
source('./fishstatj_data/read_nat_catch_data.R', print.eval = TRUE)

# delete df generated by sourced scripts that are not needed
rm(aq, aq_inl, ca)

# filter to only freshwater catch
aq_sum_bycountry <- aq_sum_bycountry %>% filter(source == 'Inland')
ca_sum_bycountry <- ca_sum_bycountry %>% filter(source == 'Inland')


# execute left join (only keep the consump countries)
consump_nat_agg_df <- 
  
  left_join(consump_nat_agg_df, aq_sum_bycountry, 
            by = c("country_code" = "country_code", "year_start" = "year")) %>%
  
  left_join(. , tr_sum_bycountry, 
            by = c("country_code" = "country_code", "year_start" = "year")) %>%
  
  left_join(. , ca_sum_bycountry, 
            by = c("country_code" = "country_code", "year_start" = "year"))


# change the empty rows (NA) to 0
vars <- c('Import', 'Export', 'sum_aquacul')
consump_nat_agg_df <- consump_nat_agg_df %>% ungroup() %>%
                      mutate_each_(funs(ifelse(is.na(.), 0, .)), vars)


# Delete duplicated columns
drop_cols <- c('source.x', 'source.y','CountryCountry.x', 
               'CountryCountry.y', 'type.x', 'type.y')
consump_nat_agg_df <- consump_nat_agg_df %>% dplyr::select(-one_of(drop_cols))


# remove temp data from env
rm(aq, aq_inl, ca, drop_cols, vars, 
   ca_sum_bycountry, aq_sum_bycountry, tr_sum_bycountry)



### Correct the consumption with import/export and aquaculture  ------------------------


# convert trade to millions of tons and remove effect on consumption
consump_nat_agg_df <- consump_nat_agg_df %>%
  
      # convert values to millions of tons as new column
      mutate(sum_import_millions = Import / 1000000,
             sum_export_millions = Export / 1000000,
             sum_aquacult_millions = sum_aquacul / 1000000,
             sum_catch_millions = sum_catch / 1000000,
             
             # account for the contribution of trade and aquaculture
             consump_million.tons.yr_fwaes_mf50_noimp = 
             consump_million.tons.yr_fwaes_mf50 - sum_import_millions + sum_export_millions - sum_aquacult_millions) %>%
             
             # remove conlumns expressed in tons 
             dplyr::select(-one_of(c('Import', 'Export','sum_aquacul', 'sum_catch')))
             



### Join the LIFDC list to the table -------------------------------------------

# read the table with LIFDC codes
lifdc_df <- read.csv('../data/lifd_country_list.csv', stringsAsFactors = FALSE)

# create binary sting column describing if LIFDC or not
lifdc_df <- mutate(lifdc_df,lifd_2015=ifelse(is.na(lifd_2015),'not LIFDC','LIFDC'))

# add country code to lifdc and join it to nat table
lifdc_df$country_code <- countrycode(lifdc_df$country,'country.name','iso3c', warn=TRUE)
consump_nat_agg_df <-  left_join(consump_nat_agg_df, lifdc_df, by='country_code')

# add continent column to the nat agg table
consump_nat_agg_df$continent <- countrycode(consump_nat_agg_df$country_code,'iso3c','continent',warn=TRUE)
consump_nat_agg_df$country <- countrycode(consump_nat_agg_df$country_code,'iso3c','country.name',warn=TRUE)
consump_df$country <- countrycode(consump_df$country_code,'iso3c','country.name',warn=TRUE)

# remove temporary df from env
rm(lifdc_df)



### Write the table to csv file ------------------------------------------------
write.csv(consump_df, '../output/consumption/hh_survey_consump_sep_prods.csv')
write.csv(consump_nat_agg_df, '../output/consumption/hh_survey_consump_nat_agg_prod.csv')


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
source('./consumption/data_proc/format_asia_pacific_survey_data.R', print.eval = TRUE)

# Exclude records where fish type is 'total', to avoid duplication of 
# counts when calculating totals
c <- c %>% filter(Fish.Type != 'Total|total')

names(c) <- names(consump_df)
c$section <- 'Asia Pacific Surveys'

# remove kazakhstan because it is duplicated in the AsiaPacific survey
c <- c %>%
  filter(country != 'Kazakhstan',
         country != 'Indonesia')

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



### Format consump_df prior to join with coding -------------------------------
consump_df <- consump_df %>% 
  # Add country code, for group_by and later for joining 
  mutate(country_code = countrycode(consump_df$country,'country.name','iso3c',warn=F)) %>%
  # keep only the latest year of consumption survey 
  group_by(country_code) %>% filter(year == max(year))  %>%
  # ungroup the grouping by year, for other changes to be made
  ungroup() %>%
  # remove rows with null Fishtype and country code
  filter(!is.na(FishType), !is.na(country_code)) %>%
  # conver product names to lowercase
  mutate(prod_name = tolower(FishType)) %>%
  # remove column (because of confusing name)
  select(-FishType)

# remove unneeded env. variable
rm(yr_splt)



### Read and format product code table -----------------------------------------

# read file
prod_codes_df <- read.csv('../output/consumption/prod_codes/Consumption_statistics_of_fish_unique_fishtypes_with_codes_v3.csv', stringsAsFactors=FALSE)

# clean data table prior to join
prod_codes_df <- prod_codes_df %>%
  mutate(
    # Convert columns to upper/lower case to match case between tables to join
    country_code = countrycode(unique.country,'country.name', 'iso3c', warn = F),
    # Add country code to consumption dataframe, for joining
    prod_src_fm = toupper(prod_src_fm),
    # Make assumption by filling blank codes with F/M, for products without an expert code
    prod_src_fm = ifelse(is.na(prod_src_fm) | prod_src_fm == "", 'F/M', prod_src_fm)) %>%
  filter(
    # filter rows to remove producs and country_codes with NA 
    !is.na(unique.prod_name), !is.na(country_code)) #%>%
# not sure what this does now.... (as of Oct 15 2016)
#dplyr::select(-one_of(c('X','X.1')))



### Fuzzy join consumed fish products and expert coding --------------------------  
# join on: type names and countries
# countries must be identical, but some difference may exist between product names (accents, etc.)
consump_df <- stringdist_left_join(
  consump_df, prod_codes_df, 
  by = c(country_code = "country_code", prod_name='unique.prod_name'),
  max_dist=8, distance_col  = "distance")



### Select a single closest matching code and country, for each product ----------
consump_df <- consump_df %>%
  filter(country_code.distance == 0) %>%
  group_by(country_code.x, prod_name) %>%
  filter(prod_name.distance == min(prod_name.distance)) %>%
  distinct(country_code.x, prod_name) %>%
  ungroup()


# remove the code df from environment
rm(prod_codes_df)



### join the national 80-20 groupings and population  -----------------------------------

# Get df of joined catch pop water area
#source('./fishstatj_data/join_nat_catch_pop_wat_2012.R')


# define the groups of 80%/20% of global catch based on cumulative sum
# prod_percap_df <- prod_percap_df %>%
#   distinct(country_code) %>%
#   arrange(-sum_catch) %>%
#   mutate(cumsum = cumsum(sum_catch)/sum(sum_catch)*100) %>%
#   mutate(grp = ifelse(cumsum < 80, 'First 80%', 'Remaining 20%'))
# 
# # join the dfs together
# consump_df <- left_join(consump_df, prod_percap_df,  
#                         by= c('country_code.x'='country_code'))
# 
# rm(prod_percap_df)


### Correct Malawi fisheries  --------------------------------------------------
# reduce consumption from 
source('./consumption/data_proc/correct_malawi_consump.r')

### Add afghanistan population - missing from FAOSTAT --------------------------
consump_df <- consump_df %>%
              mutate(tot_pop_both_sexes= ifelse(country_code.x =='AFG',
                                                (25877.544 * 1000),tot_pop_both_sexes))



### Convert consumption units to national tons/yr ------------------------------

# Convert the consumptions units from g/capita/day  to  tonnes/capita/yr
consump_df$consump_million.tons.yr <-
                      consump_df$Averageediblequantityconsumedgpersonday *
                      consump_df$tot_pop_both_sexes  * 365 / 10^6 / 10^6



### Write the table to csv file ------------------------------------------------
write.csv(consump_df, '../output/consumption/hh_survey_consump_sep_prods.csv')


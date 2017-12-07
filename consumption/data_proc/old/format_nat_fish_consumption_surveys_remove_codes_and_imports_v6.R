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
prod_codes_df <- read.csv('../output/consumption/prod_codes/Consumption_statistics_of_fish_unique_fishtypes_with_codes_SFS.csv', stringsAsFactors=FALSE)

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
source('./fishstatj_data/join_nat_catch_pop_wat_2012.R')


# define the groups of 80%/20% of global catch based on cumulative sum
prod_percap_df <- prod_percap_df %>%
  distinct(country_code) %>%
  arrange(-sum_catch) %>%
  mutate(cumsum = cumsum(sum_catch)/sum(sum_catch)*100) %>%
  mutate(grp = ifelse(cumsum < 80, 'First 80%', 'Remaining 20%'))

# join the dfs together
consump_df <- left_join(consump_df, prod_percap_df,  by= c('country_code.x'='country_code'))

rm(prod_percap_df)



### Convert consumption units to national tons/yr ---------------------------------------

# Convert the consumptions units from g/capita/day  to  tonnes/capita/yr
consump_df$consump_million.tons.yr <-
                      consump_df$Averageediblequantityconsumedgpersonday *
                      consump_df$tot_pop_both_sexes  * 365 / 10^6 / 10^6



### FWAEs w/ conv.factors - convert consumed weights into whole animal weights ------------------------

# read conversion factors table
conv_fwae <- read.csv('../data/consumption/fwae_conversion_factors/fwae_conversion_factors.csv', 
                      stringsAsFactors=FALSE)


# subset to the factors from Hortle report
# --- until a full table of conversion factor is compiled
# TODO: Use range of FWAE conversion factor, and include as uncertainty range 

conv_fwae <- conv_fwae %>% 
             #filter(original_source == 'Hortle et al. 2007') %>%
             select(-one_of('compiled_from','region','species')) %>%
             #spread(original_source, factor_cons_prod_to_fwae) 
             group_by(prod_eng) %>%
             mutate(min_conv_fac = min(factor_cons_prod_to_fwae),
                    med_conv_fac = median(factor_cons_prod_to_fwae),
                    max_conv_fac = max(factor_cons_prod_to_fwae)) %>%
             select(-one_of(c('species','region','compiled_from','original_source','factor_cons_prod_to_fwae','comment'))) %>%
             distinct() %>%
             gather(lang, product, prod_eng:prod_esp) %>%
             select(-lang)  # remove column listing language of product


# create string of words to remove from fish product names
rm_str <- 'fish|Fish|poisson|Poisson|pescado|autres|autros|otros|peixe|outros'
consump_df <- consump_df %>%
        # Remove word fish from strings of both tables to improve match
        mutate(prod_name_mod = gsub(rm_str," ", unique.prod_name)) %>%
        # substitute some punctionation for spaces to improve splits in some cases.
        mutate(prod_name_mod = gsub("[[:punct:]]", "", prod_name_mod)) %>%
        # create empty product column where matches will be written 
        mutate(product = NA)


# loop through products with conv factors
# there is probs a way to make this more efficient with a 
for (y in 1:nrow(conv_fwae)) {
  
  # declare product name of loop as string pattern to look for
  pattern <- conv_fwae[y,'product']
  
  
  # look for string pattern in each of the words of fish type name (split with spaces)
  # allow for a 1 letter change for match to be made
  consump_df$temp_match <- sapply(seq(nrow(consump_df)),
           function(i){ unlist(agrepl(pattern, 
                        strsplit(as.character(consump_df[i,'prod_name_mod']), ' '), 
                        ignore.case = TRUE, max = 1), recursive = TRUE)})
  
  # for rows with match in loop, copy the matched product name 
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
  mutate(min_conv_fac = ifelse(is.na(min_conv_fac), 1.25, min_conv_fac),
         med_conv_fac = ifelse(is.na(med_conv_fac), 2.50, med_conv_fac),
         max_conv_fac = ifelse(is.na(max_conv_fac), 2.91, max_conv_fac)) %>%
  
  # convert consumed weight to FWAEs by mutliplying with consumed weight
  mutate(cons_miltonspyr_min_fwaes = min_conv_fac * consump_million.tons.yr,
         cons_miltonspyr_med_fwaes = med_conv_fac * consump_million.tons.yr,
         cons_miltonspyr_max_fwaes = max_conv_fac * consump_million.tons.yr)

# rename columns after join added some subfix
colnames(consump_df)[colnames(consump_df) == 'country_code.y'] <- 'country_code'
colnames(consump_df)[colnames(consump_df) == 'country.y'] <- 'country'

# remove unneeded object from environment
rm(pattern, y, rm_str, conv_fwae)



### Include uncertainty of F/M codes to consumed weights ------------------------

# create vector of column names to process and the output column names
# vars <- c('consump_million.tons.yr', 'consump_million.tons.yr_fwaes')
# vars_mf50 <- setNames(vars, paste0(vars, "_mf50"))
# vars_mf50_rng <- setNames(vars, paste0(vars, "_mf50_rng"))

# # create new columns of 50% value for products of F/M coding.
# consump_df <- consump_df %>% 
#   ungroup() %>%
#   mutate_each_(funs(ifelse(prod_src_fm == 'F/M', ./2, .)), vars_mf50) %>%
#   mutate_each_(funs(ifelse(prod_src_fm == 'F/M', ./2, NA)), vars_mf50_rng) 

consump_df <- consump_df %>% 
  ungroup() %>%
  mutate(min_conv_fac = ifelse(prod_src_fm == 'F/M', 0, min_conv_fac,
  mutate(med_conv_fac = ifelse(prod_src_fm == 'F/M', 0, min_conv_fac))
  

# remove temp vector of column names
rm(vars, vars_mf50, vars_mf50_rng)



### Write the table to csv file ------------------------------------------------
write.csv(consump_df, '../output/consumption/hh_survey_consump_sep_prods.csv')



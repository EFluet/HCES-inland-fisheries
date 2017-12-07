# Description: This script format the consumption table from ADepT
#              Also, joins in population and exports from FAO.



### Import libraries -----------------------------------------------------------
library(dplyr)
library(tidyr)
# Module that matches and converts country names to codes 
# alternatively, use library(ISOcodes)
library(countrycode)
options("scipen"=100, "digits"=4)



### Read HH survey consumption data  -------------------------------------------
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



### Split year start-end and convert format to YYYY ----------------------------

# split year along hyphen 
yr_splt <- strsplit(consump_df$year, "-")

# write start and end year to columns
consump_df$year_start <- lapply(yr_splt, function(l) if (length(l[l]) > 0 ) {l[[1]]} else {NA})
consump_df$year_end <- lapply(yr_splt, function(l) if (length(l[l]) > 1 ) {l[[2]]} else {NA})



### Add  digits to end year to make it full year format YYYY -------------------
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



### Join the fish product coding -----------------------------------------------

prod_codes <- read.csv('../output/consumption/prod_codes/Consumption_statistics_of_fish_unique_fishtypes_with_codes_SFS.csv', 
         stringsAsFactors=FALSE)

consump_df$FishType <- tolower(consump_df$FishType)


# Make assumption by filling blank codes with F, for products without a coding
prod_codes <- prod_codes %>%
              mutate(Freshwater..F..or.Marine..M. = ifelse(is.na(Freshwater..F..or.Marine..M.) |
                                                           Freshwater..F..or.Marine..M. == "", 
                                                           'F', Freshwater..F..or.Marine..M.))

# Add country code to consumption dataframe, for joining
consump_df$country_code <- countrycode(consump_df$country,
                                       'country.name', 'iso3c', warn = TRUE)
# Add country code to consumption dataframe, for joining
prod_codes$country_code <- countrycode(prod_codes$unique.country,
                                       'country.name', 'iso3c', warn = TRUE)

# execute left join (only keep the consump countries)
consump_df1 <- left_join(consump_df, prod_codes, by = c("country_code" = "country_code",
                           "FishType" = "unique.FishType"))

# remove the code df from environment
rm(prod_codes)



### Convert consumed weights into FWAEs w/ conv.factors ------------------------

library(stringdist)


# read conversion factors
conv_fwae <- read.csv('../data/consumption/FWAE_conversion_factors.csv', 
                      stringsAsFactors=FALSE)


# only keep factors from Hortle report-until a full table is compiled  
conv_fwae <- conv_fwae %>% filter(original_source == 'Hortle et al. 2007')


# Remove word fish from the string to improve match
conv_fwae$product <- gsub('fish|Fish|   ',"", conv_fwae$product)
consump_df$FishType <- gsub('fish|Fish|   ',"", consump_df$FishType)


# Fuzzy closest match betwen fish products 
# currently using 'longest common string' method. More testing needed.
consump_df$conv_fwae_m <- amatch(consump_df$FishType, 
                                 conv_fwae$product, method = c("lcs"), 
                                 maxDist=20)


# pre-assign empty data frame of right size for the matches
fact_df_rpt <- data.frame(matrix(ncol = length(names(conv_fwae)), 
                                 nrow = length(consump_df[,1])))
colnames(fact_df_rpt) <- names(conv_fwae)


for (i in 1:length(consump_df[,1])){
  idx_row_to_add <- consump_df[i, 'conv_fwae_m']
  fact_df_rpt[i,]  <- conv_fwae[idx_row_to_add,]
}


# add the factors for each row to the consump dataframe
consump_df <- cbind(consump_df, fact_df_rpt)
rm(fact_df_rpt, idx_row_to_add, i)


# unamtched produts, assumed to be fresh fish
na_fac_idx <- is.na(consump_df$factor_cons_prod_to_fwae)
consump_df[na_fac_idx, c('factor_cons_prod_to_fwae')] <- 1.25
consump_df[na_fac_idx, c('product')] <- 'no match - assumed fresh fish'


# multiply the consumed quantity
consump_df <- consump_df %>%
  mutate(consump_gppersperday_fwaes =
           Averageediblequantityconsumedgpersonday * factor_cons_prod_to_fwae)


rm(na_fac_idx, conv_fwae)





### join the national 80-20 groupings  -----------------------------------

source('./join_nat_catch_pop_wat_2012.R')


prod_percap_df <- prod_percap_df %>% 
  distinct(country_code) %>%
  arrange(-sum_catch) %>%
  mutate(cumsum = cumsum(sum_catch)/sum(sum_catch)*100) %>%
  mutate(grp = ifelse(cumsum < 80, 'First 80%', 'Remaining 20%'))

# # Add country code to consumption dataframe, for joining
# consump_df$country_code <- countrycode(consump_df$country.x,
#                                        'country.name', 'iso3c', warn = TRUE)


consump_df <- left_join(consump_df, prod_percap_df,  by=c('country_code'))

rm(prod_percap_df)



### Convert consumption units to tons/yr ---------------------------------------

# Convert the consumptions units from g/capita/day  to  tonnes/capita/yr
consump_df$consump_fwae_million.tons.yr <-
                  consump_df$consump_gppersperday_fwaes *
                  consump_df$tot_pop_both_sexes  * 365 / 1000000 / 1000000



### keep only the latest year of consumption survey ----------------------------
consump_df <- consump_df %>% 
  group_by(country_code) %>%
  filter(year == max(year))



### Subset freshwater capture consumption and aggregate consumption to national -------------------------------

consump_nat_agg_df <- consump_df %>%
                      filter(Freshwater..F..or.Marine..M. == 'F',
                             Confidence == 'High') %>%
                      group_by(country.x, country_code, year_start, grp) %>%
                      summarise(consump_gppersperday_fwaes = sum(consump_gppersperday_fwaes),
                                consump_fwae_million.tons.yr = sum(consump_fwae_million.tons.yr))
                      



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

# reread table because the group_by created an error with country.x column
consump_nat_agg_df <- as.data.frame(consump_nat_agg_df)  
consump_nat_agg_df <- consump_nat_agg_df %>%
                  mutate(sum_import_millions = Import / 1000000,
                         sum_export_millions = Export / 1000000,
                         consump_fwae_million.tons.yr_noimp = 
                           consump_fwae_million.tons.yr - sum_import_millions + sum_export_millions)

# delete the trade table from the environment 
rm(tr_sum_bycountry)



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


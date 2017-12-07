# Description: This script format the consumption table from ADepT
#              Also, joins in population and exports from FAO.



### Import libraries -----------------------------------------------------------
library(dplyr)
library(tidyr)
# Module that matches and converts country names to codes 
# alternatively, use library(ISOcodes)
library(countrycode)
options("scipen"=100, "digits"=4)
library(rgdal)
library(rgeos)
library(ggplot2)
library(fuzzyjoin)
library(RColorBrewer)
library(scales)


### Read HH survey consumption data  -------------------------------------------
consump_df <- read.csv('../output/consumption/Consumption_statistics_of_fish.csv', 
                       stringsAsFactors=FALSE)
# '../output/consumption/Consumption_statistics_of_fish.csv'
# '../output/consumption/Food_item_protein_consumption_by_region.csv'


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

consump_df <- rbind(consump_df, c)

rm(c)



### Make a sheet for FishType encoding by experts  -----------------------------
# 
# consump_df$FishType <- tolower(consump_df$FishType)
# 
# consump_df_forcoding <- consump_df %>%
#   group_by(country,FishType) %>%
#   summarise() %>%
#   dplyr::select(unique.country=country, unique.FishType=FishType)
# 
# 
# write.csv(consump_df_forcoding, '../output/consumption/Consumption_statistics_of_fish_unique_fishtypes_for_coding.csv')
# rm(consump_df_forcoding)



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



### Join the fish product coding -----------------------------------------------

prod_codes <- read.csv('../output/consumption/Consumption_statistics_of_fish_unique_fishtypes_with_codes.csv', 
                       stringsAsFactors=FALSE)



# Make assumption that all uncoded fish product is freshwater
prod_codes[is.na('Freshwater..F..or.Marine..M.') , 'Freshwater..F..or.Marine..M.'] <- 'F'

prod_codes <- prod_codes %>%
  mutate(Freshwater..F..or.Marine..M. = ifelse(is.na(Freshwater..F..or.Marine..M.) |
                                                 Freshwater..F..or.Marine..M. == "", 
                                               'F', 
                                               Freshwater..F..or.Marine..M.))


# execute left join (only keep the consump countries)
consump_df <- consump_df %>%
  left_join(prod_codes, by = c("country" = "unique.country",
                               "FishType" = "unique.FishType"))

# remove the code df from environment
rm(prod_codes)



### Subset freshwater capture consumption and aggregate consumption to national -------------------------------
consump_df <- consump_df %>%
  filter(!grepl('National', Region),
         !grepl('Urban', Region),
         !grepl('Rural', Region)) %>%
  group_by(country, Region, year) %>%
  summarise(consump_gppersperday_fwaes = 
              sum(consump_gppersperday_fwaes, na.rm=TRUE))

# export table to move the 
consump_df_foredit <- consump_df %>%
                      select(country, Region) %>%
                      distinct(country, Region)
# write.csv(consump_df_foredit, '../output/consumption/consumption_regions_foredit.csv')
rm(consump_df_foredit)



### Split year start-end and convert format to YYYY ----------------------------

# split year along hyphen 
yr_splt <- strsplit(consump_df$year, "-")

# write start and end year to columns
consump_df$year_start <- lapply(yr_splt, function(l) if (length(l[l]) > 0 ) {l[[1]]} else {NA})
consump_df$year_end <- lapply(yr_splt, function(l) if (length(l[l]) > 1 ) {l[[2]]} else {NA})



### Add digits to end year to make it full year format YYYY -------------------
# Add digit to end year column depending of if years 1900 or 2000
consump_df$year_end <- lapply(consump_df$year_end, function(l) 
  sub("0.+?|1.+?", paste("20", l, sep=''), l))
consump_df$year_end <- lapply(consump_df$year_end, function(l) 
  sub("9.+?", paste("19", l, sep=''), l))

# convert yrear columns to numeric
consump_df$year_start <- as.numeric(consump_df$year_start)
consump_df$year_end <- as.numeric(consump_df$year_end)


# remove unneeded env. variable
rm(yr_splt)



### keep only the latest year of consumption survey ----------------------------
consump_df <- consump_df %>% 
              group_by(country) %>%
              filter(year == max(year))


# add country code to table
consump_df$country_code <- countrycode(consump_df$country, 
                                       'country.name', 'iso3c', warn = TRUE)



### Join the renamed regions list ------------------------------------------ 

reg <- read.csv('../output/consumption/gadm1_data_with_subdiv_match.csv', 
                stringsAsFactors=FALSE)

reg <-  reg %>%
        select(ISO, PolyName, SurveyUnit) %>%
        filter(SurveyUnit != '') 

names(reg) <- c('country_code', 'SubDiv','Region')

consump_subdiv_df <- right_join(consump_df, reg, by=c('country_code','Region'))

# remove the edit table
rm(reg, consump_df)



### Write the comsup_subdiv_df to CSV

write.csv(consump_subdiv_df, '../output/consumption/hh_survey_consump_w_subdiv.csv')

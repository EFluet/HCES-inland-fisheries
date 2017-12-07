# Join national statistics of capture, aquaculture and trade to consumption
# This is used to compare the consumption to the different FAO national statistics.




### Import modules -------------------------------------------------------------
library(countrycode)
library(data.table)


setwd('C:/Users/efluet/Dropbox/chap5_global_inland_fish_catch/scripts/consumption')



### run scripts that read in data ----------------------------------------------

# read data for consumption and catch
source('./format_nat_fish_consumption_data.R', print.eval=TRUE)

# read in consumption survey and national fishstat 
source('../fishstatj_data/read_nat_aquaculture_data.R', print.eval=TRUE)
source('../fishstatj_data/read_nat_catch_data.R', print.eval=TRUE)
source('../fishstatj_data/read_nat_trade_data.R', print.eval=TRUE)

# delete unneeded objects from the sourced scripts
rm(tr, ca, aq, aq_inl)


### Rename columns for the FishStatJ data --------------------------------------
# (catch, aquacult, trade)

colnames(aq_sum_bycountry)[4] <- "value"
colnames(ca_sum_bycountry)[4] <- "value"
colnames(tr_sum_bycountry)[3] <- "type"
colnames(tr_sum_bycountry)[4] <- "value"



### Modify consumption data to long format -------------------------------------

# Drop columns in consumption df
drops <- c("year", "year_end", "Averageediblequantityconsumedgpersonday",
           "tot_pop_both_sexes", "country.y", "CountryCountry")
consump_pop_df <- consump_pop_df[ , !(names(consump_pop_df) %in% drops)]
rm(drops)


# add values to two new columns, and rename other columns
consump_pop_df <- consump_pop_df %>%
  mutate(type = "Consumption", source = "All") %>%
  rename("year" = year_start, "value" = consump_million.tons.yr)



### Rbind diff fishstatj tables (ca, aq, tr) together --------------------------

nat_joined <- do.call("rbind", 
                      list(aq_sum_bycountry, 
                           ca_sum_bycountry, 
                           tr_sum_bycountry))

colnames(nat_joined)[1] <- "country"


# remove data frames no longer used after rbind and join
rm(aq_sum_bycountry, ca_sum_bycountry, tr_sum_bycountry)



###  Join consumption to national statistics -----------------------------------


# Convert the country names of national catch data to ISO3 codes 
nat_joined$country_code <- countrycode(nat_joined$country, 
                                       'country.name', 'iso3c', warn = TRUE)


# select only rows with matching country and year of surveys
# from the FishStatJ data 
nat_joined <- semi_join(nat_joined, 
                        consump_pop_df, 
                        by = c('country_code'='country_code',
                               'year'='year'))



# add rows at bottom of table
consump_pop_nat_joined <- rbind(nat_joined, consump_pop_df)

# Convert the Code back to country names for consistency across sources  
consump_pop_nat_joined$country <- countrycode(consump_pop_nat_joined$country_code, 
                                       'iso3c','country.name',  warn = TRUE)



### Write joined table to CSV and close ----------------------------------------
# The reason for this is because the paper df gives an error when queries
# something about unknown column CountryCountry, which should be deleted
write.csv(consump_pop_nat_joined, "../../output/consump_pop_nat_joined.csv")
rm(consump_pop_nat_joined)

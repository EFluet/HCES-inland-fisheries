
### FWAEs w/ conv.factors - convert consumed weights into whole animal weights ------------------------

# read conversion factors table
consump_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods_refuse.csv',
                       stringsAsFactors=F)


# source function doing fwae conversion 
source('./consumption/data_proc/fcn/fcn_fwae_conv.r')

# execute function matching products to FWAE factors 
prod_conv_factors <-  fwae_conversion(consump_df$prod_name)

# add factor to the product list
consump_df <- cbind(consump_df, prod_conv_factors)

# copy country names (for consistency with nat_agg df)
# rename columns after join added some subfix
#consump_df$country <- countrycode(consump_df$country_code,'iso3c','country.name',warn=TRUE)
colnames(consump_df)[colnames(consump_df) == 'country_code.y'] <- 'country_code'
colnames(consump_df)[colnames(consump_df) == 'country.y'] <- 'country'
consump_df$country <- consump_df$country.x

### Write the table to csv file ------------------------------------------------
write.csv(consump_df, '../output/consumption/hh_survey_consump_sep_prods_refuse_fwaes.csv')


rm(conv_fwae, fwae_conversion)

# This script intakes the consumed products, ajoined by the expert coding
# and removes the 
# Combine output of agg prod into a single df 
#### TODOs:
# - unify the reading of trade FishStatJ file into a single script-function
#   using a filenames as input
# - Move the reading of catch and aquaculutre files outside the loop, and rename the filtered version



### Read input files -----------------------------------------------------------
# read the code-assigned product list
consump_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods_fwaes_uncert.csv',
                       stringsAsFactors=FALSE)



### ** Aggregate product consumption to national total -----------------------------

# create vector of columns to summarise
vars <- c('cons_miltonspyr',
          'cons_miltonspyr_fm_min',
          'cons_miltonspyr_fm_mean',
          'cons_miltonspyr_fm_max',
          'cons_miltonspyr_fm_min_fwaes',
          'cons_miltonspyr_fm_mean_fwaes',
          'cons_miltonspyr_fm_max_fwaes')

# sum consumption per country
consump_nat_agg_df <- consump_df %>%
  
  
  filter(confidence_lvl == 'high' | confidence_lvl == 'medium') %>%
  #prod_src_fm == 'F' | prod_src_fm == 'F/M') %>%
  
  group_by(country.x, country_code, year_start, grp, confidence_lvl) %>%
  summarise_each_(funs(sum(. , na.rm=TRUE)), vars) %>% 
  ungroup() %>%
  filter(cons_miltonspyr_fm_min_fwaes > 0) 




### Join imports, exports, aquaculture and catch to consumption data -----------

# run script that formats the trade data 
#source('./fishstatj_data/read_nat_trade_data_fw.R', print.eval = TRUE)
source('./consumption/data_proc/tr_commodity_fwae_conv.r')#, print.eval = TRUE)
source('./fishstatj_data/read_nat_aquaculture_data.R', print.eval = TRUE)
source('./fishstatj_data/read_nat_catch_data.R', print.eval = TRUE)
rm(aq, aq_inl, ca, tr_inl_sel, tr_inl_sel_fwae)  # delete unused df from sourced scripts


# filter to only freshwater catch
aq_sum_bycountry <- aq_sum_bycountry %>% filter(source == 'Inland')
ca_sum_bycountry <- ca_sum_bycountry %>% filter(source == 'Inland')


# execute left join (only keep the consump countries)
consump_nat_agg_df <- 
  
        left_join(consump_nat_agg_df , ca_sum_bycountry, 
                  by = c("country_code"="country_code","year_start"="year")) %>%
        left_join(. , aq_sum_bycountry, 
                  by = c("country_code"="country_code","year_start"="year")) %>%
        left_join(. , tr_fwae_sum_bycountry, 
                by = c("country_code"="country_code","year_start"="year_start"))

# drop factor columns
consump_nat_agg_df <- consump_nat_agg_df %>%
    select(-one_of('CountryCountry.x','CountryCountry.y'))
# change NAs to 0
consump_nat_agg_df[is.na(consump_nat_agg_df)] <- 0

# remove temp data from env
rm(drop_cols, vars, ca_sum_bycountry, aq_sum_bycountry, tr_sum_bycountry)



### Correct the consumption with import/export and aquaculture  ----------------
# convert trade to millions of tons and remove effect on consumption
consump_nat_agg_df <- consump_nat_agg_df %>%
  
  # convert values to millions of tons as new column
  mutate(#import_miltons = Import / 10^6,
          #export_miltons = Export / 10^6,
          Export.sum_tr_fwae_min = Export.sum_tr_fwae_min / 10^6,
          Export.sum_tr_fwae_mean = Export.sum_tr_fwae_mean / 10^6,
          Export.sum_tr_fwae_max = Export.sum_tr_fwae_max / 10^6,
          Import.sum_tr_fwae_min = Import.sum_tr_fwae_min / 10^6,
          Import.sum_tr_fwae_mean = Import.sum_tr_fwae_mean / 10^6,
          Import.sum_tr_fwae_max = Import.sum_tr_fwae_max / 10^6,
          aquacult_miltons = sum_aquacul / 10^6,
          catch_miltons = sum_catch / 10^6) %>%

  
  mutate(cons_miltonspyr_fm_min_fwaes_notr = 
           cons_miltonspyr_fm_min_fwaes-aquacult_miltons - Import.sum_tr_fwae_min + Export.sum_tr_fwae_min,
         
         cons_miltonspyr_fm_mean_fwaes_notr = 
           cons_miltonspyr_fm_mean_fwaes-aquacult_miltons - Import.sum_tr_fwae_mean + Export.sum_tr_fwae_mean,
         
         cons_miltonspyr_fm_max_fwaes_notr = 
           cons_miltonspyr_fm_max_fwaes-aquacult_miltons - Import.sum_tr_fwae_min + Export.sum_tr_fwae_max) #%>%
  


### Join the LIFDC list to the table -------------------------------------------
# read the table with LIFDC codes
lifdc_df <- read.csv('../data/lifd_country_list.csv', stringsAsFactors = FALSE)

# create binary sting column describing if LIFDC or not
# add country code to lifdc and join it to nat table
lifdc_df <- lifdc_df %>%
            mutate(lifd_2015=ifelse(is.na(lifd_2015),'not LIFDC','LIFDC'),
                   country_code= countrycode(lifdc_df$country,'country.name','iso3c', warn=TRUE))
  


consump_nat_agg_df <- consump_nat_agg_df %>% 
                      left_join(., lifdc_df, by='country_code') %>%
                      mutate(continent= countrycode(consump_nat_agg_df$country_code,'iso3c','continent',warn=TRUE))

# add continent column to the nat agg table
consump_nat_agg_df$continent <- countrycode(consump_nat_agg_df$country_code,'iso3c','continent',warn=TRUE)
consump_nat_agg_df$country <- countrycode(consump_nat_agg_df$country_code,'iso3c','country.name',warn=TRUE)

# remove temporary df from env
rm(lifdc_df)


# # rename country column
consump_nat_agg_df$country <- countrycode(consump_nat_agg_df$country_code,'iso3c','country.name',warn=TRUE)


### Write the table to csv file ------------------------------------------------
write.csv(consump_nat_agg_df, '../output/consumption/hh_survey_consump_nat_agg_prod_fw_uncert_tradecorr_tr_fwae.csv')




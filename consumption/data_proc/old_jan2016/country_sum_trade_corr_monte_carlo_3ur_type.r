# import package
library(tidyr)
library(dplyr)
library(mc2d) # monte carlo
# fitdistrplus package is convenient for assessing a parametric distribution of data

# wrapper function that checks if variable object exists or not; return logical
exist <- function(x) { return(exists(deparse(substitute(x))))}



# Read input files -----------------------------------------------------------
# read the code-assigned product list
filedpath <- '../output/consumption/hh_survey_consump_sep_prods_fwaes_uncert.csv'
consump_df <- read.csv(filedpath, stringsAsFactors=FALSE)

# count the number of F/M products per country
# this count is used to filter countries
prod_src_fm_cnt_perctry <-  consump_df %>%
                            group_by(country.x, prod_src_fm) %>%
                            summarise(src_fm_count = n()) %>%
                            ungroup() %>%
                            mutate(prod_src_fm=ifelse(prod_src_fm=='F/M','FM',prod_src_fm)) %>%
                            spread(prod_src_fm, src_fm_count) %>%
                            mutate(F=ifelse(is.na(F),0, F),
                                   FM=ifelse(is.na(FM),0, FM),
                                   M=ifelse(is.na(M),0, M)) %>%
                            # selection criteria for countries, 
                            # that F products constitute 50% of product pool
                            filter((F/(F+FM)) >= 0.5)

# filter the consumption data to keep only countries meeting the F + F/M criteria
# also filter for  NA in critical fields
consump_df <- consump_df %>% 
              filter(country.x %in% prod_src_fm_cnt_perctry$country.x) %>%
              #filter(confidence_lvl == 'high' | confidence_lvl == 'medium') %>%
              filter(prod_src_fm != 'M') %>%
              # remove Afghanistan - currently missing population data
              filter(!is.na(consump_million.tons.yr)) %>%
              mutate(cons_miltonspyr = consump_million.tons.yr)


# create list of unique country codes to loop through
uniq_cntry <- unique(consump_df$country_code)

# Sets or retrieves the default number of simulations.
ndvar(10000)


# create output mc objects
template_mc <- mc(mcstoc())
template_mc[1] <- NULL 
cntry_sum_prod_fwae_mc <- template_mc
cntry_sum_uncert_prodfm_mc <- template_mc
cntry_sum_uncert_fwae_matched_mc <- template_mc
cntry_sum_uncert_fwae_assumed_mc <- template_mc



# loop through unique country codes ------------------------------------------
for (j in seq(uniq_cntry)) {
  
  # make temp df only of looped-country survey entries
  temp_consump_df <-  consump_df %>% filter(country_code == uniq_cntry[j])

  # loop through products of the country
  for (i in seq(nrow(temp_consump_df))) {
   
    cur_fwae_prod <-temp_consump_df[i,'product']
    
    # for unresolved products create unifor distribution, otherwise, value of 1
    if (temp_consump_df[i,'prod_src_fm'] == 'F/M') {
      prod_fm_unif <- mcstoc(runif, min=0, max=1)
    } else prod_fm_unif <- mcstoc(runif, min=1, max=1) 
    
    # Creates a mcnode object using the monte carlo stochastic sampling function.
    # assume a triangular distribution between the min, mean, max factors.
    prod_fwae_fac_mc <- mcstoc(rtriang, 
                       min=  temp_consump_df[i,'min_conv_fac'], 
                       mode= temp_consump_df[i,'mean_conv_fac'], 
                       max=  temp_consump_df[i,'max_conv_fac'])
    
    # multiply product weight to mc factors to estimate the FWEA consumption with 
    # both sources of uncertainty: FWAE factors and F/M uncertainty.
    # F/M unif * FWAE triang * prodweight
    prod_fwae_mc <- prod_fm_unif * prod_fwae_fac_mc * temp_consump_df[i,'cons_miltonspyr']
  
    # create vectors for each of the uncertainty sources
    uncert_prodfm_mc <- prod_fm_unif * temp_consump_df[i,'cons_miltonspyr']
    
    # depending on whether the FWAE is matched, list uncertainty 
    if (is.na(temp_consump_df[i,'product'])){
      uncert_fwae_assumed_mc <- prod_fwae_fac_mc * temp_consump_df[i,'cons_miltonspyr']
    }else{uncert_fwae_matched_mc <- prod_fwae_fac_mc * temp_consump_df[i,'cons_miltonspyr']}


    # sum the products per country --------------------------------------------
    # if firt product look, creat the sum MC object
    if (i==1) {
      sum_prod_fwae_mc <- prod_fwae_mc
      sum_uncert_prodfm_mc <- uncert_prodfm_mc}
    
    if (exist(sum_uncert_fwae_assumed_mc)==FALSE & is.na(cur_fwae_prod)){
        sum_uncert_fwae_assumed_mc <- uncert_fwae_assumed_mc}
    
    if (exist(sum_uncert_fwae_matched_mc)==FALSE & !is.na(cur_fwae_prod)){
      sum_uncert_fwae_matched_mc <- uncert_fwae_matched_mc}
    
    
    # on following MC object, add the products to the uncertainty that sum all products 
    if (i != 1){
      sum_prod_fwae_mc <- sum_prod_fwae_mc + prod_fwae_mc
      sum_uncert_prodfm_mc <- sum_uncert_prodfm_mc + uncert_prodfm_mc}
    
    # conditionally add the products to the Null / Non-Null uncertainty
    if (exist(sum_uncert_fwae_assumed_mc)==TRUE & is.na(cur_fwae_prod)){
      sum_uncert_fwae_assumed_mc <- sum_uncert_fwae_assumed_mc + uncert_fwae_assumed_mc}
    
    if (exist(sum_uncert_fwae_matched_mc)==TRUE & !is.na(cur_fwae_prod)){
      sum_uncert_fwae_matched_mc <- sum_uncert_fwae_matched_mc + uncert_fwae_matched_mc}
  }
    
    
  
  # append the mcnode into a mc object with the summed vector for each country
   
  # if the first country, create 
  cntry_sum_prod_fwae_mc <- mc(cntry_sum_prod_fwae_mc, sum_prod_fwae_mc)
  names(cntry_sum_prod_fwae_mc)[j] <- uniq_cntry[j]
  
  cntry_sum_uncert_prodfm_mc <- mc(cntry_sum_uncert_prodfm_mc, sum_uncert_prodfm_mc)
  names(cntry_sum_uncert_prodfm_mc)[j] <- uniq_cntry[j]

  
  # if the first country, create 
  if (exist(sum_uncert_fwae_assumed_mc)==TRUE){
    cntry_sum_uncert_fwae_assumed_mc <- mc(cntry_sum_uncert_fwae_assumed_mc, sum_uncert_fwae_assumed_mc)
    a <- length(cntry_sum_uncert_fwae_assumed_mc)
    names(cntry_sum_uncert_fwae_assumed_mc)[a] <- uniq_cntry[j]}
  
  # if the first country, create 
  if (exist(sum_uncert_fwae_matched_mc)==TRUE){
    cntry_sum_uncert_fwae_matched_mc <- mc(cntry_sum_uncert_fwae_matched_mc, sum_uncert_fwae_matched_mc)
    m <- length(cntry_sum_uncert_fwae_matched_mc)
    names(cntry_sum_uncert_fwae_matched_mc)[j] <- uniq_cntry[j]}
  
  # remove the sum objects
  rm(sum_uncert_fwae_matched_mc, sum_uncert_fwae_assumed_mc, sum_uncert_prodfm_mc, sum_prod_fwae_mc)
}


# remove temp objects
rm(temp_consump_df, sum_prod_fwae_mc, prod_fwae_fac_mc, j, i, uncert_prodfm_mc, 
   prod_fm_unif, uniq_cntry, prod_fwae_mc, 
   sum_uncert_fwae_mc, sum_uncert_prodfm_mc, sum_uncert_fwae_matched_mc, sum_uncert_fwae_assumed_mc)

hist(cntry_sum_prod_fwae_mc)





source('./consumption/data_proc/')




# copy the percentiles to the df with contries with FAO catch data -------------
consump_nat_agg_df_mcperc <-  consump_nat_agg_df %>% 
                        select(-one_of('sum_catch','sum_aquacul')) %>%
                        left_join(., out_ur_df, by='country_code') %>%
                        # set uncertainty range to NA, if both are missing (or equal to eachother)
                        # this prevents errorbars from needlessly appearing
                        mutate(p025= ifelse(p025==p975, NA, p025),
                               p975= ifelse(p025==p975, NA, p975))




# Write the table to csv file ------------------------------------------------
write.csv(consump_nat_agg_df_mcperc, '../output/consumption/hh_survey_consump_nat_agg_prod_fw_mc.csv')





# ### Correct the consumption with import/export and aquaculture  ----------------
# # convert trade to millions of tons and remove effect on consumption
# consump_nat_agg_df <- consump_nat_agg_df %>%
#   
#   # convert values to millions of tons as new column
#   mutate(#import_miltons = Import / 10^6,
#     #export_miltons = Export / 10^6,
#     Export.sum_tr_fwae_min = Export.sum_tr_fwae_min / 10^6,
#     Export.sum_tr_fwae_mean = Export.sum_tr_fwae_mean / 10^6,
#     Export.sum_tr_fwae_max = Export.sum_tr_fwae_max / 10^6,
#     Import.sum_tr_fwae_min = Import.sum_tr_fwae_min / 10^6,
#     Import.sum_tr_fwae_mean = Import.sum_tr_fwae_mean / 10^6,
#     Import.sum_tr_fwae_max = Import.sum_tr_fwae_max / 10^6,
#     aquacult_miltons = sum_aquacul / 10^6,
#     catch_miltons = sum_catch / 10^6) %>%
#   
#   
#   mutate(cons_miltonspyr_fm_min_fwaes_notr = 
#            cons_miltonspyr_fm_min_fwaes-aquacult_miltons - Import.sum_tr_fwae_min + Export.sum_tr_fwae_min,
#          
#          cons_miltonspyr_fm_mean_fwaes_notr = 
#            cons_miltonspyr_fm_mean_fwaes-aquacult_miltons - Import.sum_tr_fwae_mean + Export.sum_tr_fwae_mean,
#          
#          cons_miltonspyr_fm_max_fwaes_notr = 
#            cons_miltonspyr_fm_max_fwaes-aquacult_miltons - Import.sum_tr_fwae_min + Export.sum_tr_fwae_max) #%>%
# 
# 
# ### Join the LIFDC list to the table -------------------------------------------
# # read the table with LIFDC codes
# lifdc_df <- read.csv('../data/lifd_country_list.csv', stringsAsFactors = FALSE)
# 
# # create binary sting column describing if LIFDC or not
# # add country code to lifdc and join it to nat table
# lifdc_df <- lifdc_df %>%
#   mutate(lifd_2015=ifelse(is.na(lifd_2015),'not LIFDC','LIFDC'),
#          country_code= countrycode(lifdc_df$country,'country.name','iso3c', warn=TRUE))
# 
# 
# 
# consump_nat_agg_df <- consump_nat_agg_df %>% 
#   left_join(., lifdc_df, by='country_code') %>%
#   mutate(continent= countrycode(consump_nat_agg_df$country_code,'iso3c','continent',warn=TRUE))
# 
# # add continent column to the nat agg table
# consump_nat_agg_df$continent <- countrycode(consump_nat_agg_df$country_code,'iso3c','continent',warn=TRUE)
# consump_nat_agg_df$country <- countrycode(consump_nat_agg_df$country_code,'iso3c','country.name',warn=TRUE)
# 
# # remove temporary df from env
# rm(lifdc_df)
# 
# 
# # # rename country column
# consump_nat_agg_df$country <- countrycode(consump_nat_agg_df$country_code,'iso3c','country.name',warn=TRUE)
# 
# 
# ### Write the table to csv file ------------------------------------------------
# write.csv(consump_nat_agg_df, '../output/consumption/hh_survey_consump_nat_agg_prod_fw_uncert_tradecorr_tr_fwae.csv')



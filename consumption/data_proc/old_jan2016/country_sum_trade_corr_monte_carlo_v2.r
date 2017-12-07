# import package
library(tidyr)
library(dplyr)
library(mc2d) # monte carlo
# fitdistrplus package is convenient for assessing a parametric distribution of data


exist <- function(x) {return(exists(deparse(substitute(x))}



# Read input files -----------------------------------------------------------
# read the code-assigned product list
consump_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods_fwaes_uncert.csv',
                       stringsAsFactors=FALSE)

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

# loop through unique country codes
for (j in seq(uniq_cntry)) {
  
  # make temp df only of looped-country survey entries
  temp_consump_df <-  consump_df %>% filter(country_code == uniq_cntry[j])

  # loop through products of the country
  for (i in seq(nrow(temp_consump_df))) {
   
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
    
    if (exists(deparse(substitute(sum_uncert_fwae_assumed_mc)))==FALSE & is.na(temp_consump_df[i,'product'])){
        sum_uncert_fwae_assumed_mc <- uncert_fwae_assumed_mc}
    
    if (exists(deparse(substitute(sum_uncert_fwae_assumed_mc)))==FALSE & !is.na(temp_consump_df[i,'product'])){
      sum_uncert_fwae_matched_mc <- uncert_fwae_matched_mc}
    
    
    # on following MC object, add the products to the uncertainty that sum all products 
    if (i != 1){
      sum_prod_fwae_mc <- sum_prod_fwae_mc + prod_fwae_mc
      sum_uncert_prodfm_mc <- sum_uncert_prodfm_mc + uncert_prodfm_mc}
    
    # conditionally add the products to the Null / Non-Null uncertainty
    if (exists(deparse(substitute(sum_uncert_fwae_assumed_mc)))==TRUE & is.na(temp_consump_df[i,'product'])){
      sum_uncert_fwae_assumed_mc <- sum_uncert_fwae_assumed_mc + uncert_fwae_assumed_mc}
    
    if (exists(deparse(substitute(sum_uncert_fwae_assumed_mc)))==TRUE & !is.na(temp_consump_df[i,'product'])){
      sum_uncert_fwae_matched_mc <- sum_uncert_fwae_matched_mc + uncert_fwae_matched_mc}
  }
    
    
  
  # append the mcnode into a mc object with the summed vector for each country
  # if the first country, create 
  if (j==1) {
    cntry_sum_prod_fwae_mc <- mc(sum_prod_fwae_mc)
    cntry_sum_uncert_prodfm_mc <- mc(sum_uncert_prodfm_mc)}
  
  
  if (exists(deparse(substitute(cntry_sum_uncert_fwae_assumed_mc)))==FALSE & 
      exists(deparse(substitute(sum_uncert_fwae_assumed_mc)))==TRUE) {
      cntry_sum_uncert_fwae_assumed_mc <- mc(sum_uncert_fwae_assumed_mc)}
  
  
    }else{cntry_sum_uncert_fwae_matched_mc <- mc(sum_uncert_fwae_matched_mc)}
    
  } else {
    cntry_sum_prod_fwae_mc <- mc(cntry_sum_prod_fwae_mc, sum_prod_fwae_mc)
    cntry_sum_uncert_prodfm_mc <- mc(cntry_sum_uncert_prodfm_mc, sum_uncert_prodfm_mc)
    if (is.na(temp_consump_df[i,'product'])){
      cntry_sum_uncert_fwae_assumed_mc <- mc(cntry_sum_uncert_fwae_assumed_mc, sum_uncert_fwae_assumed_mc)
    }else{cntry_sum_uncert_fwae_matched_mc <- mc(cntry_sum_uncert_fwae_matched_mc, sum_uncert_fwae_matched_mc)}}

  
  # name the mcnode with country code
  names(cntry_sum_prod_fwae_mc)[j] <- uniq_cntry[j]
  names(cntry_sum_uncert_prodfm_mc)[j] <- uniq_cntry[j]
  if (is.na(temp_consump_df[i,'product'])){
    names(cntry_sum_uncert_fwae_assumed_mc)[j] <- uniq_cntry[j]
  }else{names(cntry_sum_uncert_fwae_matched_mc)[j] <- uniq_cntry[j]}

}


# remove temp objects
rm(temp_consump_df, sum_prod_fwae_mc, prod_fwae_fac_mc, j, i, uncert_prodfm_mc, 
   prod_fm_unif, uniq_cntry, prod_fwae_mc, 
   sum_uncert_fwae_mc, sum_uncert_prodfm_mc, sum_uncert_fwae_matched_mc, sum_uncert_fwae_assumed_mc)

hist(cntry_sum_prod_fwae_mc)






# Join imports, exports, aquaculture and catch to consumption data -------------

# run script that formats the trade data 
#source('./fishstatj_data/read_nat_trade_data_fw.R', print.eval = TRUE)
source('./consumption/data_proc/tr_commodity_fwae_conv_mc.r')#, print.eval = TRUE)
source('./fishstatj_data/read_nat_aquaculture_data.R', print.eval = TRUE)
source('./fishstatj_data/read_nat_catch_data.R', print.eval = TRUE)
rm(aq, aq_inl, ca)#, tr_inl_sel, tr_inl_sel_fwae)  # delete unused df from sourced scripts


# filter aq and ca  to only freshwater catch
aq_sum_bycountry <- aq_sum_bycountry %>% filter(source == 'Inland') %>% ungroup() %>% select(-c(source, type))
ca_sum_bycountry <- ca_sum_bycountry %>% filter(source == 'Inland') %>% ungroup() %>% select(-c(source, type))


# make unique country df
# join ca, aq, tr data (only keep the consump countries)
consump_nat_agg_df <- consump_df %>%
        select(country.x, country_code, year_start, grp, confidence_lvl) %>%
        distinct() %>%
        left_join(. , ca_sum_bycountry, 
                  by = c("country_code"="country_code","year_start"="year")) %>%
        left_join(. , aq_sum_bycountry, 
                  by = c("country_code"="country_code","year_start"="year"))  %>%
        select(-one_of('CountryCountry.x','CountryCountry.y')) %>%
        mutate(sum_aquacul = sum_aquacul / 10^6, 
               sum_catch = sum_catch / 10^6)

# change NAs to zeros
consump_nat_agg_df[is.na(consump_nat_agg_df)] <- 0

# remove temp data from env
rm(ca_sum_bycountry, aq_sum_bycountry)#, drop_cols, vars, tr_sum_bycountry)





# loop through the consumption rows (i.e. countries) ------------------------------------------------

for (j in seq(nrow(consump_nat_agg_df))) {

  # get country_code of current loop
  temp_cntry_code <- consump_nat_agg_df[j,'country_code']
  
  # get scalar value for aquaculture
  aq <- consump_nat_agg_df[j,'sum_aquacul']
  
  # get vector of consumption
  cons <- c(cntry_sum_prod_fwae_mc[[temp_cntry_code]])
  
  # get Monte Carlo vector of exports 
  if (class(cntry_sum_exp_fwae_mc[[temp_cntry_code]])!="NULL"){
    exp <- c(cntry_sum_exp_fwae_mc[[temp_cntry_code]])
  } else {exp <- rep(0,10000)}
  
  # get Monte Carlo vector of exports 
  if (class(cntry_sum_imp_fwae_mc[[temp_cntry_code]])!="NULL"){
    imp <- cntry_sum_imp_fwae_mc[[temp_cntry_code]]
  } else {imp <- rep(0,10000)}
  
  
  # calculate the correction of the survey catch  ------------------------------
  tr_corr_consump <- cons - imp + exp - aq
  
  # add FWAE uncertainty from import and export
  uncert_fwae_mc <- c(cntry_sum_uncert_fwae_mc[[temp_cntry_code]])
  uncert_fwae_mc <- uncert_fwae_mc - imp + exp
  
  uncert_prodfm_ur <- c(cntry_sum_uncert_prodfm_mc[[temp_cntry_code]])

  
  
  # Calculate the expectation and uncertainty range ----------------------------
  # extract the 2.5%, mean, 97.5% 
  tr_corr_ur <- as.data.frame(c(summary(mcdata(tr_corr_consump))))[c(4,1,8)]
  colnames(tr_corr_ur) <- c("p025","mean","p975")
  tr_corr_ur$datatype <- "tr_corr_consump" 
  
  cons_pretrcorr <- as.data.frame(c(summary(mcdata(cons))))[c(4,1,8)]
  colnames(cons_pretrcorr) <- c("p025","mean","p975")
  cons_pretrcorr$datatype <- "cons_pretrcorr" 
  
  imp_ur <- as.data.frame(c(summary(mcdata(imp))))[c(4,1,8)]
  colnames(imp_ur) <- c("p025","mean","p975")
  imp_ur$datatype <- "import"
  
  exp_ur <- as.data.frame(c(summary(mcdata(exp))))[c(4,1,8)]
  colnames(exp_ur) <- c("p025","mean","p975")
  exp_ur$datatype <- "export" 
  
  
  
  
  # calc source-specific uncertainty range and add it to the table
  uncert_fwae_ur <- as.data.frame(c(summary(mcdata(uncert_fwae_mc))))[c(4,1,8)]
  colnames(uncert_fwae_ur) <- c("p025","mean","p975")
  uncert_fwae_ur$datatype <- "uncert_fwae_ur" 
  
  uncert_prodfm_ur <- as.data.frame(c(summary(mcdata(uncert_prodfm_ur))))[c(4,1,8)]
  colnames(uncert_prodfm_ur) <- c("p025","mean","p975") 
  uncert_prodfm_ur$datatype <- "uncert_prodfm_ur" 
  
  
  
  ur_outdf_temp <- rbind(cons_pretrcorr, tr_corr_ur, imp_ur, exp_ur, 
                         c(NA, consump_nat_agg_df[j,'sum_catch'], NA,'sum_catch'),
                         c(NA, consump_nat_agg_df[j,'sum_aquacul'], NA,'sum_aquacul'),
                         uncert_fwae_ur, uncert_prodfm_ur)
  
  ur_outdf_temp$country_code <- temp_cntry_code
  #colnames(ur_outrow) <- gsub("\\.","", colnames(ur_outrow))

  # bind output df for each country to a single output df
  if (j == 1){
    out_ur_df <- ur_outdf_temp
  } else {out_ur_df <- rbind(out_ur_df, ur_outdf_temp)}
  rownames(out_ur_df) <- NULL
}


rm(aq, cons, exp, imp, j, temp_cntry_code, exp_ur, imp_ur, tr_corr_ur, 
   ur_outdf_temp, uncert_fwae_ur, uncert_prodfm_ur, tr_corr_consump, uncert_fwae_mc, cons_pretrcorr)







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



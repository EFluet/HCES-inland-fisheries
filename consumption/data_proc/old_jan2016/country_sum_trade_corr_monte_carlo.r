# import package
library(tidyr)
library(dplyr)
library(mc2d) # monte carlo
# fitdistrplus package is convenient for assessing a parametric distribution of data


### Read input files -----------------------------------------------------------
# read the code-assigned product list
consump_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods_fwaes_uncert.csv',
                       stringsAsFactors=FALSE)

# sum consumption per country
consump_df <- consump_df %>% 
              filter(confidence_lvl == 'high' | confidence_lvl == 'medium') %>%
              filter(prod_src_fm != 'M')

# create list of unique country code
uniq_cntry <- unique(consump_df$country_code)

# Sets or retrieves the default number of simulations.
ndvar(10000)

# loop through countries
for (j in seq(uniq_cntry)) {
  
  # make temp df only of looped-country
  temp_consump_df <-  consump_df %>% filter(country_code == uniq_cntry[j])

  # loop through products of the country
  for (i in seq(nrow(temp_consump_df))) {
   
    # Creates a mcnode object using the stochastic sample generating function.
    prod_fwae_mc <- mcstoc(rtriang, 
                       min=  temp_consump_df[i,'min_conv_fac'], 
                       mode= temp_consump_df[i,'mean_conv_fac'], 
                       max=  temp_consump_df[i,'max_conv_fac']) 
    
    # multiply product weight to mc factors
    prod_fwae_mc <- prod_fwae_mc * temp_consump_df[i,'cons_miltonspyr']
  
    # sum the products per country
    if (i==1) {sum_prod_fwae_mc <- prod_fwae_mc}
    else {sum_prod_fwae_mc <- sum_prod_fwae_mc + prod_fwae_mc}
    }
  
  # append the mcnode into a mc object
  if (j==1) {cntry_sum_prod_fwae_mc <- mc(sum_prod_fwae_mc)}
  else {cntry_sum_prod_fwae_mc <- mc(cntry_sum_prod_fwae_mc, sum_prod_fwae_mc)}
  
  # name the mcnode of 
  names(cntry_sum_prod_fwae_mc)[j] <- uniq_cntry[j]

  # assign(eval(uniq_cntry[j]), sum_prod_fwae_mc)
  # if (j==1) {cntry_sum_prod_fwae_mc <- mc(get(eval(uniq_cntry[j])))
  # } else {cntry_sum_prod_fwae_mc <- mc(cntry_sum_prod_fwae_mc, get(eval(uniq_cntry[j])))}
}

# remove temp objects
rm(temp_consump_df, sum_prod_fwae_mc, prod_fwae_mc, j, i)


#########################################################
#########################################################


### Join imports, exports, aquaculture and catch to consumption data -----------

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

# change NAs to 0
consump_nat_agg_df[is.na(consump_nat_agg_df)] <- 0

# remove temp data from env
rm(ca_sum_bycountry, aq_sum_bycountry)#, drop_cols, vars, tr_sum_bycountry)


#  ------------------------------------------------

surv_catch_corr <- data.frame(country_code = character(),
                              surv_catch_corr_2.5 = numeric(),
                              surv_catch_corr_mean = numeric(),
                              surv_catch_corr_97.5 = numeric())



for (j in seq(nrow(consump_nat_agg_df))) {

  # get country_code of 
  temp_cntry_code <- consump_nat_agg_df[j,'country_code']
  
  cons <- c(cntry_sum_prod_fwae_mc[[temp_cntry_code]])
  aq <- consump_nat_agg_df[j,'sum_aquacul']
  
  # get vector of exports
  if (class(cntry_sum_exp_fwae_mc[[temp_cntry_code]])!="NULL"){
    exp <- c(cntry_sum_exp_fwae_mc[[temp_cntry_code]])
  } else {exp <- rep(0,10000)}
  
  # get vector of imports
  if (class(cntry_sum_imp_fwae_mc[[temp_cntry_code]])!="NULL"){
    imp <- cntry_sum_imp_fwae_mc[[temp_cntry_code]]
  } else {imp <- rep(0,10000)}
  
  # this is the core of it - calculate the correction of the survey catch 
  tr_corr_consump <- cons - imp + exp - aq

  # add the tr corrected catch into a mc object (for histograms)
  # mc(mcdata(tr_corr_consump))
    
  # extract the 2.5%, mean, 97.5% 
  surv_catch_corr_outtrow <- as.data.frame(c(temp_cntry_code, as.data.frame(c(summary(mcdata(tr_corr_consump))))[c(4,1,8)]))
  names(surv_catch_corr_outtrow) <- c('country_code', 
                                      'surv_catch_corr_2.5', 
                                      'surv_catch_corr_mean', 
                                      'surv_catch_corr_97.5')
  
  surv_catch_corr_outtrow$country_code <-   as.character(surv_catch_corr_outtrow$country_code)
  surv_catch_corr <- rbind.data.frame(surv_catch_corr, surv_catch_corr_outtrow)
  
}


rm(aq, cons, exp, imp, j, temp_cntry_code, surv_catch_corr_outtrow)


#
consump_nat_agg_df <- consump_nat_agg_df %>% 
                      left_join(., surv_catch_corr, by='country_code') 


### Write the table to csv file ------------------------------------------------
write.csv(consump_nat_agg_df, '../output/consumption/hh_survey_consump_nat_agg_prod_fw_mc.csv')



consump_nat_agg_df_forplot <- consump_nat_agg_df %>%
                      gather(a , b, sum_catch, surv_catch_corr_mean)


ggplot(consump_nat_agg_df_forplot, aes(x = country.x,
                                       y = b,
                                       fill=a)) +
      geom_bar(stat="identity", position='dodge') + coord_flip()

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



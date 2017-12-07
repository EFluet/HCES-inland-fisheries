# import package
source('./consumption/import_libraries.r')

# wrapper function that checks if variable object exists or not; return logical
exist <- function(x) { return(exists(deparse(substitute(x))))}




# Read input files -----------------------------------------------------------
# read the code-assigned product list
consump_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods_refuse_fwaes.csv', 
                       stringsAsFactors=FALSE)


# count the number of F/M products per country  to filter countries ------------
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


# filter the consumption data to keep only countries meeting 
# the F + F/M criteria also filter for  NA in critical fields
consump_df <- consump_df %>% 
      # filter(country.x %in% prod_src_fm_cnt_perctry$country.x) %>%
      # remove Afghanistan - currently missing population data
      filter(!is.na(consump_million.tons.yr))
      
### the calc of the prob kernel comes here 
#source('./consumption/data_proc/country_geo_pos_frac_kernel.r')
source('./consumption/data_proc/FtoM_appconsump_v2.r')




# remove products listed as marine
consump_df <- consump_df %>% 
      filter(prod_src_fm != 'M') %>%
      mutate(cons_miltonspyr = consump_million.tons.yr) %>%
      left_join(., appcon_unfilt, by=c('country_code'='country_code')) %>% 
      select(-one_of('X','X.1','X.2',"prod_sys_cf","prod_trade_di",
                 "comment_or_assumption","coder","other_comment","source"))



rm(appcon, prod_src_fm_cnt_perctry)



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

  # estimate the F/M ratio for each country
  temp_unique_clpa <- unique(temp_consump_df[,'clpa'])
  tot_fm_boot_preds <- fx(temp_unique_clpa, a=boots[,"a"], b=boots[,"b"])

  
  # update the F/M ratio based on known products
  if (nrow(subset(temp_consump_df, prod_src_fm=='F'))>0){
    W_f <- sum(subset(temp_consump_df, prod_src_fm=='F')['Averageediblequantityconsumedgpersonday'])
  } else W_f <- 0
  
  if (nrow(subset(temp_consump_df, prod_src_fm=='M'))>0){
    W_m <- sum(subset(temp_consump_df, prod_src_fm=='M')['Averageediblequantityconsumedgpersonday'])
  } else W_m <- 0

  if (nrow(subset(temp_consump_df, prod_src_fm=='F/M'))>0){
    W_u <- sum(subset(temp_consump_df, prod_src_fm=='F/M')['Averageediblequantityconsumedgpersonday'])
  } else W_u <- 0
  
  W_t <- sum(subset(temp_consump_df)['Averageediblequantityconsumedgpersonday'])
  

  
  if (W_u != W_t){  
    W_k <- (W_m + W_f)
    unres_boot_preds <- (W_t * tot_fm_boot_preds - W_k * (W_f/(W_f+W_m))) / W_u
    }else{unres_boot_preds <- tot_fm_boot_preds}
  
  #rm(W_t, W_k , W_f, W_m, W_u)
  
  
  
  # loop through products of the country
  for (i in seq(nrow(temp_consump_df))) {
   
    # create object of current product
    cur_fwae_prod <-temp_consump_df[i,'product']
    
    # for unresolved products create distribution of F/M ratio, otherwise, value of 1
    if (temp_consump_df[i,'prod_src_fm'] == 'F/M') {

      
      prod_fm_unif <- mcstoc(rempiricalD, n=10000,  values= tot_fm_boot_preds, prob=NULL)
        # mcstoc(runif, min=temp_consump_df[i,'F_tototal'], 
        #                      max=temp_consump_df[i,'F_tototal'])
    }else{prod_fm_unif <- mcstoc(runif, min=1, max=1)}

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


    ### sum the products per country --------------------------------------------
    # if first product look, creat the sum MC object
    if (i==1) {
      sum_prod_fwae_mc <- prod_fwae_mc
      sum_uncert_prodfm_mc <- uncert_prodfm_mc}
    
    # create sum uncert obj for assumed FWAE if doesn't exist
    if (exist(sum_uncert_fwae_assumed_mc)==FALSE & is.na(cur_fwae_prod)){
        sum_uncert_fwae_assumed_mc <- uncert_fwae_assumed_mc}
    
    # create sum uncert obj for matched FWAE if doesn't exist
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
   
  # append mcnode of prod value (actual result) --------------------------------
  cntry_sum_prod_fwae_mc <- mc(cntry_sum_prod_fwae_mc, sum_prod_fwae_mc)
  names(cntry_sum_prod_fwae_mc)[j] <- uniq_cntry[j]
  rm(sum_prod_fwae_mc)
  
  # append node of uncertainty of F/M products
  cntry_sum_uncert_prodfm_mc <- mc(cntry_sum_uncert_prodfm_mc, sum_uncert_prodfm_mc)
  names(cntry_sum_uncert_prodfm_mc)[j] <- uniq_cntry[j]
  rm(sum_uncert_prodfm_mc)
  

  #  append to uncert of assumed FWAE products 
  # if the first country, create 
  if (exist(sum_uncert_fwae_assumed_mc)==TRUE){
    cntry_sum_uncert_fwae_assumed_mc <- mc(cntry_sum_uncert_fwae_assumed_mc, sum_uncert_fwae_assumed_mc)
    names(cntry_sum_uncert_fwae_assumed_mc)[length(cntry_sum_uncert_fwae_assumed_mc)] <- uniq_cntry[j]
    rm(sum_uncert_fwae_assumed_mc)}
  
  #  append to uncert of matched FWAE products
  # if the first country, create 
  if (exist(sum_uncert_fwae_matched_mc)==TRUE){
    cntry_sum_uncert_fwae_matched_mc <- mc(cntry_sum_uncert_fwae_matched_mc, sum_uncert_fwae_matched_mc)
    m <- length(cntry_sum_uncert_fwae_matched_mc)
    names(cntry_sum_uncert_fwae_matched_mc)[length(cntry_sum_uncert_fwae_matched_mc)] <- uniq_cntry[j]
    rm(sum_uncert_fwae_matched_mc)}
  
  # remove the sum objects
  #rm(sum_uncert_fwae_matched_mc, sum_uncert_fwae_assumed_mc, sum_uncert_prodfm_mc, sum_prod_fwae_mc)
}


# remove temp objects
rm(temp_consump_df, prod_fwae_fac_mc, j, i, uncert_prodfm_mc, 
   prod_fm_unif, uniq_cntry, prod_fwae_mc, uncert_fwae_assumed_mc, uncert_fwae_matched_mc,
   template_mc, m, cur_fwae_prod)


hist(cntry_sum_prod_fwae_mc)



# run script summing the trade with FWAE consumption
source('./consumption/data_proc/country_sum_trade_corr_mc_v2.r')




# copy the percentiles to the df with contries with FAO catch data -------------
consump_nat_agg_df_mcperc <-  consump_nat_agg_df %>% 
      select(-one_of('sum_catch','sum_aquacul')) %>%
      left_join(., out_tr_sum_df, by='country_code') %>%
  
      # set uncertainty range to NA, if both are missing (or equal to eachother)
      # this prevents errorbars from needlessly appearing
      mutate(p025= ifelse(p025==p975, NA, p025),
             p975= ifelse(p025==p975, NA, p975)) %>%
  
      #mutate(p025= ifelse(p025 < 0, 0, p025)) %>%


      mutate(country_label= ifelse(year_end==0, 
                     paste(country.x,' (', year_start,')', sep=''),
                     paste(country.x,' (', year_start, '-', 
                           str_sub(year_end,-2,-1),')', sep=''))) %>%
      group_by(country.x) %>%
      #filter(min(mean) >= 0) %>%
      ungroup 
      




# Write the table to csv file ------------------------------------------------
write.csv(consump_nat_agg_df_mcperc, '../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc.csv')



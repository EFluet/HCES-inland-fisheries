# import libraries
library(dplyr)
library(tidyr)


### FWAEs w/ conv.factors - convert consumed weights into whole animal weights ------------------------

# read conversion factors table
consump_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods.csv',
                       stringsAsFactors=F)

# source function doing fwae conversion 
source('./consumption/data_proc/fcn/fcn_fwae_conv.r')

# execute function matching products to FWAE factors 
prod_conv_factors <-  fwae_conversion(consump_df$prod_name)

# add factor to the product list
consump_df <- cbind(consump_df, prod_conv_factors)


### Monte Carlo resampling ---------------------------
# import package
library(mc2d) # monte carlo
# fitdistrplus package is convenient for assessing a parametric distribution of data




modelEC3 <- mcmodel({
  # Sets or retrieves the default number of simulations.
  ndvar(10000)
  
  # create distribution based on the min,mean,max of each product.
  # Creates a mcnode object using a random generating function.
  prod_fwae_mcnode <- mcstoc(rtriang, min=1.25, mode=2.45, max=2.955) 
  
  
  a <- c(prod_fwae_mcnode)
  
  b <- cbind(a, a)
  c <- as.data.frame(t(b))
  
  })

modelEC3




# loop through rows of products

# create distribution based on the min,mean,max of each product.
# Creates a mcnode object using a random generating function.
prod_fwae_mcnode <- mcstoc(rtriang, min=1.25, mode=2.45, max=2.955) 

# multiply prod weight with mc factors
prod_fwae_mcnode <- prod_fwae_mcnode * 


# add the conversion factor to 

  
# sum products per country

  
ec_ab <- mc(ec_ab, fwae_fac)  

et = sapply(ec_ab, sd)


a <- mcstoc(rtriang, min=3, mode=4, max=5) 
b <- mcstoc(rtriang, min=1, mode=2, max=3) 

ec_ab <- mc(ec_ab, fwae_fac)  

et = sapply(ec_ab, sd)

c <- a - b












### calculate to new columns and apply fwae factors ---------------------------
consump_df <- consump_df %>% 
  
  #left_join(., conv_fwae, by=c('product')) %>% 
  
  # rename columns
  mutate(cons_miltonspyr = consump_million.tons.yr) %>%  
  mutate(cons_miltonspyr_fm_min = cons_miltonspyr,
         cons_miltonspyr_fm_mean= cons_miltonspyr,
         cons_miltonspyr_fm_max = cons_miltonspyr) %>%
  
  # apply the 0, 50, 100% to the F/M products
  mutate(cons_miltonspyr_fm_min = ifelse(prod_src_fm == 'F/M', 0, cons_miltonspyr_fm_min),
         cons_miltonspyr_fm_mean = ifelse(prod_src_fm == 'F/M', cons_miltonspyr/2, cons_miltonspyr_fm_mean),
         cons_miltonspyr_fm_max = ifelse(prod_src_fm == 'F/M', cons_miltonspyr, cons_miltonspyr_fm_max)) %>%
  
  # set marine consumption to 0
  mutate(cons_miltonspyr_fm_min = ifelse(prod_src_fm == 'M', 0, cons_miltonspyr_fm_min),
         cons_miltonspyr_fm_mean = ifelse(prod_src_fm =='M', 0, cons_miltonspyr_fm_mean),
         cons_miltonspyr_fm_max = ifelse(prod_src_fm == 'M', 0, cons_miltonspyr_fm_max)) %>%
  
  # apply fwae to consumed weights
  # min (or max) factor is applied to min (or max) consumption to expand the range of estimate
  mutate(cons_miltonspyr_fm_min_fwaes = min_conv_fac * cons_miltonspyr_fm_min,
         cons_miltonspyr_fm_mean_fwaes = mean_conv_fac * cons_miltonspyr_fm_mean,
         cons_miltonspyr_fm_max_fwaes = max_conv_fac * cons_miltonspyr_fm_max)




# remove unneeded object from environment
rm(pattern, y, rm_str, conv_fwae)


# copy country names (for consistency with nat_agg df)
# rename columns after join added some subfix
#consump_df$country <- countrycode(consump_df$country_code,'iso3c','country.name',warn=TRUE)
colnames(consump_df)[colnames(consump_df) == 'country_code.y'] <- 'country_code'
colnames(consump_df)[colnames(consump_df) == 'country.y'] <- 'country'
consump_df$country <- consump_df$country.x

### Write the table to csv file ------------------------------------------------
write.csv(consump_df, '../output/consumption/hh_survey_consump_sep_prods_fwaes_uncert.csv')


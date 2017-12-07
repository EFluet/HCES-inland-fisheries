# Calculates the difference between survey derived catch and FAO statistics

# Tricky column names to remember - will make more explicit
# sum_catch is from 2012
# sum_catch_millions is from survey year


### Import modules -------------------------------------------------------------
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(stringr)
options("scipen"=100, "digits"= 8)


### Read the data consumption table ----------------------------------------------
consump_nat_agg_df <- read.csv('../output/consumption/hh_survey_consump_nat_agg_prod.csv')



### calculate diff between survey & FAO data as percentage difference -----------------------------------
consump_nat_agg_df <- consump_nat_agg_df %>%
      
      # rename colmuns with shorter names
      mutate(surv_catch = consump_million.tons.yr_fwaes_mf50_noimp, 
             surv_catch_rng = consump_million.tons.yr_fwaes_mf50_rng) %>%
      
  
      # calculate percentage difference between the survey and FAO catch
      mutate(prc_dif = surv_catch/sum_catch_millions,
             prc_dif_lwr = (surv_catch - surv_catch_rng)/sum_catch_millions,
             prc_dif_upr = (surv_catch + surv_catch_rng)/sum_catch_millions) %>%
      

      # delete unneeded columns, and those replaced with shorter colname
      select(-one_of(c('X','country.x','country.y','source',
                       'consump_million.tons.yr',
                       'consump_million.tons.yr_fwaes',
                       'consump_million.tons.yr_fwaes_mf50',
                       'consump_million.tons.yr_fwaes_mf50_rng',
                       'consump_million.tons.yr_fwaes_mf50_noimp')))
  
      # keep only rows with positive catch
      #filter(prc_dif_lwr > 0)



### Join the FAO catch for all countries for 2012 ------------------------------
source('./fishstatj_data/join_nat_catch_pop_wat_2012.R')

# remove duplicate countries (e.g. china & china,mainland)
prod_percap_df <- prod_percap_df %>%
                  distinct(country_code) %>%
                  mutate(sum_catch = sum_catch/1000000)


# join consump to entire country list 
ca_ctry_jnd <-  left_join(prod_percap_df, consump_nat_agg_df, 
                         by=c('country_code'='country_code')) %>%
            
  # calcualte the substituted catch for the same year (2012)
  mutate(subst_catch_mid= ifelse(!is.na(surv_catch), sum_catch*prc_dif, NA),
         subst_catch_lwr= ifelse(!is.na(surv_catch), sum_catch*prc_dif_lwr, NA),
         subst_catch_upr= ifelse(!is.na(surv_catch), sum_catch*prc_dif_upr, NA))

# remove var from env.
#rm(consump_nat_agg_df, prod_percap_df)



### summary table of catch (of countries with surv.catch > 0 ) -----------------

# remove countries with errorbar going into negatives
ca_ctry_jnd_forsum <-  ca_ctry_jnd %>% filter(prc_dif_lwr > 0)

# calculate sum of each columns, transpose to maintain columns
sums_df <- as.data.frame(t(colSums(Filter(is.numeric, ca_ctry_jnd_forsum), na.rm = TRUE)))




# calculate total difference from summed columns
sums_df <- sums_df %>%
  
     # keep only columns that can be summed (not much sense in summing up percentages)
     select(sum_catch, sum_catch_millions, surv_catch, surv_catch_rng, year_start,
            subst_catch_mid, subst_catch_lwr, subst_catch_upr) %>%

     # calculate percentage difference from the sums of columns
     mutate(subst_catch_rng = subst_catch_mid - subst_catch_lwr,
            perc_fao_catch_in_surv_countries = sum_catch_millions/sum_catch,
            perc_diff_fao_subst_mid = subst_catch_mid/sum_catch_millions,
            perc_diff_fao_subst_rng = (subst_catch_mid-subst_catch_lwr)/sum_catch_millions) %>%
    
     mutate(sum_catch2012 = sum_catch,
            sum_catch_compositeyr = sum_catch_millions)



## Print important statistics ---------------------------


sums_df$sum_catch2012  # this is FAO catch in 2012
sums_df$sum_catch_compositeyr  # this is FAO catch composite of different years of surveys 
sums_df$surv_catch



# calculate percentage difference in total 
sum(ca_ctry_jnd_forsum$sum_catch_millions, na.rm=T) / sum(ca_ctry_jnd$sum_catch)

# this is the medium perc diff between survey and 
sums_df$perc_diff_fao_subst_mid

sums_df$perc_diff_fao_subst_rng


# write summary table to csv ---------------------------------------------------
# format of 2 column list: descriptor and value
write.csv(ca_ctry_jnd_forsum, './ca_ctry_jnd_forsum.csv')












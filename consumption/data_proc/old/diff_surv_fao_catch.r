### Calculate difference between survey catch and FAO catch
### Difference in tons and percentage, for range of estimates



### import packages ------------------------------------------------------------
library(dplyr)



### read combined product file -------------------------------------------------
f <- '../output/consumption/prod_coded_n_agg/hh_survey_consump_nat_agg_prod_combined.csv'
prod_agg_comb <- read.csv(f, stringsAsFactors=FALSE)
rm(f)


### calculate diff between survey & FAO data as percentage difference ----------
prod_agg_comb <- prod_agg_comb %>%
  
  # rename colmuns with shorter names
  mutate(surv_catch = consump_million.tons.yr_fwaes_mf50_noimp, 
         surv_catch_rng = consump_million.tons.yr_fwaes_mf50_rng) %>%
  
  
  # calculate percentage difference between the survey and FAO catch
  mutate(tons_dif= surv_catch-sum_catch_millions,
         tons_dif_lwr= surv_catch - surv_catch_rng -sum_catch_millions,
         tons_dif_upr= surv_catch + surv_catch_rng -sum_catch_millions) %>%
  
  mutate(prc_dif = tons_dif/sum_catch_millions,
         prc_dif_lwr = tons_dif_lwr/sum_catch_millions,
         prc_dif_upr = tons_dif_upr/sum_catch_millions) %>%
  
  # delete unneeded columns, and those replaced with shorter colname
  select(-one_of(c('X','X.1','country.x','country.y','source','CountryCountry',
                   'consump_million.tons.yr',
                   'consump_million.tons.yr_fwaes',
                   'consump_million.tons.yr_fwaes_mf50',
                   'consump_million.tons.yr_fwaes_mf50_rng',
                   'consump_million.tons.yr_fwaes_mf50_noimp')))

  # keep only rows with positive catch
  #filter(prc_dif_lwr > 0)



### write combined df to new file -----------------------------------------------------
file_path <- "../output/consumption/prod_coded_n_agg/"
write.csv(prod_agg_comb, paste(file_path,'hh_survey_consump_nat_agg_prod_combined_diff.csv',sep=''))




# calcualte sums of catch for per source and conf_lvl groups of countries ----------------------
# (conf_lvl & prod_separation)
prod_agg_comb_sums <- prod_agg_comb %>%
      filter(surv_catch > 0,
             !is.na(sum_catch_millions)) %>%

      group_by(prod_src, confidence_lvl) %>%
      summarise(surv_catch= sum(surv_catch),
                surv_catch_rng= sum(surv_catch_rng),
                sum_catch_millions= sum(sum_catch_millions))

#group_by(grp) %>% summarise_each(funs(mean))



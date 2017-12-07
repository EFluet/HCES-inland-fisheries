### Calculate difference between survey catch and FAO catch
### Difference in tons and percentage, for range of estimates



### import packages ------------------------------------------------------------
library(dplyr)



### read combined product file -------------------------------------------------
f <- '../output/consumption/hh_survey_consump_nat_agg_prod_fw_uncert_tradecorr.csv'
prod_agg_comb <- read.csv(f, stringsAsFactors=FALSE)
rm(f)

### calculate diff between survey & FAO data as percentage difference ----------
prod_agg_comb <- prod_agg_comb %>%
  
  # rename colmuns with shorter names
  mutate(surv_catch_min = cons_miltonspyr_fm_min_fwaes_notr, 
         surv_catch_mean = cons_miltonspyr_fm_mean_fwaes_notr,
         surv_catch_max = cons_miltonspyr_fm_max_fwaes_notr) %>%
  
  
  # calculate percentage difference between the survey and FAO catch
  mutate(tons_dif_min = surv_catch_min - catch_miltons,
         tons_dif_mean = surv_catch_mean - catch_miltons,
         tons_dif_max = surv_catch_max - catch_miltons) %>%
  
  mutate(prc_dif_min =  tons_dif_min  / catch_miltons,
         prc_dif_mean = tons_dif_mean / catch_miltons,
         prc_dif_max =  tons_dif_max  / catch_miltons) %>%
  
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
file_path <- "../output/consumption/prod_coded_n_agg/hh_survey_consump_nat_agg_prod_fw_uncert_tradecorr_diff.csv"
write.csv(prod_agg_comb, file_path)






### Write the table to csv file ------------------------------------------------
consump_nat_agg_df <- read.csv('../output/consumption/hh_survey_consump_nat_agg_prod_fw_uncert_tradecorr_tr_fwae.csv')

cols_to_rm <- c("X","country","country_code","year_start","cons_miltonspyr","cons_miltonspyr_fm_min","confidence_lvl", 'source.y',
                "sum_catch","type.x","source.y","sum_aquacul","type.y","source",
                "cons_miltonspyr_fm_mean", "cons_miltonspyr_fm_max", "grp","lifd_2015",
                "source.x","source","continent","type.x","type.y")

min_cols <- c("Export.sum_tr_fwae_min", 
              "Import.sum_tr_fwae_min",
              "cons_miltonspyr_fm_min_fwaes",
              "cons_miltonspyr_fm_min_fwaes_notr")

mean_cols <- c("aquacult_miltons",
               "catch_miltons", 
               "Export.sum_tr_fwae_mean",
               "Import.sum_tr_fwae_mean",
               "cons_miltonspyr_fm_mean_fwaes", 
               "cons_miltonspyr_fm_mean_fwaes_notr")

max_cols <- c("Export.sum_tr_fwae_max", 
              "Import.sum_tr_fwae_max",
              "cons_miltonspyr_fm_max_fwaes",
              "cons_miltonspyr_fm_max_fwaes_notr")
              
  


consump_nat_agg_df <- consump_nat_agg_df %>%
  
  select(-one_of(cols_to_rm)) %>%
  gather(col_names, value, cons_miltonspyr_fm_min_fwaes:cons_miltonspyr_fm_max_fwaes_notr) %>%
  mutate(value_grping = ifelse(col_names %in% min_cols, 'min_value', NA),
         value_grping = ifelse(col_names %in% mean_cols, 'mean_value', value_grping),
         value_grping = ifelse(col_names %in% max_cols, 'max_value', value_grping)) %>%

  # rename columns names for nicer plot labels
  mutate(col_names = ifelse(col_names=='Export.sum_tr_fwae_min'|
                            col_names=='Export.sum_tr_fwae_mean'|
                            col_names=='Export.sum_tr_fwae_max' ,'Freshwater Export (FishStatJ)',col_names),
         
         col_names = ifelse(col_names=='Import.sum_tr_fwae_min'|
                              col_names=='Import.sum_tr_fwae_mean'|
                              col_names=='Import.sum_tr_fwae_max','Freshwater Import (FishStatJ)',col_names),
         
         col_names = ifelse(col_names=='catch_miltons','Inland Catch (FishStatJ)',col_names),
         
         col_names = ifelse(col_names=='cons_miltonspyr_fm_min_fwaes' |
                            col_names=='cons_miltonspyr_fm_mean_fwaes'|
                            col_names=='cons_miltonspyr_fm_max_fwaes','HH Survey consumption (FWEA)',col_names),
         
         col_names = ifelse(col_names=='cons_miltonspyr_fm_min_fwaes_notr'|
                            col_names=='cons_miltonspyr_fm_mean_fwaes_notr'|
                            col_names=='cons_miltonspyr_fm_max_fwaes_notr','HH Survey consumpion (FWEA, trade corr.)',col_names),
         
         col_names = ifelse(col_names=='aquacult_miltons','Freshwater aquaculture production (FishStatJ)',col_names)) %>%
  
  spread(value_grping, value) %>%
  
  # set uncertainty range to NA, if both are missing (or equal to eachother)
  # this prevents errorbars from needlessly appearing
  mutate(min_value= ifelse(min_value==max_value, NA, min_value),
         max_value= ifelse(min_value==max_value, NA, max_value)) %>%
  
  mutate(country = country.x)



### -----------------
write.csv(consump_nat_agg_df, '../output/consumption/hh_survey_consump_nat_agg_prod_fw_uncert_tradecorr_tr_fwae_prepforplotting.csv')

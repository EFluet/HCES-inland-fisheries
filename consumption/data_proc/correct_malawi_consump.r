
library(dplyr)


### Write the table to csv file ------------------------------------------------
#consump_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods.csv')


consump_sum_malawi <-  consump_df %>%
                      filter(country.x == 'Malawi') %>%
                      summarise(sum_consump = sum(Averageediblequantityconsumedgpersonday)) %>%
                      as.numeric()



consump_df <- consump_df %>%
  mutate(Averageediblequantityconsumedgpersonday = ifelse(country.x == 'Malawi',
                                                         (Averageediblequantityconsumedgpersonday/consump_sum_malawi)*32, 
                                                         Averageediblequantityconsumedgpersonday))


rm(consump_sum_malawi)
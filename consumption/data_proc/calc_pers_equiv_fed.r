# the person 


### get  countries w/ surveys  ----------------------------------------
# keep only diff rows from survey df
#surv_data_diff <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_diff.csv')
surv_data_diff<- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv')

surv_data_diff <- surv_data_diff %>%
  dplyr::select(country_code, tr_corr_consump, sum_catch, mean_dif) %>%
  distinct() %>%
  filter(tr_corr_consump > 0) %>%
  mutate(hces_surv_asprcfao = tr_corr_consump/sum_catch *100)



# read the code-assigned product list ---------------------
consump_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods_refuse_fwaes.csv', 
                       stringsAsFactors=FALSE)

consump_df <- consump_df %>%
  
  # calculate the per capita consumption based before refuse
  mutate(Averageediblequantityconsumedgpersonday_wrefuse=  
           Averageediblequantityconsumedgpersonday/ (1-refuse_factor/100)) %>%
  
  
  # calculate the weighted average FWAE conversion factor
  # Goal: get average factors to apply to production to infer breakdown of consumption
  mutate(weighted_mean_fwae = mean_conv_fac * Averageediblequantityconsumedgpersonday_wrefuse,  
         weighted_min_fwae = min_conv_fac * Averageediblequantityconsumedgpersonday_wrefuse,
         weighted_max_fwae = max_conv_fac * Averageediblequantityconsumedgpersonday_wrefuse) %>%
  
  # for each country, back calculate the 
  group_by(country_code) %>%
  summarise(allsource_consump_kgyr_percap = sum(Averageediblequantityconsumedgpersonday_wrefuse / 1000 *365),
            tot_pop_both_sexes = mean(tot_pop_both_sexes),
            weighted_mean_fwae = sum(weighted_mean_fwae)/sum(Averageediblequantityconsumedgpersonday_wrefuse),
            weighted_min_fwae = sum(weighted_min_fwae)/sum(Averageediblequantityconsumedgpersonday_wrefuse),
            weighted_max_fwae = sum(weighted_max_fwae)/sum(Averageediblequantityconsumedgpersonday_wrefuse)) 



### to do the error sensitivity properly

# loop through products,
# draw the triangle distribution,
# multiply by weight
# sum per country
# divide by total country weight
# get mean & 95 intervals






# ~~~ calc person equivalent of fish protein ----------------------------------------------
surv_data_persequiv<- surv_data_diff %>%
  
  left_join(., consump_df, by='country_code') %>%
  
  # convert production to kilograms
  mutate(mean_dif_kg = mean_dif * 10^6 * 10^3,
         fao_sum_catch_kg = sum_catch* 10^6 * 10^3,
         tr_corr_consump_kg = tr_corr_consump* 10^6 * 10^3,
         
         # calculate the pers equiv; original and revised
         meandif_millionpersequiv = (mean_dif_kg / weighted_mean_fwae) / allsource_consump_kgyr_percap / 10^6,
         fao_sum_catch_millionpersequiv = (fao_sum_catch_kg / weighted_mean_fwae) / allsource_consump_kgyr_percap / 10^6,
         tr_corr_consump_millionpersequiv = (tr_corr_consump_kg / weighted_mean_fwae) / allsource_consump_kgyr_percap / 10^6,
         tot_pop_both_sexes_million = tot_pop_both_sexes / 10^6) %>%
  
  # calculate the difference as percentage of population
  mutate(persequiv_popperc = meandif_millionpersequiv/tot_pop_both_sexes*100) %>%
    
  # label countries where pers equiv is larger than population
  mutate(persequiv_dir = ifelse(fao_sum_catch_millionpersequiv > tot_pop_both_sexes/10^6,
                                "fao larger than pop", NA)) %>%
  
  # and remove
  filter(is.na(persequiv_dir ))



# ~~~ calc global totals -------------------------------------------------------------------

sum(surv_data_persequiv$meandif_millionpersequiv)
sum(surv_data_persequiv$tr_corr_consump_millionpersequiv)
sum(surv_data_persequiv$fao_sum_catch_millionpersequiv)
sum(surv_data_persequiv$tot_pop_both_sexes_million)

# Sum only positive person equivalent
sum(surv_data_persequiv[, 'meandif_millionpersequiv'])
# Sum only positive person equivalent
sum(surv_data_persequiv[which(surv_data_persequiv[,'meandif_millionpersequiv']>0), 'meandif_millionpersequiv'])
sum(surv_data_persequiv[which(surv_data_persequiv[,'meandif_millionpersequiv']<0), 'meandif_millionpersequiv'])


sum(surv_data_persequiv[, 'fao_sum_catch_millionpersequiv'])
sum(surv_data_persequiv[, 'tr_corr_consump_millionpersequiv'])





#  calc person equiv for animal protein -----------------------------------
anim_protein <- read.csv('../data/consumption/animal_protein_frompete/Pete calculations from PNAS paper for protein supply.csv')
anim_protein <- anim_protein[,c(1,3,4, 6,10, 11)]


anim_protein<- anim_protein %>%
  
  # calc inland fish as percentage of animal protein  
  mutate(infish_perc_animprotein = FW.Protein.consumption.from.Capture.percap_g_day..from.Cathy./
           Protein.Consumption.from.all.animals.percap_g_day..from.Cathy. * 100) %>%
  
  # calc pers equiv of inland fish (from McIntyre 2016)   
  mutate(pers_equiv = infish_perc_animprotein /100 * X2010_Popx1000.from.Cathy.) %>%
  
  # Add country code, for group_by and later for joining 
  mutate(country_code = countrycode(Country,'country.name','iso3c',warn=F)) %>%
  
  left_join(., surv_data_diff, by='country_code') %>%
  
  #scale the inland protein with the catch under-reporting ratio
  mutate(diff_in_protein = (FW.Protein.consumption.from.Capture.percap_g_day..from.Cathy. *  
                              hces_surv_asprcfao/100) - FW.Protein.consumption.from.Capture.percap_g_day..from.Cathy.) %>%
  
  # remove zeros and infinity
  mutate(diff_in_protein = ifelse(is.na(diff_in_protein), 0, diff_in_protein)) %>%
  filter(diff_in_protein < 100) %>%
  
  # calculate revised person equivalence
  mutate(rev_pers_equiv = (FW.Protein.consumption.from.Capture.percap_g_day..from.Cathy. + diff_in_protein) / 
           (Protein.Consumption.from.all.animals.percap_g_day..from.Cathy. + diff_in_protein ) * X2010_Popx1000.from.Cathy.)  %>%
  
  mutate(diff_pers_equiv = rev_pers_equiv - pers_equiv)





# ~~~ sum test this sums to pete's calc of 157 million ------------------------  
sum(anim_protein$pers_equiv)/1000
sum(anim_protein$rev_pers_equiv)/1000




# calculate protein for the survey countries 
anim_protein_42 <-  anim_protein %>%
                    filter(country_code %in% surv_data_diff$country_code) %>% 
                    mutate(cumul_pers_equiv = cumsum(pers_equiv)) %>%
  
                    # calculate gdp_percap
                    mutate(gdp_percap = (X2010.GDP.USDx10.6.PPP..from.Cathy. * 10^6)/ (X2010_Popx1000.from.Cathy.*10^3)) %>%
                    arrange(gdp_percap) %>%
  
                    mutate(cumul_pers_equiv = cumsum(anim_protein_42$pers_equiv)) %>%
                    mutate(rel_cumul_pers_equiv = cumul_pers_equiv/sum(anim_protein_42$pers_equiv)) %>%
                    mutate(cumul_rev_pers_equiv = cumsum(anim_protein_42$rev_pers_equiv)) %>%
                    mutate(rel_cumul_rev_pers_equiv = cumul_rev_pers_equiv/sum(anim_protein_42$rev_pers_equiv)) 






anim_protein <- anim_protein %>%
  mutate(cumul_pers_equiv = cumsum(pers_equiv)) %>%
  
  mutate(gdp_percap = (X2010.GDP.USDx10.6.PPP..from.Cathy. * 10^6)/ (X2010_Popx1000.from.Cathy.*10^3)) %>%
  arrange(gdp_percap) %>%
  mutate(cumul_pers_equiv = cumsum(anim_protein$pers_equiv)) %>%
  mutate(rel_cumul_pers_equiv = cumul_pers_equiv/sum(anim_protein$pers_equiv)) %>%
  mutate(cumul_rev_pers_equiv = cumsum(anim_protein$rev_pers_equiv)) %>%
  mutate(rel_cumul_rev_pers_equiv = cumul_rev_pers_equiv/sum(anim_protein$rev_pers_equiv)) 






# this sums to pete's calc of 157 million  
sum(anim_protein_42$pers_equiv)/1000
sum(anim_protein_42$rev_pers_equiv)/1000
# the ratio is 96/138 million pers equiv





### 
ggplot(anim_protein_42) +
  geom_step(aes(x= gdp_percap, y= rel_cumul_pers_equiv)) +
  geom_step(aes(x= gdp_percap, y= rel_cumul_rev_pers_equiv), color='red')+
  geom_vline(xintercept= max(anim_protein_42$gdp_percap), color='grey')+
  scale_x_log10()


ggplot(anim_protein) +
  geom_step(aes(x= gdp_percap, y= rel_cumul_pers_equiv)) +
  geom_step(aes(x= gdp_percap, y= rel_cumul_rev_pers_equiv), color='red')+
  geom_vline(xintercept= max(anim_protein_42$gdp_percap), color='grey')+
  scale_x_log10()




ggplot(anim_protein_42) +
  geom_step(aes(x= gdp_percap, y= cumul_pers_equiv/1000)) +
  geom_step(aes(x= gdp_percap, y= cumul_rev_pers_equiv/1000), color='red')+
  geom_vline(xintercept= max(anim_protein_42$gdp_percap), color='grey')+
  scale_x_log10()

ggplot(anim_protein) +
  geom_step(aes(x= gdp_percap, y= cumul_pers_equiv/1000)) +
  geom_step(aes(x= gdp_percap, y= cumul_rev_pers_equiv/1000), color='red')+
  geom_vline(xintercept= max(anim_protein_42$gdp_percap), color='grey')+
  scale_x_log10()






### Save figure to file --------------------------------------------------------
ggsave('../output/figures/cumul_pers_equiv_abs.png',
       width=90, height=80, dpi=800, units="mm", type = "cairo-png")
dev.off()

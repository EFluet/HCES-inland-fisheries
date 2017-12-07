

# get countries w/ surveys  ----------------------------------------
surv_data_diff<- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv')

surv_data_diff <- surv_data_diff %>%
  dplyr::select(country_code, tr_corr_consump, sum_catch, mean_dif) %>%
  distinct() %>%
  filter(tr_corr_consump > 0) %>%
  mutate(hces_surv_asprcfao = tr_corr_consump/sum_catch *100)



# get survey protein---------------------------------------------------------------
surv_protein <- read.csv('../data/consumption/survey_protein_consump_june2017.csv')


# get FAO stats aq, ca, tr  ------------------------------------------
aq_ca_tr_stat <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm.csv',
                           stringsAsFactors = FALSE)

aq_ca_tr_stat <- aq_ca_tr_stat %>%
  filter(!datatype %in% c('uncert_fwae_ur', 'uncert_prodfm_ur',
                          'uncert_fwae_matched_mc', 'uncert_fwae_assumed_mc')) %>%
  dplyr::select(-one_of("country.x","grp","X", "confidence_lvl","p025","p975")) %>%
  spread(datatype, mean) %>%
  dplyr::select(-one_of("cons_pretrcorr","tr_corr_consump")) 



# get inland fraction from gam model  ------------------------------------------
boot_pred <- read.csv('../output/bootpred_inland_consump_fraction_gam_pred.csv')
boot_pred <- boot_pred %>%
  mutate(mean_pred_inland_frac = mean) %>%
  dplyr::select(country_code, mean_pred_inland_frac)




# get apparent consumption  -----------------------------------------------------
path <- "../data/consumption/FAOSTAT_data_6-29-2017_fish_protein_apparent_consump.csv"
appcon <- read.csv(path)

### calc fraction of inland consumption from apparent consumption 
appcon<- appcon %>%
  filter(Element == "Protein supply quantity (g/capita/day)",
         Unit == "g/capita/day",
         Year == 2008) %>%
  select(Country, Item, Year, Value, Unit) %>%
  group_by(Country, Year) %>%
  summarise(fao.protein.g.cap.day.all = sum(Value),
            fao.protein.g.cap.day.fish = sum(Value[Item %in% c('Meat, Aquatic Mammals','Freshwater Fish','Cephalopods',
                                                               'Crustaceans','Demersal Fish','Fish, Body Oil', 'Fish, Liver Oil',
                                                               'Marine Fish, Other', 'Molluscs, Other', 'Pelagic Fish')]),
            # calc protein from inland fish - using the categories from FAO...
            fao.protein.g.cap.day.inland.fish = sum(Value[Item %in% c('Freshwater Fish')])) %>%
  ungroup %>%
  mutate(country_code = countrycode(Country,'country.name','iso3c',warn=F))




# get population data (FAOSTAT) -------------------------------------

pop_df <- read.csv('../data/population/faostat_population.csv', 
                   stringsAsFactors=FALSE)

# select all populations
pop_df <- pop_df %>%
  dplyr::select(AreaName, ElementName, Value, Year) %>%
  filter(ElementName =='Total Population - Both sexes' & Year == 2008) %>%
  dplyr::select(-ElementName)  # remove element name column


# rename remaining columns
names(pop_df) <- c('country', 'tot_pop_both_sexes', 'year')

# Match country name with code, for later joining
pop_df$country_code <- countrycode(pop_df$country, 
                                   'country.name', 'iso3c', warn = TRUE)

pop_df <- filter(pop_df, !is.na(country_code))

# Convert the population numbers from thousands to total number
pop_df$tot_pop_both_sexes <- pop_df$tot_pop_both_sexes * 1000


# get GDP.per.cap -----------------------------------------------------------
f <- '../data/wdi/gdp_percapita/API_NY.GDP.PCAP.CD_DS2_en_csv_v2.csv'
wdi_gdp <- read.csv(f, stringsAsFactors=FALSE)

# remove all '.' and 'X' in the column names 
colnames(wdi_gdp) <- gsub("[.]|X","",colnames(wdi_gdp))


wdi_gdp<- wdi_gdp %>%
  gather(year, NY.GDP.PCAP.CD, 5:ncol(wdi_gdp)) %>%
  select(CountryName, CountryCode, year, NY.GDP.PCAP.CD) %>%
  mutate(year= as.numeric(year)) %>%
  filter(!is.na(NY.GDP.PCAP.CD),
         year == 2008) %>%
  #group_by(CountryName, CountryCode) %>%
  #summarize(meanNY.GDP.PCAP.CD = mean(NY.GDP.PCAP.CD)) %>%
  mutate(country_code = CountryCode) %>%
  select(country_code, NY.GDP.PCAP.CD)




# join df ---------------------------------------------------------------------
protein.df <- surv_data_diff %>%
                  left_join(., pop_df, by='country_code') %>%
                  left_join(., wdi_gdp, by='country_code') %>%
                  left_join(., surv_protein, by='country_code') %>%
                  left_join(., aq_ca_tr_stat, by='country_code') %>%
                  left_join(., boot_pred, by='country_code') %>%
                  left_join(., appcon, by='country_code')



protein.df <- protein.df %>%
              mutate(fao.protein.g.cap.day.all  = ifelse(country_code=='COD', 15.7, fao.protein.g.cap.day.all),
                     fao.protein.g.cap.day.fish = ifelse(country_code=='COD', 5.2, fao.protein.g.cap.day.fish))



#  calc % of fish is inland capture  -----------------------------------------------------------
protein.df <- protein.df %>%

    # calc % of inland fish that is capture (from FAO stats)
    mutate(perc.of.inland.fish.is.capture = sum_catch.x / 
             (sum_catch.x + sum_aquacul - export + import)) %>%
    # max out 100%
    mutate(perc.of.inland.fish.is.capture = ifelse(perc.of.inland.fish.is.capture > 1, 
                                             1, perc.of.inland.fish.is.capture)) %>%
  
    # apply the percentage to survey
    mutate(survey.inland.capture.fish.protein.consumption..g.person.day. = 
             Average.protein.consumption..g.person.day. * mean_pred_inland_frac * perc.of.inland.fish.is.capture) 


# replace the fao inland capture calculation -------------------------
# for consistency, use the GAM predicted split, rather than the FAO 'Freshwater' category
protein.df <- protein.df %>%
  
  mutate(fao.protein.g.cap.day.inland.capture.fish = 
           fao.protein.g.cap.day.fish * mean_pred_inland_frac * perc.of.inland.fish.is.capture)



# replce the survey within the FAO numbers and calculate person equivalents
protein.df <- protein.df %>%

    # calc pers equiv of inland fish (from McIntyre 2016)   
    mutate(fao.pers.equiv = 
             fao.protein.g.cap.day.inland.capture.fish / fao.protein.g.cap.day.all * tot_pop_both_sexes) %>%
    

    # substitute in the survey protein
    mutate(rev.fao.protein.g.cap.day.all = 
             fao.protein.g.cap.day.all - fao.protein.g.cap.day.fish + Average.protein.consumption..g.person.day.) %>%


    # calc rev pers equiv of inland fish (from McIntyre 2016)   
    mutate(rev.fao.pers.equiv = 
             survey.inland.capture.fish.protein.consumption..g.person.day. / rev.fao.protein.g.cap.day.all * tot_pop_both_sexes)  %>%

     # cal dif pers equiv
    mutate(dif.pers.equiv = rev.fao.pers.equiv - fao.pers.equiv) %>%
    
    filter(!is.na(rev.fao.pers.equiv)) %>%
    arrange(NY.GDP.PCAP.CD) 

  

#  sum in million 
sum(protein.df$fao.pers.equiv, na.rm=1)/10^6
sum(protein.df$rev.fao.pers.equiv, na.rm=1)/10^6
sum(protein.df$tot_pop_both_sexes, na.rm=1)/10^6



sum(protein.df$rev.fao.pers.equiv, na.rm=1)/10^6 - sum(protein.df$fao.pers.equiv, na.rm=1)/10^6



# plot it karo ---------------------------------------------------------------
ggplot(protein.df) +
  geom_step(aes(x= NY.GDP.PCAP.CD, y= cumsum(fao.pers.equiv/10^6))) +
  geom_step(aes(x= NY.GDP.PCAP.CD, y= cumsum(rev.fao.pers.equiv/10^6)), color='red')+
  geom_vline(xintercept= max(protein.df$NY.GDP.PCAP.CD), color='grey')+
  geom_vline(xintercept= median(wdi_gdp$NY.GDP.PCAP.CD), color='grey')+
  scale_x_log10()





ggplot(protein.df) +
  geom_step(aes(x= NY.GDP.PCAP.CD, y= cumsum(fao.pers.equiv)/sum(fao.pers.equiv))) +
  geom_step(aes(x= NY.GDP.PCAP.CD, y= cumsum(rev.fao.pers.equiv)/sum(rev.fao.pers.equiv)), color='red')+
  geom_vline(xintercept= max(protein.df$NY.GDP.PCAP.CD), color='grey')+
  scale_x_log10() +
  ylim(0, 1)





# # bar plot of person equivalent ----------------------------------------------

# prep data
protein.df.summ <-  protein.df %>%
                    mutate(NY.GDP.PCAP.CD.cut = cut(NY.GDP.PCAP.CD/1000, breaks=seq(0,12, 0.2))) %>%
                    group_by(NY.GDP.PCAP.CD.cut) %>%
                    summarize(fao.pers.equiv = sum(fao.pers.equiv),
                              rev.fao.pers.equiv = sum(rev.fao.pers.equiv))


# replace the categories stings to make them nicer in the legend
protein.df.summ$NY.GDP.PCAP.CD.cut <- gsub("\\(|\\]", "", protein.df.summ$NY.GDP.PCAP.CD.cut)
protein.df.summ$NY.GDP.PCAP.CD.cut <- gsub("\\,", " to ", protein.df.summ$NY.GDP.PCAP.CD.cut)

protein.df.summ$NY.GDP.PCAP.CD.cut.cntr <- unlist(strsplit(protein.df.summ$NY.GDP.PCAP.CD.cut, " to "))[1].




ggplot(protein.df.summ) +
  geom_bar(aes(x= NY.GDP.PCAP.CD.cut, y= rev.fao.pers.equiv/10^6), fill='red', stat='identity', alpha=0.3) +
  geom_bar(aes(x= NY.GDP.PCAP.CD.cut, y= fao.pers.equiv/10^6), fill='blue', stat='identity', alpha=0.3) +

  geom_vline(xintercept= max(protein.df$NY.GDP.PCAP.CD), color='grey')+
  geom_vline(xintercept= median(wdi_gdp$NY.GDP.PCAP.CD), color='grey') +
  xlab("GDP per capita") +
  ylab("Person equivalent with intake \n supported by inland fisheries") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  #+ scale_x_log10()






ggplot(protein.df) +
  geom_line(aes(x= NY.GDP.PCAP.CD, y= fao.protein.g.cap.day.fish)) +   
  geom_line(aes(x= NY.GDP.PCAP.CD, y= survey.inland.capture.fish.protein.consumption..g.person.day.), color='blue')
  












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
  arrange(gdp_percap)
  
anim_protein_42 <-  anim_protein_42 %>%
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






# # read the code-assigned product list ---------------------
# consump_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods_refuse_fwaes.csv', 
#                        stringsAsFactors=FALSE)
# 
# consump_df <- consump_df %>%
#   
#   # calculate the per capita consumption based before refuse
#   mutate(Averageediblequantityconsumedgpersonday_wrefuse=  
#            Averageediblequantityconsumedgpersonday/ (1-refuse_factor/100)) %>%
#   
#   
#   # calculate the weighted average FWAE conversion factor
#   # Goal: get average factors to apply to production to infer breakdown of consumption
#   mutate(weighted_mean_fwae = mean_conv_fac * Averageediblequantityconsumedgpersonday_wrefuse,  
#          weighted_min_fwae = min_conv_fac * Averageediblequantityconsumedgpersonday_wrefuse,
#          weighted_max_fwae = max_conv_fac * Averageediblequantityconsumedgpersonday_wrefuse) %>%
#   
#   # for each country, back calculate the 
#   group_by(country_code) %>%
#   summarise(allsource_consump_kgyr_percap = sum(Averageediblequantityconsumedgpersonday_wrefuse / 1000 *365),
#             tot_pop_both_sexes = mean(tot_pop_both_sexes),
#             weighted_mean_fwae = sum(weighted_mean_fwae)/sum(Averageediblequantityconsumedgpersonday_wrefuse),
#             weighted_min_fwae = sum(weighted_min_fwae)/sum(Averageediblequantityconsumedgpersonday_wrefuse),
#             weighted_max_fwae = sum(weighted_max_fwae)/sum(Averageediblequantityconsumedgpersonday_wrefuse)) 
# 
# 
# 
# ### to do the error sensitivity properly
# 
# # loop through products,
# # draw the triangle distribution,
# # multiply by weight
# # sum per country
# # divide by total country weight
# # get mean & 95 intervals
# 
# 
# 
# 
# 
# 
# # ~~~ calc person equivalent of fish protein ----------------------------------------------
# surv_data_persequiv<- surv_data_diff %>%
#   
#   left_join(., consump_df, by='country_code') %>%
#   
#   # convert production to kilograms
#   mutate(mean_dif_kg = mean_dif * 10^6 * 10^3,
#          fao_sum_catch_kg = sum_catch* 10^6 * 10^3,
#          tr_corr_consump_kg = tr_corr_consump* 10^6 * 10^3,
#          
#          # calculate the pers equiv; original and revised
#          meandif_millionpersequiv = (mean_dif_kg / weighted_mean_fwae) / allsource_consump_kgyr_percap / 10^6,
#          fao_sum_catch_millionpersequiv = (fao_sum_catch_kg / weighted_mean_fwae) / allsource_consump_kgyr_percap / 10^6,
#          tr_corr_consump_millionpersequiv = (tr_corr_consump_kg / weighted_mean_fwae) / allsource_consump_kgyr_percap / 10^6,
#          tot_pop_both_sexes_million = tot_pop_both_sexes / 10^6) %>%
#   
#   # calculate the difference as percentage of population
#   mutate(persequiv_popperc = meandif_millionpersequiv/tot_pop_both_sexes*100) %>%
#     
#   # label countries where pers equiv is larger than population
#   mutate(persequiv_dir = ifelse(fao_sum_catch_millionpersequiv > tot_pop_both_sexes/10^6,
#                                 "fao larger than pop", NA)) %>%
#   
#   # and remove
#   filter(is.na(persequiv_dir ))
# 
# 
# 
# # ~~~ calc global totals -------------------------------------------------------------------
# 
# sum(surv_data_persequiv$meandif_millionpersequiv)
# sum(surv_data_persequiv$tr_corr_consump_millionpersequiv)
# sum(surv_data_persequiv$fao_sum_catch_millionpersequiv)
# sum(surv_data_persequiv$tot_pop_both_sexes_million)
# 
# # Sum only positive person equivalent
# sum(surv_data_persequiv[, 'meandif_millionpersequiv'])
# # Sum only positive person equivalent
# sum(surv_data_persequiv[which(surv_data_persequiv[,'meandif_millionpersequiv']>0), 'meandif_millionpersequiv'])
# sum(surv_data_persequiv[which(surv_data_persequiv[,'meandif_millionpersequiv']<0), 'meandif_millionpersequiv'])
# 
# 
# sum(surv_data_persequiv[, 'fao_sum_catch_millionpersequiv'])
# sum(surv_data_persequiv[, 'tr_corr_consump_millionpersequiv'])


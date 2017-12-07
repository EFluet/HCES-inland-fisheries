
# get data -----------------------------------------------------------

#~~ get countries w/ surveys  ----------------------------------------
surv_data_diff<- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv')

surv_data_diff <- surv_data_diff %>%
  dplyr::select(country_code, tr_corr_consump, sum_catch, mean_dif) %>%
  distinct() %>%
  filter(tr_corr_consump > 0) %>%
  mutate(hces_surv_asprcfao = tr_corr_consump/sum_catch *100)



#~~ get survey protein---------------------------------------------------------------
surv_protein <- read.csv('../data/consumption/survey_protein_consump_june2017.csv')


#~~ get FAO stats aq, ca, tr  ------------------------------------------
aq_ca_tr_stat <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm.csv',
                           stringsAsFactors = FALSE)

aq_ca_tr_stat <- aq_ca_tr_stat %>%
  filter(!datatype %in% c('uncert_fwae_ur', 'uncert_prodfm_ur',
                          'uncert_fwae_matched_mc', 'uncert_fwae_assumed_mc')) %>%
  dplyr::select(-one_of("country.x","grp","X", "confidence_lvl","p025","p975")) %>%
  spread(datatype, mean) %>%
  dplyr::select(-one_of("cons_pretrcorr","tr_corr_consump")) 



#~~ get inland fraction from gam model  ------------------------------------------
boot_pred <- read.csv('../output/bootpred_inland_consump_fraction_gam_pred.csv')
boot_pred <- boot_pred %>%
  mutate(mean_pred_inland_frac = mean,
         up_pred_inland_frac = up,
         lo_pred_inland_frac = lo) %>%
  dplyr::select(country_code, mean_pred_inland_frac, up_pred_inland_frac, lo_pred_inland_frac)




#~~ get apparent consumption  -----------------------------------------------------
path <- "../data/consumption/FAOSTAT_data_6-29-2017_fish_protein_apparent_consump.csv"
appcon <- read.csv(path)

# calc fraction of inland consumption from apparent consumption
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




#~~ get population data (FAOSTAT) -------------------------------------
pop_df <- read.csv('../data/population/faostat_population.csv', stringsAsFactors=FALSE)

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


#~~ get GDP.per.cap -----------------------------------------------------------
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




# join data df together --------------------------------------------------------
protein.df <- surv_data_diff %>%
                  left_join(., pop_df, by='country_code') %>%
                  left_join(., wdi_gdp, by='country_code') %>%
                  left_join(., surv_protein, by='country_code') %>%
                  left_join(., aq_ca_tr_stat, by='country_code') %>%
                  left_join(., boot_pred, by='country_code') %>%
                  left_join(., appcon, by='country_code')


# add countries with missing protein -------------------------------------------


# The missing protein statistics in FishStatJ are inserted for COD and PNG.
# Replacement data originates from: 
# 1961-2003: Fish and Fishery Products : World Apparent Consumption Statistics ...
# By Food and Agriculture Organization of the 
# https://books.google.com/books?id=qRTy6x-BtKkC&pg=PP11&lpg=PP11&dq=faostat+sua&source=bl&ots=-QoCy-24V4&sig=Ciozt4SdoKAD8c2XfaNqyg30jLU&hl=en&sa=X&ved=0ahUKEwiny6yzjObUAhWC7D4KHdDuB9kQ6AEIOTAD#v=onepage&q=papua&f=false

protein.df <- protein.df %>%
  
  # add D.R.Congo
  mutate(fao.protein.g.cap.day.all  = ifelse(country_code=='COD', 15.7, fao.protein.g.cap.day.all),
         fao.protein.g.cap.day.fish = ifelse(country_code=='COD', 5.2, fao.protein.g.cap.day.fish)) %>%

  # add PNG ?  <-- doesn't matter because survey protein not available
  mutate(fao.protein.g.cap.day.all  = ifelse(country_code=='PNG', 39.4, fao.protein.g.cap.day.all),
         fao.protein.g.cap.day.fish = ifelse(country_code=='PNG', 5.3, fao.protein.g.cap.day.fish))



# calc % of fish is inland capture  -----------------------------------------------------------
protein.df <- protein.df %>%

  # calc % of inland fish that is capture (from FAO stats)
  mutate(perc.of.inland.fish.is.capture = sum_catch.x / 
           (sum_catch.x + sum_aquacul - export + import)) %>%

  # max out inland% at 100%
  mutate(perc.of.inland.fish.is.capture = ifelse(perc.of.inland.fish.is.capture > 1, 
                                           1, perc.of.inland.fish.is.capture)) %>%

  # apply the inland%  percentage to survey
  mutate(survey.inland.capture.fish.protein.consumption..g.person.day. = 
           Average.protein.consumption..g.person.day. * mean_pred_inland_frac * perc.of.inland.fish.is.capture) %>%
  # lo bound
  mutate(survey.inland.capture.fish.protein.consumption..g.person.day.lo = 
           Average.protein.consumption..g.person.day. * lo_pred_inland_frac * perc.of.inland.fish.is.capture) %>%
  # upper bound
  mutate(survey.inland.capture.fish.protein.consumption..g.person.day.up = 
           Average.protein.consumption..g.person.day. * up_pred_inland_frac * perc.of.inland.fish.is.capture) 
  

# replace the fao inland capture calculation -------------------------
# for consistency, use the GAM predicted split, rather than the FAO 'Freshwater' category
protein.df <- protein.df %>%
  
  mutate(fao.protein.g.cap.day.inland.capture.fish = 
           fao.protein.g.cap.day.fish * mean_pred_inland_frac * perc.of.inland.fish.is.capture) %>%

  mutate(fao.protein.g.cap.day.inland.capture.fish.lo = 
           fao.protein.g.cap.day.fish * lo_pred_inland_frac * perc.of.inland.fish.is.capture) %>%
  
  mutate(fao.protein.g.cap.day.inland.capture.fish.up = 
           fao.protein.g.cap.day.fish * up_pred_inland_frac * perc.of.inland.fish.is.capture)





# replce the survey within the FAO numbers and calculate person equivalents
protein.df <- protein.df %>%

    # calc pers equiv of inland fish (from McIntyre 2016)   
    mutate(fao.pers.equiv = 
             fao.protein.g.cap.day.inland.capture.fish / fao.protein.g.cap.day.all * tot_pop_both_sexes) %>%
    
    mutate(fao.pers.equiv.lo = 
             fao.protein.g.cap.day.inland.capture.fish.lo / fao.protein.g.cap.day.all * tot_pop_both_sexes) %>%
    
    mutate(fao.pers.equiv.up = 
           fao.protein.g.cap.day.inland.capture.fish.up / fao.protein.g.cap.day.all * tot_pop_both_sexes)  %>%
  

    # substitute in the survey protein
    mutate(rev.fao.protein.g.cap.day.all = 
             fao.protein.g.cap.day.all - fao.protein.g.cap.day.fish + Average.protein.consumption..g.person.day.) %>%
      

    # calc revised pers equiv of inland fish (from McIntyre 2016)   
    mutate(rev.fao.pers.equiv = 
             survey.inland.capture.fish.protein.consumption..g.person.day. / rev.fao.protein.g.cap.day.all * tot_pop_both_sexes)  %>%

    mutate(rev.fao.pers.equiv.lo = 
             survey.inland.capture.fish.protein.consumption..g.person.day.lo / rev.fao.protein.g.cap.day.all * tot_pop_both_sexes)  %>%
      
    mutate(rev.fao.pers.equiv.up = 
             survey.inland.capture.fish.protein.consumption..g.person.day.up / rev.fao.protein.g.cap.day.all * tot_pop_both_sexes)  %>%  
      
     # cal dif pers equiv
    mutate(dif.pers.equiv = rev.fao.pers.equiv - fao.pers.equiv) %>%
    
    filter(!is.na(rev.fao.pers.equiv)) %>%
    arrange(NY.GDP.PCAP.CD) 

  

# total sum in million pers equiv ----------------------
sum(protein.df$fao.pers.equiv, na.rm=1)/10^6
sum(protein.df$rev.fao.pers.equiv, na.rm=1)/10^6
sum(protein.df$tot_pop_both_sexes, na.rm=1)/10^6

# 
sum(protein.df$fao.pers.equiv.lo, na.rm=1)/10^6
sum(protein.df$rev.fao.pers.equiv.lo, na.rm=1)/10^6

sum(protein.df$fao.pers.equiv.up, na.rm=1)/10^6
sum(protein.df$rev.fao.pers.equiv.up, na.rm=1)/10^6

# sum additional million pers equiv from surveys -------------------------------
# mean
sum(protein.df$rev.fao.pers.equiv, na.rm=1)/10^6 - sum(protein.df$fao.pers.equiv, na.rm=1)/10^6
# lower CI bound
sum(protein.df$rev.fao.pers.equiv.lo, na.rm=1)/10^6 - sum(protein.df$fao.pers.equiv.lo, na.rm=1)/10^6
# upper CI bound
sum(protein.df$rev.fao.pers.equiv.up, na.rm=1)/10^6 - sum(protein.df$fao.pers.equiv.up, na.rm=1)/10^6





protein.df <- protein.df %>%
              #sort by GDP/cap
              arrange(NY.GDP.PCAP.CD) %>%
  
              # cumul FAO mid
              mutate(cum.fao.pers.equiv = cumsum(fao.pers.equiv),
                     rel.cum.fao.pers.equiv = cum.fao.pers.equiv/max(cum.fao.pers.equiv),
                     
              # cumul fao up & lo
              cum.fao.pers.equiv.lo = cumsum(fao.pers.equiv.lo),
              cum.fao.pers.equiv.up = cumsum(fao.pers.equiv.up),
              
              cum.rev.fao.pers.equiv.lo = cumsum(rev.fao.pers.equiv.lo),
              cum.rev.fao.pers.equiv.up = cumsum(rev.fao.pers.equiv.up),
              
                     
              cum.rev.fao.pers.equiv = cumsum(rev.fao.pers.equiv),
              rel.cum.rev.fao.pers.equiv = cum.rev.fao.pers.equiv/max(cum.rev.fao.pers.equiv))


# save protein df to CSV file --------------------------------------------------
write.csv(protein.df, '../output/consumption/protein_equivalence/protein_equiv_fao_n_survey.csv')

# PLOTS ---------------------------------------------------------------

# step ribbon ggproto object
source('./consumption/plots/fcn/step_ribbon.r')

# theme 
source('./consumption/plots/themes/custom_catch_trend_line_theme.r')

# #~~ cumul line plot, absolute ------------------------------------
l_abs <- ggplot(protein.df) +
  
  # vertical lines
  geom_vline(xintercept= median(protein.df$NY.GDP.PCAP.CD), color='grey')+
  geom_vline(xintercept= median(wdi_gdp$NY.GDP.PCAP.CD), color='grey')+ 
  
    # label vertical lines
  annotate("text", label='World median\n GDP/cap', 
           x=median(wdi_gdp$NY.GDP.PCAP.CD),  y=35, 
           angle= 90, color='grey', vjust=1.1, fontface="italic", size=3) +
  
  annotate("text", label='Survey country \nmedian GDP/cap', 
           x=median(protein.df$NY.GDP.PCAP.CD), y=35, 
           angle= 90, color='grey', vjust=1.1, fontface="italic", size=3) +

  
  annotate("text", label='Survey', 
           x=1400, y=135, 
           color='blue', size=3.25) +
  
  annotate("text", label='Reported', 
           x=2900, y=60, 
           color='darkgoldenrod2', size=3.25) +
  
  
  geom_ribbon(aes(x=NY.GDP.PCAP.CD, 
                  ymin = cum.rev.fao.pers.equiv.lo/10^6, 
                  ymax = cum.rev.fao.pers.equiv.up/10^6), 
              stat='stepribbon',fill = "blue", alpha=0.1) +
  
  ## ribbon for FAO person equivalent
  geom_ribbon(aes(x=NY.GDP.PCAP.CD,
                  ymin = cum.fao.pers.equiv.lo/10^6,
                  ymax = cum.fao.pers.equiv.up/10^6),
              stat='stepribbon',fill = "darkgoldenrod2", alpha=0.1) +
  
  # draw step cumul lines
  geom_step(aes(x= NY.GDP.PCAP.CD, y= cum.fao.pers.equiv/10^6), color='darkgoldenrod2') +
  geom_step(aes(x= NY.GDP.PCAP.CD, y= cum.rev.fao.pers.equiv/10^6), color='blue') +

  
  
  scale_x_log10(breaks = c(500, 1000, 5000, 10000)) +
  scale_y_continuous(breaks = c(10, 50, 100, 150), limits=c(10, 150), expand=c(0,0)) +
  
  ylab('Cumulative person equivalent \n of animal protein consumption (million people)') +
  xlab("GDP per capita (x1000 USD)") +
  custom_catch_trend_line_theme +
  theme(text = element_text(size=8, colour='black'))


l_abs

# ~~~ save plot ------------------------------------------------------------------
ggsave("../Output/Figures/fig5_abs_cumul_persequiv_wCI_v3.pdf",
       dpi=1000, width=87, height=81, units='mm')#, type = "cairo-png")

dev.off()



# #~~ cumul line plot, relative ------------------------------- 
# l_rel <- ggplot(protein.df) +
#   
#   geom_vline(xintercept= median(protein.df$NY.GDP.PCAP.CD), color='grey')+
#   geom_vline(xintercept= median(wdi_gdp$NY.GDP.PCAP.CD), color='grey')+ 
#   
#   geom_step(aes(x= NY.GDP.PCAP.CD, y= rel.cum.fao.pers.equiv)) +
#   geom_step(aes(x= NY.GDP.PCAP.CD, y= rel.cum.rev.fao.pers.equiv), color='red')+
# 
#   scale_x_log10() +
#   ylim(0, 1) +
#   ylab('Relative cumul. \nperson equivalent') +
#   xlab('GDP per capita')
# 
# 
# ### Combine maps and plots -------------------------------------
# require("gridExtra")
# require("grid")
# 
# l_abs_g <- ggplotGrob(l_abs)
# l_rel_g <- ggplotGrob(l_rel)
# 
# pers_equiv_line_plots <- plot_grid(l_abs_g, l_rel_g, align = "v", nrow = 2)  #, rel_heights = c(1/8, 7/8))
# pers_equiv_line_plots
# 
# 
# ### save plot ------------------------------------------------------------------
# ggsave("../Output/Figures/pers_equiv_line_plots.png", pers_equiv_line_plots,
#        dpi=1000, width=87, height=155, units='mm' , type = "cairo-png")
# 
# dev.off()
# 
# 
# 
# 
# 
# # bar plot of person equivalent ----------------------------------------------
# 
# # ~~~ prep data, summarizing values -------------------
# protein.df.summ <-  protein.df %>%
#   
#   mutate(NY.GDP.PCAP.CD.cut = cut(NY.GDP.PCAP.CD/1000, breaks=seq(0,12, 0.2))) %>%
#   group_by(NY.GDP.PCAP.CD.cut) %>%
#   summarize(tot_pop_both_sexes= sum(tot_pop_both_sexes),
#             fao.pers.equiv = sum(fao.pers.equiv),
#             rev.fao.pers.equiv = sum(rev.fao.pers.equiv), 
#             fao.protein.g.cap.day.all = mean(fao.protein.g.cap.day.all),
#             rev.fao.protein.g.cap.day.all = mean(rev.fao.protein.g.cap.day.all),
#             fao.protein.g.cap.day.inland.capture.fish = mean(fao.protein.g.cap.day.inland.capture.fish),
#             survey.inland.capture.fish.protein.consumption..g.person.day. = 
#                                                   mean(survey.inland.capture.fish.protein.consumption..g.person.day.))
# 
# 
# # replace the categories stings to make them nicer in the legend
# protein.df.summ$NY.GDP.PCAP.CD.cut <- gsub("\\(|\\]", "", protein.df.summ$NY.GDP.PCAP.CD.cut)
# protein.df.summ$NY.GDP.PCAP.CD.cut <- gsub("\\,", "-", protein.df.summ$NY.GDP.PCAP.CD.cut)
# #protein.df.summ$NY.GDP.PCAP.CD.cut.cntr <- unlist(strsplit(protein.df.summ$NY.GDP.PCAP.CD.cut, " to "))[1].
# 
# #~~ read bargraph theme  --------------------------------------------------
# source('./consumption/plots/themes/barplot_catchdiff_theme.r')
# 
# 
# # ~~~ bar graph --------------------------------------------------
# ggplot(protein.df.summ) +
#   
#   # bars for total pop
#   #geom_bar(aes(x= NY.GDP.PCAP.CD.cut, y= tot_pop_both_sexes/10^6), fill='grey80', stat='identity') +
#   
#   # bars for 
#   geom_bar(aes(x= NY.GDP.PCAP.CD.cut, y= rev.fao.pers.equiv/10^6), fill='red', stat='identity', alpha=0.3) +
#   geom_bar(aes(x= NY.GDP.PCAP.CD.cut, y= fao.pers.equiv/10^6), fill='blue', stat='identity', alpha=0.3) +
# 
#   # add vertical lines 
#   #geom_vline(xintercept= median(protein.df$NY.GDP.PCAP.CD/1000), color='grey') +
#   #geom_vline(xintercept= median(wdi_gdp$NY.GDP.PCAP.CD/1000), color='grey') +
#   
#   xlab("GDP per capita (x1000 USD)") +
#   ylab("Person equivalent supported  \nby inland fish protein (million) ") +  #of animal protein intake
#   barplot_catchdiff_theme +
#   theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
#         plot.margin = unit( c(1,1,1,1) , "mm"))
#   
# 
# # ~~~ save plot ------------------------------------------------------------------
# ggsave("../Output/Figures/fig5_barplot_pers_equiv.pdf",
#        dpi=800, width=87, height=64, units='mm' )  #, type = "cairo-png")
# 
# dev.off()
# 
# 
# 
# 
# # line plot of protein consumption -----------------------------------------
# ggplot(protein.df.summ) +
#   geom_line(aes(x= NY.GDP.PCAP.CD.cut, y= fao.protein.g.cap.day.all, group=1), stat='summary') +
#   geom_line(aes(x= NY.GDP.PCAP.CD.cut, y= rev.fao.protein.g.cap.day.all, group=1), color='blue', stat='summary') +   
# 
#   geom_line(aes(x= NY.GDP.PCAP.CD.cut, y= fao.protein.g.cap.day.inland.capture.fish, group=1), stat='summary') +
#   geom_line(aes(x= NY.GDP.PCAP.CD.cut, y= survey.inland.capture.fish.protein.consumption..g.person.day., group=1), color='blue', stat='summary') +
#   barplot_catchdiff_theme +
#   theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
#         plot.margin = unit( c(1,1,1,1) , "mm"))
# 
# 
# # ~~~ save plot ------------------------------------------------------------------
# ggsave("../Output/Figures/fig5_barplot_protein_consump.png",
#        dpi=1000, width=90, height=80, units='mm', type = "cairo-png")
# 
# dev.off()

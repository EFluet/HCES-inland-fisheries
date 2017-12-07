

### import packages ---------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(countrycode)

### set working directory --------------------------------------------
setwd("C:/Users/efluet/Dropbox/chap5_global_inland_fish_catch/scripts")


### read consumption data file ---------------------------------------------------
path <- "../output/consumption/prod_coded_n_agg/hh_survey_consump_nat_agg_prod_fw_uncert_tradecorr_diff.csv"
consump_nat_agg_dif <- read.csv(path, stringsAsFactors=F)
rm(path)

#  make unique list of country and year of surveys 
surv_cntry_yr <- unique(consump_nat_agg_dif[c('country_code','year_start')]) 


### source scripts that count the number of flags
source('./consumption/data_proc/fishstatj_flag_tally/aq_fao_flag_tally.r')
source('./consumption/data_proc/fishstatj_flag_tally/tr_fao_flag_tally.r')
source('./consumption/data_proc/fishstatj_flag_tally/ca_fao_flag_tally.r')
rm(aq_flag_tally, tr, tr_inl_sel, tr_flag_tally, ca_flag_tally)


# add row to aquaculture flag (because tr has one identifying import/export)
aq_flag_tally_agg$TradeflowTradeflow <- 'Aquaculture'
ca_flag_tally_agg$TradeflowTradeflow <- 'Catch'


# combine the flag df from aq and tr
all_flag_tally_agg <- rbind(aq_flag_tally_agg, tr_flag_tally_agg) %>%
                      rbind(. , ca_flag_tally_agg)

# remove parts of the rbind
rm(tr_flag_tally_agg, aq_flag_tally_agg, ca_flag_tally_agg)


# Join the catch (survey and fao) to  the flag tally df
all_flag_tally_agg <- left_join(all_flag_tally_agg, consump_nat_agg_dif,
                                by = c("country_code"="country_code"))



### get theme of plot -------------------------------------------
source('./consumption/plots/themes/custom_scatterplot_theme.r')



### creat scatterplot object ---------------------------------------------------
s <- ggplot(all_flag_tally_agg, 
            aes(x = flag_perc,
                ymin= tons_dif_min,
                y = tons_dif_mean,
                ymax= tons_dif_max)) +
  
  geom_point(size = 0.75, shape = 1,  colour='black') + #stroke = 1.15, alpha= 0.6,
  geom_errorbar(size = 0.3, colour='grey55', width = 0.05) +
  
  geom_hline(aes(yintercept=0), colour="black", size=0.25) +
  
  # add ggrepel labels
  geom_text_repel(aes(label=country_code),
                  size = 2, fontface = 'bold', colour='gray25', 
                  segment.size = 0.15, segment.color='gray25',
                  force = 8, box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines')) + 
  
  xlab("Percentage of entries flagged as unavailable (...) \n or estimated by FAO (F)") +
  ylab("Tons difference between survey catch and FAO catch") +
  ggtitle("") +
  facet_wrap(~ TradeflowTradeflow, nrow=1) +
  custom_scatterplot_theme
  
  # add custom theme
  #custom_scatterplot_theme

s

# save figure to output file
ggsave("../output/figures/flag_tally/catch_surv_fao_perc_dif_vs_flag_tally_facet.png", 
       width=210, height=90, dpi=800, units="mm", type = "cairo-png")



### regroup Import-Export-Aquaculture -----------------------
### REcalculate the percentage of flags per country
all_flag_tally_agg_regroup <- all_flag_tally_agg %>%
                      mutate(regroup = ifelse(TradeflowTradeflow=='Import'|
                                              TradeflowTradeflow=='Export'|
                                              TradeflowTradeflow=='Aquaculture',
                                              'Import_Export_Aquaculture',
                                              'Catch')) %>%
                      #select(-flag_perc) %>%
                      group_by(country_code, regroup) %>%
                      summarize(flag = sum(flag), not_flag = sum(not_flag)) %>%
                      mutate(flag_perc = flag/(flag+not_flag)) %>%
                      left_join(. , consump_nat_agg_dif, by = c("country_code"="country_code"))









### creat scatterplot object ---------------------------------------------------
s2 <- ggplot(all_flag_tally_agg_regroup, 
             aes(x = flag_perc,
                 ymin= tons_dif_min,
                 y = tons_dif_mean,
                 ymax= tons_dif_max)) +
  
  geom_point(size = 0.75,colour='black') + #stroke = 1.15, alpha= 0.6,  shape = 1,  
  geom_errorbar(size = 0.25, colour='black', width = 0.02) +
  
  geom_hline(aes(yintercept=0), colour="black", size=0.25) +
  
  # add ggrepel labels
  geom_text_repel(aes(label=country_code),
                  size = 2, fontface = 'bold', colour='gray45', 
                  segment.size = 0.15, segment.color='gray45',
                  force = 8, box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines')) + 
  
  xlab("Percentage of entries flagged as unavailable (...) \n or estimated by FAO (F)") +
  ylab("Tons difference between survey catch and FAO catch") +
  ggtitle("") +
  facet_wrap(~ regroup, nrow=1) +
  custom_scatterplot_theme

s2

# save figure to output file
ggsave("../output/figures/flag_tally/catch_surv_fao_perc_dif_vs_flag_tally_regroup_facet.png", 
       width=210, height=90, dpi=800, units="mm", type = "cairo-png")






### regroup Import-Export-Aquaculture -----------------------
### REcalculate the percentage of flags per country
all_flag_tally_agg_regroup_spread <-  all_flag_tally_agg_regroup %>%
                                      select(country_code, regroup, flag_perc, prc_dif_mean) %>% 
                                      spread(regroup, flag_perc) 
  
# remove small countries with very large percentage difference 
all_flag_tally_agg_regroup_spread <- all_flag_tally_agg_regroup_spread %>% 
                                     filter(country_code != 'BTN', 
                                            country_code != 'AZE',
                                            country_code != 'ARM')

### creat scatterplot object ---------------------------------------------------
s3 <- ggplot(all_flag_tally_agg_regroup_spread, 
             aes(x = Import_Export_Aquaculture,
                 y = Catch)) +
  
  #geom_point(size = 0.7, colour='black') + #stroke = 1.15, alpha= 0.6, shape = 1,
  geom_point(aes(size = prc_dif_mean*100), colour='#0000FF', alpha= 0.3) + #stroke = 1.15, alpha= 0.6, shape = 1,
  scale_size(range = c(0.05, 8),name = "Percentage difference \nof survey and FAO catch") + 
  
  # add ggrepel labels
  geom_text_repel(aes(label=country_code),
                  size = 2, fontface = 'bold', colour='gray45', 
                  segment.size = 0.15, segment.color='gray45',
                  force = 8, box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines')) + 
  
  xlab("Percentage of FAO import-export-aquaculture entries \n flagged as unavailable (...) or estimated by FAO (F)") +
  ylab("Percentage of FAO catch entries \n flagged as unavailable (...)  or estimated by FAO (F)") +
  ggtitle("") + #xlim(0,1) + ylim(0,1) +
  custom_scatterplot_theme

s3



# save figure to output file
ggsave("../output/figures/flag_tally/catch_flag_vs_tr_aq_flag_bubblesize.png", 
       width=120, height=90, dpi=800, units="mm", type = "cairo-png")




### linear model of flags and diff  --------------------

all_flag_tally_agg_forlm <-  all_flag_tally_agg %>%
  select(country_code, TradeflowTradeflow, flag_perc, prc_dif_mean) %>% 
  spread(TradeflowTradeflow, flag_perc) 


fit <-lm(prc_dif_mean ~ Aquaculture + Catch + Export + Import, data=all_flag_tally_agg_forlm)
summary(fit)


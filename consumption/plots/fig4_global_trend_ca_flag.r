
# get ggplot2 theme
source('./consumption/plots/themes/custom_catch_trend_line_theme.r')



### get survey data ----------------------------------------------------------
# read survey data, incl. catch difference with FAO stats 
f <- '../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_diff.csv'
sum_surv <- read.csv(f, stringsAsFactors=FALSE)
glob_sum_surv <- sum_surv %>%
  select(country_code, country_label, year_start,
         p025, p975, tr_corr_consump) %>%
  distinct %>%
  summarize(tr_corr_consump= sum(tr_corr_consump),
            p025 = sum(p025), 
            p975 = sum(p975),
            min_year_start = min(year_start),
            #year_start = mean(year_start),
            max_year_start = max(year_start))



### get ca flags for each year ---------------------------------------- 
source('./consumption/data_proc/fishstatj_flag_tally/byweight/ca_fao_flag_tally_byweight.r')



### get FishStatJ catch data  -------------------------------------------------
source("./fishstatj_data/read_nat_catch_data.R")

# join on country and year, keeping years after survey
ca_sum_bycountry <- ca_sum_bycountry %>%
                    ungroup() %>%
                    filter(source == 'Inland') %>%
                  
                    mutate(catch_fao = sum_catch / 10^6) %>%
                    mutate(CountryCountry= ifelse(country_code=='SDN','Sudan (former)', CountryCountry)) %>% 
                    select(-one_of('CountryCountry','type','sum_catch','source'))


### sum ca globally --------------------------------------------
glob_ca_sum <- ca_sum_bycountry %>%
               group_by(year) %>%
               summarize(catch_fao = sum(catch_fao))


### sum ca surveyed countries --------------------------------------------
surveyed_ca_sum <- ca_sum_bycountry %>%
  filter(country_code %in% sum_surv$country_code) %>%
  group_by(year) %>%
  summarize(catch_fao = sum(catch_fao)) 


### sum ca for China & India ---------------------------------------------------
chn_ind_ca_sum <- ca_sum_bycountry %>%
                  filter(country_code %in% c('CHN','IND')) %>%
                  spread(country_code, catch_fao) %>%
                  left_join(., surveyed_ca_sum, by=c('year')) %>%
  
                  mutate(ind_catch_stacked = IND + catch_fao,
                         chn_catch_stacked = IND + CHN + catch_fao) %>%
                  select(-CHN, -IND, -catch_fao) %>%
                  gather(cntry, catch_stacked, chn_catch_stacked:ind_catch_stacked)
  



### convert estimates to common units -----------------------------------------
ca_flag <- ca_sum_bycountry %>%
  left_join(., ca_flag_tally_agg, by = c('country_code'='country_code',
                                'year'='year')) %>%
  mutate(flag_perc_cut = ifelse(flag_perc > 0.4, 'Flag', 'No_flag')) %>%
  filter(!is.na(flag_perc), 
         year != 2014,
         country_code %in% sum_surv$country_code) %>%
  group_by(flag_perc_cut, year) %>%
  summarize(catch_fao = sum(catch_fao)) %>%
  ungroup() %>%
  spread(flag_perc_cut, catch_fao, fill=0) %>%
  gather(flag_perc_cut, catch_fao, Flag:No_flag) %>%
  mutate(flag_perc_cut = factor(flag_perc_cut, levels=c('No_flag','Flag'))) %>%
  arrange(year, flag_perc_cut)




## ggplot ----------------------------------------------------
t <- ggplot() +
  
  
  
  # *** add global catch line --------------------------------------------------
  geom_area(data=glob_ca_sum, aes(x= year, y=catch_fao), fill='grey85', color=NA) +
  
  # ~~~ add lines for China and India
  geom_line(data=chn_ind_ca_sum, aes(x=year, y= catch_stacked, group=cntry), color='white', size=0.4) +
  
  #
  annotate("text", label='China', x=2003, y=6.3, angle = 45, color='white', size=2.9) +
  annotate("text", label='India', x=2010, y=6.45, angle = 45, color='white', size=2.9) +
  annotate("text", label='Other X countries', x=1988, y=5.2, angle = 45, color='white', size=2.9) +
  annotate("text", label='1997-2013', x=2006, y=11.8, color='blue', hjust=0.5, fontface="italic", size=2.5) +
  
  
  # *** add label to global catch line -----------------------------------------
  geom_text_repel(data = subset(glob_ca_sum, year==1970),
                aes(x=year, y=catch_fao, label= "Global reported\ninland catch"),
                color='black',
                segment.color='black',
                size = 2.5,
                nudge_x = -1,
                nudge_y = 1,
                segment.size = 0.25,
                box.padding = unit(3, 'mm'),
                point.padding = unit(3, 'mm')) +
  
  
  # ~~~ add area of survey, grouped by flags -----------------------------------
  geom_area(data=ca_flag, 
            aes(x=year, y=catch_fao, fill=flag_perc_cut), 
            position = 'stack', stat = "identity", na.rm=TRUE) +  #colour='grey15', 
    
  # add  line for total catch of survey countries
  geom_line(data=surveyed_ca_sum, aes(x= year, y=catch_fao), colour='blue', size=0.6) +
  
  # *** add label to reported catch in survey countries ------------------------
  geom_text_repel(data = subset(surveyed_ca_sum, year==mean(year)),
                  aes(x=year, y=catch_fao, label= "Reported catch in\n42 survey countries"),
                  color='blue',
                  segment.color='blue',
                  size = 2.5,
                  nudge_x = 1,
                  nudge_y = -1,
                  segment.size = 0.25,
                  box.padding = unit(3, 'mm'),
                  point.padding = unit(3, 'mm')) +
    
    
  # *** add rectangle ---------------------------------------
  geom_rect(data=glob_sum_surv, aes(xmin=min_year_start, xmax=max_year_start, ymin=p025, ymax=p975),
                  fill='blue', color=NA,
                  alpha=0.20,
                  inherit.aes = FALSE) +
    
  # *** add mid line to rectangle --------------------------------------------------
  geom_segment(data = glob_sum_surv, color='blue', size=0.6,
               aes(x = min_year_start, y = tr_corr_consump, 
                   xend = max_year_start, yend = tr_corr_consump)) +
  
  # *** add label of HCES avg line -------------------------------------------------
  geom_text_repel(data = glob_sum_surv,
                  aes(x=min_year_start, y=tr_corr_consump, label= "Estimated catch in\n42 survey countries"),
                  color='blue',
                  segment.color='blue',
                  size = 2.5,
                  nudge_x = -5.5,
                  nudge_y = -0.25,
                  segment.size = 0.25,
                  box.padding = unit(3, 'mm'),
                  point.padding = unit(3, 'mm')) +
    
    
  # *** add global catch line --------------------------------------------------
  #geom_line(data=glob_ca_sum, aes(x= year, y=catch_fao), color='black', size=0.6) +

  
  scale_fill_brewer(palette='YlOrBr',
                    labels =  c("Flag" = 'Catch flagged as estimated ',
                                "No_flag" = 'Catch not flagged')) +
  
  coord_cartesian(expand=0, xlim= c(1950, 2014), ylim= c(0, 12)) +
  
  ylab('Inland catch (million tonnes)') + xlab('Year') +
  

  custom_catch_trend_line_theme+
  theme(legend.position = c(0.03, 0.94),
        axis.text.y=element_text(size = 7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        strip.background = element_rect(colour = NA, fill = NA))


### *** save figure to file --------------------------------------------------------
ggsave('../output/figures/fig4_global_flag_timeline_v5.pdf', t, 
       width=87, height=70, dpi=800, units="mm")  #, type = "cairo-png")
dev.off()





### get survey data ----------------------------------------------------------

### Extract years after surveys to investigate if large changes have occured

# read survey data, incl. catch difference with FAO stats 
f <- '../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv'
sum_surv <- read.csv(f, stringsAsFactors=FALSE)
sum_surv <- sum_surv %>%
            select(country_code, country_label, year_start, 
                   p025, p975, tr_corr_consump) %>%
            distinct


### get FishStatJ catch data  -------------------------------------------------
source("./fishstatj_data/read_nat_catch_data.R")
# RENAME sum_catch  to  sum_catch_fao
rm(f, ca)


# join on country and year, keeping years after survey
sum_surv_ca <- ca_sum_bycountry %>%
               ungroup() %>%
               filter(source == 'Inland',
                      year >= 1990,
                      country_code %in% sum_surv$country_code) %>%
               left_join(., sum_surv, by = c('country_code'='country_code',
                                             'year'='year_start')) %>%
                mutate(catch_fao = sum_catch / 10^6) %>%
                mutate(CountryCountry= ifelse(country_code=='SDN','Sudan (former)', CountryCountry)) %>%
                select(-one_of('CountryCountry','type','sum_catch','source'))






### get Big Numbers Project data  & join in fao catch data -----------------------
bnp_cases <- read.csv('../data/catch_yield/national_catch_assessments.csv', 
                      stringsAsFactors=F)

colnames(bnp_cases) <- paste(colnames(bnp_cases), "bnp", sep = "_")

# add country_code column 
bnp_cases<- bnp_cases %>%
            mutate(country_code= countrycode(country_bnp, 'country.name','iso3c',warn=FALSE)) %>%
            select(year_start_bnp, total_catch_bnp, country_code,
                   total_catch_low_bnp, total_catch_high_bnp, Source_bnp)

# join BNP to the catch table
sum_surv_ca <- sum_surv_ca %>% 
        left_join(., bnp_cases, 
                   by=c('country_code'='country_code',
                        'year'= 'year_start_bnp')) %>%
        mutate(total_catch_bnp = total_catch_bnp,
               continent = countrycode(country_code, 'iso3c', 'continent',warn=FALSE),
               country = countrycode(country_code, 'iso3c', 'country.name',warn=FALSE))



### select subset of countries -------------------------------------
sub <- data.frame(region=character(),
                  country_code=character(), stringsAsFactors = F) 

sub <- rbind(sub, 
             c('South Asia', 'BGD'),
             c('South Asia', 'MMR'),
             
             c('Mekong Basin', 'KHM'),
             c('Mekong Basin', 'LAO'),
             c('Mekong Basin', 'THA'),
             
             c('West Africa', 'GHA'),
             c('West Africa', 'BEN'),
             c('West Africa', 'TGO'),
             
             c('Amazon Basin', 'BRA'),
             c('Amazon Basin', 'PER'),
             c('Amazon Basin', 'COL'),
             c('Amazon Basin', 'BOL'),
             
             c('East Africa', 'UGA'),
             c('East Africa', 'COD'),
             c('East Africa', 'TZA'),
             c('East Africa', 'KEN'),
             #c('East Africa', 'MOZ'),
             #c('East Africa', 'MWI'),
             #c('East Africa', 'ZMB'),
             
             c('Sahel', 'TCD'),
             c('Sahel', 'NER'),
             c('Sahel', 'MLI'),
             c('Sahel', 'BFA'))

names(sub) <- c('region','country_code')
sub$region <- as.character(sub$region)
sub$country_code <- as.character(sub$country_code)


sum_surv_ca <- sum_surv_ca %>%
               filter(country_code %in% sub$country_code) %>%
               left_join(., sub, by='country_code') %>%
               mutate(country = ifelse(country_code=='LAO', 'Lao PDR', country),
                      country = ifelse(country_code=='BOL', 'Bolivia', country),
                      country = ifelse(country_code=='COD', 'D.R.Congo', country),
                      country = ifelse(country_code=='BFA', 'Burkina Faso', country),
                      country = ifelse(country_code=='TZA', 'Tanzania', country))



### get watebody catch estimates  ---------------------------------------------
cs <- read.csv('../data/case_studies/cs_db_subset_fortimelineplot_may2017.csv',
               stringsAsFactors = F)
cs<- cs %>%
  mutate(CATCH_tperyr_mid = as.numeric(CATCH_tperyr_mid),
         year = as.numeric(year),
         wb_label = paste(WB_NAME,'\n','(',REF_NAME,')', sep='')) %>% 
  filter(!is.na(year),
         !is.na(region),
         region != '',
         select_ef == 'y',
         year >= 1990) %>%
  group_by(WB_NAME) %>%
  mutate(wb_name_label = ifelse(year == max(year), WB_NAME, NA)) %>%
  ungroup()

cs$wb_name_label <- sub("[[:space:]]", '\n', cs$wb_name_label)





### order regions, to order facets in plot --------------------------------------
region_facet_order <- c('West Africa','Sahel', 'South Asia',
                        'Amazon Basin', 'East Africa','Mekong Basin')

sum_surv_ca$region <- factor(sum_surv_ca$region, levels=region_facet_order)
cs$region <- factor(cs$region, levels=region_facet_order)



### order sources, to order numbering of points --------------------------------
source_order <- c("This study", "Hidden Harvest (2012) / Big Numbers Project (2008)", 
                 "Van Zalinge & Nao Thuok 1999","Coates et al. 2002",
                 "Hortle et al. 2007","Barlow 2002", "Van Zalinge et al. 2004")

# set the manual order as ordered factor

sum_surv_ca <- sum_surv_ca %>%
               mutate(Source_bnp = ifelse(!is.na(tr_corr_consump),'This study', Source_bnp))

sum_surv_ca$Source_bnp <- factor(sum_surv_ca$Source_bnp, levels=source_order)






### convert estimates to common units
sum_surv_ca <- sum_surv_ca %>%
               mutate(assess_mean = total_catch_bnp/1000,
                      assess_mean = ifelse(is.na(assess_mean), tr_corr_consump*1000, assess_mean),
                      
                      assess_high = total_catch_high_bnp/1000,
                      assess_high = ifelse(is.na(assess_high), p975*1000, assess_high),
                      
                      assess_low = total_catch_low_bnp/1000,
                      assess_low = ifelse(is.na(assess_low), p025*1000, assess_low))




# filter to keep only 3 regions now
sum_surv_ca <- sum_surv_ca %>%
               filter(region %in% c('South Asia', 'East Africa','Mekong Basin'))

cs <- cs %>% filter(region %in% c('South Asia', 'East Africa','Mekong Basin'))

### order regions, to order facets in plot --------------------------------------
region_facet_order <- c('East Africa', 'Mekong Basin', 'South Asia')

sum_surv_ca$region <- factor(sum_surv_ca$region, levels=region_facet_order)
cs$region <- factor(cs$region, levels=region_facet_order)





### ggplot - timeline catch estimate -----------------------------------

source('./consumption/plots/themes/custom_catch_trend_line_theme.r')
library(ggrepel)

l <- ggplot(sum_surv_ca) +
  
  # add line for FAO stats
  geom_line(data=sum_surv_ca, 
            aes(x=year, y=catch_fao*1000, color=country), size=0.3) +
  
  # Add point for other assessments 
  geom_point(data=subset(sum_surv_ca, !is.na(assess_mean)),
             aes(x=year, y=assess_mean,
                 color=country), shape=16, size=3) +

  # add errorbars of assessment estimates
  geom_errorbar(data=sum_surv_ca, 
                aes(x=year, ymin=assess_low, ymax=assess_high, color=country),
                width=0, size=0.2) +
  
  # add nb label fo assessments
  geom_point(data=subset(sum_surv_ca, !is.na(assess_mean)), 
             aes(x=year, y=assess_mean, 
                  shape=Source_bnp), color='white', size=2) +
  scale_shape_manual(values = c(49, 50, 51, 52, 53, 54, 55)) +
  

  # waterbodies points (diff for Victoria)
  geom_point(data=subset(cs, WB_NAME!='Lake Victoria'),  
             aes(x=year, y=CATCH_tperyr_mid/1000), color='black', size=1) +
  
  geom_line(data=subset(cs, WB_NAME=='Lake Victoria'), 
            aes(x=year, y=CATCH_tperyr_mid/1000), color='black', size=0.3) +
  
  geom_errorbar(data=cs, aes(x=year, 
                             ymin=as.numeric(CATCH_range_low)/1000, 
                             ymax=as.numeric(CATCH_range_high)/1000), 
                width=0, color='black', size=0.2) +
  
  # waterbody label
  geom_text_repel(data = cs,
                  aes(x=year, y=CATCH_tperyr_mid/1000, label = wb_name_label),
                  color='grey25',
                  segment.color='grey25',
                  size = 2.5,
                  nudge_x = 0,
                  segment.size = 0.25,
                  box.padding = unit(3, 'mm'),
                  point.padding = unit(3, 'mm')) +

  
  # country labels on right side
  coord_cartesian(xlim = c(min(sum_surv_ca$year), max(sum_surv_ca$year) + 7)) +
  geom_text_repel(data = subset(sum_surv_ca, year == max(sum_surv_ca$year)),
                  aes(x=year, y=catch_fao*1000, label = country, color = country),
                  size = 2.5,
                  nudge_x = 2,
                  segment.color = NA,
                  box.padding = unit(0.5, 'mm')) + 

  
  scale_x_continuous(breaks=c(1980, 1990, 2000, 2010, 2014), expand = c(0,0)) +
  #scale_y_continuous(breaks=pretty_breaks(n=5)) +
  scale_y_continuous(breaks=pretty_breaks(n=5), limits = c(0, NA), expand = c(0,10)) +
  scale_color_discrete(guide=FALSE) +
  #scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))+
  
  facet_wrap(~region, scales='free_y', ncol=3) +
  ylab("Inland wild fish catch (×1000 tonnes)") + xlab("") +
  custom_catch_trend_line_theme + 

  theme(legend.position = 'none',
        legend.direction = 'vertical',
        axis.ticks = element_line(colour='black'),
        plot.margin=unit(c(1, 6, -1, 1), "mm"),
        panel.spacing = unit(6, "mm"),
        strip.text = element_text(hjust= 0, vjust = -1),
        legend.key = element_rect(fill='black'))
        #'bottom',#c(0.75, 0.3),


### save figure to file ----------------------------------------------------------
ggsave('../output/figures/fig3_catch_timeline_3regions_nobox.pdf', l,
       width=178, height=75, dpi=800, units="mm")  #, type = "cairo-png")
dev.off()





### Calculate year on year change in % ---------------------------------------
# # identify years of large change (readjustments)
# sum_surv_ca_yrch<- sum_surv_ca %>%
#   
#   # calculate yr-on-yr difference
#   arrange(country_code, year) %>%
#   group_by(country_code) %>%
#   mutate(ca_lag_perc_diff= (sum_catch.y - lag(sum_catch.y)) / lag(sum_catch.y) * 100) %>%
#   ungroup %>%
#   
#   # keep only change >30%
#   mutate(ca_lag_perc_diff= ifelse(abs(ca_lag_perc_diff) > 30, ca_lag_perc_diff, NA)) %>%
#   
#   # keep timeperiod after survey
#   filter(year >= year_start -1 ) %>%
#   mutate(ca_lag_perc_diff= ifelse(year >= year_start, ca_lag_perc_diff, NA)) %>%
#   mutate(sel_readj = ifelse(!is.na(ca_lag_perc_diff), 'readj', NA)) %>%
#   
#   #group_by(country_code) %>%
#   mutate(sel_readj2 = ifelse(lead(sel_readj)=='readj' & country_code==lead(country_code),'readj', sel_readj)) %>%
#   mutate(sel_readj3 = ifelse(sel_readj=='readj' | sel_readj2=='readj','readj', NA)) %>%
#   
#   select(-one_of('sel_readj','sel_readj2')) %>%
#   mutate(sum_catch.y = ifelse(sel_readj3 =='readj', sum_catch.y, NA))
#   
#   #select(country_code, year, sel_readj3) %>%
#   #filter(sel_readj3 == 'readj')





# # subset to countries with BNP value
# sum_surv_ca <- sum_surv_ca %>%
#   filter(country_code %in%  bnp_cases$country_code) %>%
#   mutate(continent = countrycode(country_code, 'iso3c', 'continent',warn=FALSE),
#          country = countrycode(country_code, 'iso3c', 'country.name',warn=FALSE)) %>%
#   filter(country != 'Indonesia') %>%
#   mutate(Source_bnp=ifelse(!is.na(tr_corr_consump), "This study", Source_bnp))
# #   left_join(., max_percntry, by=c('country_code'='country_code'))



# 
# max_percntry <- sum_surv_ca     
# max_percntry[is.na(max_percntry)] <- 0
# 
# max_percntry <- max_percntry %>%
#   select(country_code, total_catch_bnp, p975, sum_catch.y) %>%
#   group_by(country_code) %>%
#   summarise_each(funs(max(., na.rm = TRUE))) %>%
#   ungroup
# 
# max_percntry$maxlim = apply(max_percntry[2:4], 1, function(x) max(x))



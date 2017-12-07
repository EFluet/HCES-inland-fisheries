


### Extract years after surveys to investigate if large changes have occured


# read survey data, incl. catch difference with FAO stats 
f <- '../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_diff.csv'
sum_surv <- read.csv(f, stringsAsFactors=FALSE)

# read FishStatJ catch data 
source("./fishstatj_data/read_nat_catch_data.R")
# RENAME sum_catch  to  sum_catch_fao
rm(f, ca)


# join on country and year, keeping years after survey
sum_surv_ca <- left_join(sum_surv, ca_sum_bycountry, 
                                  by = c('country_code'='country_code')) %>%
                filter(source =='Inland') %>%
                mutate(sum_catch.y = sum_catch.y / 10^6)




# # Calculate year on year change in % ---------------------------------------
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



sum_surv_ca<- sum_surv_ca %>%
      mutate(tr_corr_consump = ifelse(year==year_start,tr_corr_consump,NA),
             p025 = ifelse(year==year_start,p025,NA),
             p975 = ifelse(year==year_start,p975,NA)) %>%
      filter(year > 1980) %>%
      mutate(CountryCountry= ifelse(country_code=='SDN','Sudan (former)', 
                                    CountryCountry))
      # left_join(., sum_surv_ca, by=c('country_code'='country_code',
      #                                     'year'='year'))



### read Big Numbers Project data  & join in fao catch data -----------------------
bnp_cases <- read.csv('../data/catch_yield/big_num_proj/inland_catch_bnp.csv', 
                      stringsAsFactors=F)
colnames(bnp_cases) <- paste(colnames(bnp_cases), "bnp", sep = "_")

bnp_cases<- bnp_cases %>%
  mutate(country_code= countrycode(bnp_cases$country,
                                   'country.name','iso3c',warn=FALSE))

# join BNP to the catch table
sum_surv_ca <- bnp_cases %>%
        right_join(., sum_surv_ca, 
                   by=c('country_code'='country_code')) %>%
        mutate(total_catch_bnp = ifelse(year_start_bnp==year, 
                                        total_catch_bnp/10^6, NA))



# subset to countreis with BNP value
sum_surv_ca <- sum_surv_ca %>%
  filter(country_code %in%  bnp_cases$country_code) 
#   left_join(., max_percntry, by=c('country_code'='country_code'))

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




#  Make line plot of time and catch estimate -----------------------------------
source('./consumption/plots/themes/custom_catch_trend_line_theme.r')

l<- ggplot(sum_surv_ca, aes(x=year, y=sum_catch.y)) +
  
    # add line for FAO stats
    geom_line(aes(color='grey65'), size=0.65) +
  
    # geom_line(data=sum_surv_ca, 
    #           aes(x=year, y=sum_catch.y, color='black'), size=0.5) +
    
    geom_point(aes(x=year, y=total_catch_bnp, color='dodgerblue4'), 
             shape=15, size=1.8) +
  
    # add my estimates
    geom_point(aes(x=year, y=tr_corr_consump, color='red'), size=1.8)+
    geom_errorbar(aes(x=year, ymin=p025, ymax=p975, color='red'), 
                  width=0, size=0.3)+
  
    facet_wrap(~country_bnp, scales='free_y', ncol=4) +
    ylab("Official FAO Catch (Million tonnes)") + xlab("") +
    custom_catch_trend_line_theme + 

  
  scale_color_manual(values=c('red'='red','grey65'='grey65',
                              'dodgerblue4'='dodgerblue4'),
                     
                     labels=c('Big Number Project catch    ',
                              'Official FAO catch    ',
                              'HCES-estimated catch    '))+
  
  theme(legend.position = c(0.75, 0.3), 
        legend.direction = 'vertical',
        axis.ticks = element_line(colour='black'),
        plot.margin=unit(c(1, 2, -1, 5), "mm"))
  



# save figure to file ----------------------------------------------------------
ggsave('../output/figures/fig3_catch_trend_with_surv_facet_wBNP_w30p_v2_selcntries.png', l,
       width=210, height=115, dpi=800, units="mm", type = "cairo-png")
dev.off()


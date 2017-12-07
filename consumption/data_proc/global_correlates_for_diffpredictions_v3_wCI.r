

# get predictors for all countries globally ------------------------------------

# ~~~ get flag cnt aq, tr, ca   ---------------------------------------------
source('./consumption/data_proc/fishstatj_flag_tally/byweight/aq_fao_flag_tally_byweight.r')
source('./consumption/data_proc/fishstatj_flag_tally/byweight/tr_fao_flag_tally_byweight.r')
source('./consumption/data_proc/fishstatj_flag_tally/byweight/ca_fao_flag_tally_byweight.r')


# add row to aquaculture flag (because tr has one identifying import/export)
aq_flag_tally_agg$TradeflowTradeflow <- 'Aquaculture'
ca_flag_tally_agg$TradeflowTradeflow <- 'Catch'


### get all flag fao -  df from aq and tr
all_flag_tally_agg <- rbind(aq_flag_tally_agg, tr_flag_tally_agg) %>%
                    rbind(. , ca_flag_tally_agg) %>%  #%>% select(-one_of('flag','not_flag'))
                    # Cut the flags with % threshold
                    mutate(flag_perc = ifelse(flag_perc >0.4, 1, 0),
                           flag_perc = as.factor(flag_perc)) %>%
                    filter(year == 2008) %>%
                    select(country_code, TradeflowTradeflow, flag_perc) %>%
                    spread(TradeflowTradeflow, flag_perc) 

# fill gap in flags with 1 --- ? good idea?
all_flag_tally_agg[is.na(all_flag_tally_agg)] <- 1

colnames(all_flag_tally_agg)[2:5] <- paste(colnames(all_flag_tally_agg)[2:5],"flag", sep="_")

# remove parts of the rbind
rm(tr_flag_tally_agg, aq_flag_tally_agg, ca_flag_tally_agg)



# ~~~ get world bank indices -------------------------------------------------------
wdi <- read.csv('../data/socio_demo/wdi/WDI_Data.csv', stringsAsFactors=F)

colstokeep <- c('GDP per capita (constant 2010 US$)',
                'Land area (sq. km)',
                'Population density (people per sq. km of land area)',
                'Rural population',
                'Rural population (% of total population)',
                "Household final consumption expenditure (constant 2010 US$)" ,
                "Employment to population ratio, 15+, total (%) (national estimate)"  )


wdi_sel <- wdi %>%
  filter(Indicator.Name %in% colstokeep) %>%
  select(Country.Name, Country.Code, Indicator.Code, X2008) %>%
  spread(Indicator.Code, X2008) %>%
  mutate(country_code = countrycode(Country.Name, 'country.name', 
                                    'iso3c', warn = TRUE))



# ~~~ get nat_standzd imports, exports, aquaculture catch -----------------------

# read and format FW trade table
source('./fishstatj_data/read_nat_trade_data_fw.R', print.eval = TRUE)
source('./fishstatj_data/read_nat_aquaculture_data.R', print.eval = TRUE)
source('./fishstatj_data/read_nat_catch_data.R', print.eval = TRUE)



# filter aq and ca  to only freshwater catch
aq_sum_bycountry <- aq_sum_bycountry %>% 
                    filter(source == 'Inland', year == 2008) %>% 
                    ungroup() %>% select(-c(source, type))

ca_sum_bycountry <- ca_sum_bycountry %>% 
                    filter(source == 'Inland', year == 2008) %>% 
                    ungroup() %>% 
                    select(-c(source, type))

tr_sum_bycountry <- tr_sum_bycountry %>%
                    select(-source) %>%
                    filter(year == 2008) %>% 
                    ungroup()


# join summed ca, aq, tr data (only keep the consump countries)
nat_agg_df <- ca_sum_bycountry %>%
                      left_join(. , tr_sum_bycountry, 
                                by = c("country_code"="country_code")) %>%
                      left_join(. , aq_sum_bycountry, 
                                by = c("country_code"="country_code"))  %>%
                      mutate(export  = Export,
                             import = Import) %>%
                      select(-one_of('CountryCountry.x','CountryCountry.y', 
                                     'year.x', 'year.y', 'Export', 'Import')) %>%
                      filter(!is.na(CountryCountry))


# change NAs to zeros
nat_agg_df[is.na(nat_agg_df)] <- 0

nat_agg_df_stdzd <-  nat_agg_df %>%
              mutate_at(.cols = vars(export, import, sum_aquacul),
                        .funs = funs(./sum_catch)) %>%
              select(country_code, CountryCountry, year, sum_catch, sum_aquacul, export, import)

# rename stdzd column names
colnames(nat_agg_df_stdzd)[5:7] <- paste(colnames(nat_agg_df_stdzd)[5:7],"fao_stdzd", sep="_")

# remove temp data from env
rm(aq, aq_inl, ca, ca_sum_bycountry, aq_sum_bycountry)



# ~~~ get surface water area -------------------------------------------------------------------
f<-'../data/water_area/GSWD_GlobalEstimatesCalc_CoastCorrected_v2_23Dec2013_bycountry.csv'
w_area <- read.csv(f, stringsAsFactors=FALSE)

# Match country name with code, for later joiningO
w_area$country_code <- countrycode(w_area$Country, 
                                   'country.name', 'iso3c', warn = F)
w_area <- w_area %>%
  dplyr::select(Country, GLWD, MAMax, country_code) %>%
  filter(!is.na(country_code))


### join all predictors together -----------------------------------------------------------
glob_preds <- nat_agg_df_stdzd %>%
              left_join(., all_flag_tally_agg, by="country_code")  %>%
              left_join(., wdi_sel, by="country_code") %>%
              left_join(., w_area, by="country_code") %>%
              mutate(sum_catch = sum_catch/ 10^6) %>%
              mutate(MAMax_dens = MAMax / AG.LND.TOTL.K2)



### predict catch with full model -------------------------------------------
cols<-c("EN.POP.DNST", "SP.RUR.TOTL.ZS", "NY.GDP.PCAP.KD", "AG.LND.TOTL.K2", 
        'Aquaculture_flag', 'Catch_flag', 'Export_flag', 
        'Import_flag', 'export_fao_stdzd','import_fao_stdzd',
        'sum_aquacul_fao_stdzd', 'MAMax_dens')

# remove missing rows - without all predictors
glob_preds <- glob_preds[complete.cases(glob_preds[,cols]),]

psmulti<- predict(obj, select="all", newdata=glob_preds, se.fit=T, type = c("response"),
                  varweighting="Buckland", icmethod="Lukacs", alphaIC=0.05)



glob_preds$psmulti_pred_avg <- c(psmulti$averages)  # write predictions from average model
glob_preds$psmulti_ci <- as.data.frame(psmulti$variability)$`+/- (alpha=0.05)`


# filter outlier predictions----------------------------------------------
glob_preds_forplot <- glob_preds %>%
  
              # fitler 
              mutate(continent = countrycode(country_code, "iso3c", "continent")) %>%
              #filter(!continent %in% c('Europe','Oceania')) %>%
              #filter(!Country %in% c('Canada','United States')) %>%
              filter(!country_code %in% c('JAM','CUB')) %>%
  
              # filter outlier preds
              filter(psmulti_pred_avg < 2000) %>%
              filter(psmulti_pred_avg > 1) %>%
              filter(NY.GDP.PCAP.KD <= max(surv_data$NY.GDP.PCAP.KD)) %>%
  
              # calculate pred catch
              mutate(pred_catch =  sum_catch * psmulti_pred_avg/100)  %>%
              mutate(pred_catch_diff = pred_catch - sum_catch) %>%
  
              # calc ci of pred catch
              mutate(pred_catch_ci = sum_catch * (psmulti_ci/100),
                     pred_catch_upr = sum_catch * ((psmulti_pred_avg + psmulti_ci) /100),
                     pred_catch_lwr = sum_catch * ((psmulti_pred_avg - psmulti_ci) /100))



# prep cols to join survey data
surv_data_catch <- surv_data %>% #surv_data_diff
                   select(country_code, tr_corr_consump, hces_surv_asprcfao)


### add & replace countries in global total ------------------------------------
nat_agg_df2 <-nat_agg_df %>%

  
    left_join(., glob_preds_forplot, by='country_code') %>%
    full_join(., surv_data_catch, by='country_code') %>%
    mutate(fao_catch_M = sum_catch.x / 10^6) %>%
    select(country_code, psmulti_pred_avg, psmulti_ci, hces_surv_asprcfao, 
           fao_catch_M, tr_corr_consump, pred_catch, pred_catch_ci) %>%
  
    # make global aggregate figure
    mutate(agg_pred_fao_catch = ifelse(is.na(pred_catch)&is.na(tr_corr_consump), fao_catch_M, pred_catch),
           agg_pred_fao_catch = ifelse(!is.na(tr_corr_consump), tr_corr_consump, agg_pred_fao_catch),
           combined_val_type =  ifelse(is.na(pred_catch)&is.na(tr_corr_consump), 'FAO','predicted'),
           combined_val_type =  ifelse(!is.na(tr_corr_consump), 'survey', combined_val_type)) 


# n, sum & CI of components of global aggregate -------------------------------

# Survey
nrow(nat_agg_df2[which(nat_agg_df2$combined_val_type=='survey'),])
sum(nat_agg_df2[which(nat_agg_df2$combined_val_type=='survey'),'agg_pred_fao_catch'])
# CI from the monte carlo

# model prediction
nrow(nat_agg_df2[which(nat_agg_df2$combined_val_type=='predicted'),])
sum(nat_agg_df2[which(nat_agg_df2$combined_val_type=='predicted'),'agg_pred_fao_catch'])
sum(nat_agg_df2[which(nat_agg_df2$combined_val_type=='predicted'),'fao_catch_M'])
sqrt(sum((nat_agg_df2[which(nat_agg_df2$combined_val_type=='predicted'),'pred_catch_ci'])^2))

# fao stats filling rest
nrow(nat_agg_df2[which(nat_agg_df2$combined_val_type=='FAO'),])
sum(nat_agg_df2[which(nat_agg_df2$combined_val_type=='FAO'),'agg_pred_fao_catch'])
# no CI



# global aggregate ----------------------------------
nrow(nat_agg_df2)
sum(nat_agg_df2$agg_pred_fao_catch)

combined_ci_pred <- sqrt(sum((nat_agg_df2[which(nat_agg_df2$combined_val_type=='predicted'),'pred_catch_ci'])^2))
survey_ci <- (11.42 - 7.12) 

# calc ci
glob_ci <- sqrt(sum(combined_ci_pred^2 + survey_ci^2))

# calc upr & lwr CI bounds
sum(nat_agg_df2$agg_pred_fao_catch) - glob_ci
sum(nat_agg_df2$agg_pred_fao_catch) + glob_ci
 


# calcualte % for paper  -------------------------------------------------------

# percentage of FAO reporting re-restimated by survey or model
sum(nat_agg_df2[which(nat_agg_df2$combined_val_type %in% c('survey','predicted')),'fao_catch_M'], na.rm=T) / sum(nat_agg_df2$fao_catch_M, na.rm = T)


# percentage difference of global agg and fao
sum(nat_agg_df2$agg_pred_fao_catch) / sum(nat_agg_df2$fao_catch_M, na.rm = TRUE)



# 
# 
# # global statistics on predicted catch -----------------------------------
# 
# # truly global sums
# sum(nat_agg_df2$fao_catch_M, na.rm = TRUE)  # reported FAO catch
# sum(nat_agg_df2$pred_catch, na.rm = TRUE)   # model predictions
# sum(nat_agg_df2$tr_corr_consump, na.rm = TRUE)   # survey estim
# sum(nat_agg_df2$agg_pred_fao_catch, na.rm = TRUE)  # model pred & reported
# 
# 



# sum(glob_preds_forplot$sum_catch)   # sum of reported catch  
# sum(glob_preds_forplot$pred_catch)  # sum of modeled estimates
# 
# # calculate % catch difference in countries included
# sum(glob_preds_forplot$pred_catch) -  sum(glob_preds_forplot$sum_catch)
# sum(glob_preds_forplot$pred_catch)/ sum(glob_preds_forplot$sum_catch)
# 
# # calculate % catch difference in countries included
# sum(nat_agg_df2$pred_catch, na.rm = TRUE)/ sum(nat_agg_df2$fao_catch_M)
# sum(glob_preds_forplot$sum_catch) / sum(nat_agg_df2$fao_catch_M, na.rm = TRUE)




# ~~~ plot pred and reported catch MT  -----
ggplot(nat_agg_df2_forplot) +
  geom_point(aes(x=fao_catch_M, y=pred_catch, color=combined_val_type))+
  geom_abline(slope=1, intercept=0, color='grey70', size=0.2) +
  
  # add labels
   geom_text_repel(data=nat_agg_df2_forplot,
  #data= subset(nat_agg_df2_forplot, 
  #                              pred_catch/fao_catch_M > 2 | pred_catch/fao_catch_M < 0.5 & 
  #                                fao_catch_M > 0.2, pred_catch > 0.2),
                  aes(label=country_code, x=fao_catch_M, y=pred_catch), 
                  size = 2, 
                  colour='gray15', 
                  segment.size = 0.25, 
                  segment.color='gray55',
                  force = 8,
                  box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines'))


#   # add labels
#   geom_text_repel(data= subset(surv_data_sel, (hces_surv_asprcfao > 210 | psmulti > 250)),
#                   aes(label=country_label.x, x=hces_surv_asprcfao, y=psmulti), 
#                   size = 2, 
#                   colour='gray15', 
#                   segment.size = 0.25, 
#                   segment.color='gray55',
#                   force = 8,
#                   box.padding = unit(0.2, 'lines'),
#                   point.padding = unit(0.2, 'lines')) +
#   
#   #xlim(0, 1350) + ylim(0, 1350) +
#   xlab('Ratio of survey-estimated over reported catch') +
#   ylab('Predicted ratio') +
#   
#   custom_scatterplot_theme +
#   theme(axis.line = element_line(colour = "black"),
#         legend.position = 'none', #c(0.115, .83), 
#         text = element_text(size=7),
#         legend.key = element_rect(colour = "white"),
#         legend.background = element_rect(colour = "black", size=0.2),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.title = element_blank(),
#         panel.border = element_rect(colour = "black"),
#         panel.background = element_blank())


# 
# 
# 
# # ~~~ plot catch -------
# ggplot(glob_preds_forplot) +
#   geom_histogram(aes(x=pred_catch), stat='bin', position='stack')
# 
# # ~~~ plot the density graphs of training and predicted diff in catch ---------
# ggplot() +
#   #geom_histogram(aes(x=pred_catch), stat='bin', position='stack') +
#   geom_density(data= glob_preds_forplot , aes(x=log(psmulti_pred_avg))) +
#   geom_density(data= surv_data_sel , aes(x=log(hces_surv_asprcfao)), color ='blue')
# 
# 
# 
# 
# 
# #save plot 
# ggsave("../Output/Figures/density_dist_train_pred.png",
#        dpi=800, width=90, height=100, units='mm', type = "cairo-png")
# 
# dev.off()
# 

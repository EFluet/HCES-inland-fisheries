

### get flag cnt aq, tr, ca   ---------------------------------------------
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
                    filter(year == 2010) %>%
                    select(country_code, TradeflowTradeflow, flag_perc) %>%
                    spread(TradeflowTradeflow, flag_perc)   


all_flag_tally_agg[is.na(all_flag_tally_agg)] <- 1

# all_flag_tally_agg <- all_flag_tally_agg %>%
#                       gather(TradeflowTradeflow, flag_perc, Aquaculture:Import)

colnames(all_flag_tally_agg)[2:5] <- paste(colnames(all_flag_tally_agg)[2:5],"flag", sep="_")

# remove parts of the rbind
rm(tr_flag_tally_agg, aq_flag_tally_agg, ca_flag_tally_agg)



### get world bank indices -------------------------------------------------------
wdi <- read.csv('../data/socio_demo/wdi/WDI_Data.csv', stringsAsFactors=F)

colstokeep <- c('Land area (sq. km)',
                'Population density (people per sq. km of land area)',
                'Rural population',
                'Rural population (% of total population)',
                "Household final consumption expenditure (constant 2010 US$)" ,
                "Employment to population ratio, 15+, total (%) (national estimate)"  )


wdi_sel <- wdi %>%
  filter(Indicator.Name %in% colstokeep) %>%
  select(Country.Name, Country.Code, Indicator.Code, X2010) %>%
  spread(Indicator.Code, X2010) %>%
  mutate(country_code = countrycode(Country.Name, 'country.name', 
                                    'iso3c', warn = TRUE))



### get standzd imports, exports, aquaculture catch -----------------------

# run script that formats the trade data 
#source('./fishstatj_data/read_nat_trade_data_fw.R', print.eval = TRUE)

# read and format FW trade table
source('./fishstatj_data/read_nat_trade_data_fw.R', print.eval = TRUE)
source('./fishstatj_data/read_nat_aquaculture_data.R', print.eval = TRUE)
source('./fishstatj_data/read_nat_catch_data.R', print.eval = TRUE)



# filter aq and ca  to only freshwater catch
aq_sum_bycountry <- aq_sum_bycountry %>% 
                    filter(source == 'Inland', year == 2010) %>% 
                    ungroup() %>% select(-c(source, type))

ca_sum_bycountry <- ca_sum_bycountry %>% 
                    filter(source == 'Inland', year == 2010) %>% 
                    ungroup() %>% 
                    select(-c(source, type))

tr_sum_bycountry <- tr_sum_bycountry %>%
                    select(-source) %>%
                    filter(year == 2010) %>% 
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





### get surface water area -------------------------------------------------------------------
f<-'../data/water_area/GSWD_GlobalEstimatesCalc_CoastCorrected_v2_23Dec2013_bycountry.csv'
w_area <- read.csv(f, stringsAsFactors=FALSE)

# Match country name with code, for later joiningO
w_area$country_code <- countrycode(w_area$Country, 
                                   'country.name', 'iso3c', warn = F)
w_area <- w_area %>%
  dplyr::select(Country, GLWD, MAMax, country_code) %>%
  filter(!is.na(country_code))




### Call script that calculates the prod percapita per country 
# # It uses inland catch and population from FAO and area from GLWD.
# source('./fishstatj_data/join_nat_catch_pop_wat_2012.R', print.eval=TRUE)
# 
# # add the numeric country code to table
# prod_percap_df <- prod_percap_df %>%
#   mutate(iso3n = countrycode(prod_percap_df$Country, 'country.name', 
#                              'iso3n', warn = TRUE)) %>%
#   # somehow got duplicated rows of country_code
#   group_by(country_code) %>%
#   filter(row_number()==1) %>%
#   select(country_code, GLWD)




### join all together -----------------------------------------------------------
# f<-'../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv'
# surv_data <- read.csv(f, stringsAsFactors = FALSE)

glob_preds <- nat_agg_df_stdzd %>%
              left_join(., all_flag_tally_agg, by="country_code")  %>%
              left_join(., wdi_sel, by="country_code") %>%
              left_join(., w_area, by="country_code") %>%
              #left_join(., nat_agg_df_stdzd, by="country_code") %>%
              mutate(sum_catch = sum_catch/ 10^6)





### read consumption data file ---------------------------------------------------
# # used to selct the
# consump_nat_agg_dif <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm.csv', 
#                                 stringsAsFactors=F)
# 
# #  make unique list of country and year of surveys 
# surv_cntry_yr <- unique(consump_nat_agg_dif[c('country_code','year_start')]) 




### predict globally -------------------------------------------

# colname of predictors
cols<-c("EN.POP.DNST", "NE.CON.PRVT.KD", "SP.RUR.TOTL", "sum_catch",
        "SP.RUR.TOTL.ZS", "GLWD", "AG.LND.TOTL.K2", "sum_catch",
        'Aquaculture_flag', 'Catch_flag', 'Export_flag', 
        'Import_flag', 'export_fao_stdzd','import_fao_stdzd',
        'sum_aquacul_fao_stdzd', 'hces_surv_asprcfao')


# choose predictor columns
cols <- cols[1:12]

# remove missing rows - without all predictors
glob_preds <- glob_preds[complete.cases(glob_preds[,cols]),]


#b <- glob_preds[complete.cases(glob_preds[,cols]),cols]

# lasso predict
#glob_preds$ps <- predict(glmmod, newx = data.matrix(b), type = "response", s = 0.05)

#  stepwise predict
# glob_preds$ps <-  predict(bothways, newdata = glob_preds,
#                             type = c("response"),
#                             se.fit = FALSE, dispersion = NULL, terms = NULL,
#                             na.action = na.pass)


  
psmulti<- predict(obj, select="all", newdata=glob_preds, se.fit=T, type = c("response"),
                  varweighting="Buckland", icmethod="Lukacs", alphaIC=0.05)

glob_preds$psmulti <- c(psmulti$averages)


glob_preds_forplot <- glob_preds %>%
              mutate(continent = countrycode(country_code, "iso3c", "continent")) %>%
              filter(!continent %in% c('Europe','Oceania'),
                     psmulti < 2000) %>%
               mutate(pred_catch = psmulti/100 * sum_catch) 

 
  

# plots ---------------

# ~~~ plot predicted % -----
ggplot(glob_preds_forplot) +
  geom_histogram(aes(x=psmulti), stat='bin', position='stack')

# ~~~ plot catch -------
ggplot(glob_preds_forplot) +
  geom_histogram(aes(x=pred_catch), stat='bin', position='stack')

# ~~~ plot the density graphs of training and predicted diff in catch ---------
ggplot() +
  #geom_histogram(aes(x=pred_catch), stat='bin', position='stack') +
  geom_density(data= glob_preds_forplot , aes(x=log(psmulti))) +
  geom_density(data= surv_data_sel , aes(x=log(hces_surv_asprcfao)), color ='blue')




#save plot 
ggsave("../Output/Figures/density_dist_train_pred.png",
       dpi=800, width=90, height=100, units='mm', type = "cairo-png")

dev.off()





# global statistics on predicted catch -----------------------------------

# calculate % catch difference in countries included
sum(glob_preds_forplot$pred_catch)/ sum(glob_preds_forplot$sum_catch)

sum(glob_preds_forplot$pred_catch)

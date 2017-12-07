# read data in for regressions


#Read world bank indices -------------------------------------------------------
wdi <- read.csv('../data/socio_demo/wdi/WDI_Data.csv', stringsAsFactors=F)

colstokeep <- c('Land area (sq. km)',
                'Population density (people per sq. km of land area)',
                'Rural population',
                'Rural population (% of total population)',
                "Household final consumption expenditure (constant 2010 US$)" ,
                "Employment to population ratio, 15+, total (%) (national estimate)"  )


wdi_sel <- wdi %>%
  filter(Indicator.Name %in% colstokeep) %>%
  select(Country.Name, Country.Code, Indicator.Code, X2012) %>%
  spread(Indicator.Code, X2012)




### Call script that calculates the prod percapita per country 
# It uses inland catch and population from FAO and area from GLWD.
source('./fishstatj_data/join_nat_catch_pop_wat_2012.R', print.eval=TRUE)

# add the numeric country code to table
prod_percap_df <- prod_percap_df %>%
  mutate(iso3n = countrycode(prod_percap_df$Country, 'country.name', 
                             'iso3n', warn = TRUE)) %>%
  # somehow got duplicated rows of country_code
  group_by(country_code) %>%
  filter(row_number()==1)





# read the FAO data flags ------------------------------------------------------
#source('./consumption/data_proc/fishstatj_flag_tally/combined_fao_flag_tally_v2.r')
# source('./consumption/data_proc/fishstatj_flag_tally/byweight/combined_fao_flag_tally_v3_byweight.r')
# 
# 
# all_flag_tally_agg <- all_flag_tally_agg %>%
#       select(country_code, flag_perc, TradeflowTradeflow) %>%
#       spread(TradeflowTradeflow, flag_perc)
# 
# 
# names(all_flag_tally_agg) <- c('country_code',
#                               "aquaculture_flag_perc",
#                               "catch_flag_perc",
#                               "export_flag_perc",
#                               "import_flag_perc")


# join data tables -------------------------------------------------------------
surv_consump_df_forreg <- surv_consump_df %>%
  left_join(., prod_percap_df, by='country_code') %>%
  left_join(., wdi_sel, by=c('country_code'='Country.Code')) %>%
  left_join(., all_flag_tally_agg, by=c('country_code'='country_code')) %>%
  mutate(GLWD_dens = GLWD/AG.LND.TOTL.K2,
         pop_dens = tot_pop_both_sexes/AG.LND.TOTL.K2,
         pop_dens_per_glwd = tot_pop_both_sexes/GLWD)

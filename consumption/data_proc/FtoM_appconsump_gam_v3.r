
library(nlstools)
library(nls2)
library(car)
library(ggrepel)
library(simpleboot)



### Read coastline length -------------------------------------------------------
coast_length <- read.csv("../data/consumption/country_coastline_length.csv", 
                         stringsAsFactors = F)

coast_length <- coast_length %>%
  mutate(country_code = countrycode(country,'country.name','iso3c',warn=F)) %>%
  select(-one_of('date','source'))



### Read Inland water surface area -------------------------------------------------------------------
f<-'../data/water_area/GSWD_GlobalEstimatesCalc_CoastCorrected_v2_23Dec2013_bycountry.csv'
w_area <- read.csv(f, stringsAsFactors=FALSE)

# Match country name with code, for later joiningO
w_area$country_code <- countrycode(w_area$Country, 
                                   'country.name', 'iso3c', warn = F)
w_area <- w_area %>%
  dplyr::select(Country, GLWD, MAMax, country_code) %>%
  filter(!is.na(country_code))




### Read and format apparent consumption table ---------------------------------
path <- "../data/consumption/FAOSTAT_data_4-4-2017_fish_apparent_consump.csv"
appcon <- read.csv(path)
rm(path)


### calculate the fraction of inland consumption from apparent consumption -----
appcon<- appcon %>%
  select(Country, Item, Year, Value) %>%
  filter(Year > 1995,
         !Item %in% c('Aquatic Plants','Meat, Aquatic Mammals','Fish, Body Oil', 'Fish, Liver Oil')) %>%
  mutate(Value = Value * 10^3) %>%
  mutate(Source = ifelse(Item == 'Freshwater Fish', 'Freshwater', 'Marine')) %>%
  group_by(Country, Year, Source) %>%
  summarize(Value = sum(Value)) %>%
  ungroup %>%
  spread(Source, Value) %>%
  mutate(Freshwater = ifelse(is.na(Freshwater), 0, Freshwater),
         Marine = ifelse(is.na(Marine), 0, Marine),
         F_tototal = Freshwater / (Freshwater + Marine)) %>%
  select(Country, Year, F_tototal) %>%
  filter(!is.na(F_tototal))  %>%
  mutate(country_code = countrycode(Country, 'country.name', 'iso3c', warn = F)) %>%
  filter(Country != 'China, mainland') %>%
  group_by(country_code) %>%
  summarize(F_tototal = mean(F_tototal)) %>%
  ungroup %>%
  full_join(., coast_length, by='country_code') %>%
  mutate(clpa = coastline_lenght_perarea_km) %>%
  mutate(continent = countrycode(country_code, 'iso3c', 'continent', warn = F)) %>%
  full_join(., w_area, by='country_code') %>%
  mutate(clpa = coastline_length_km / MAMax) 


### read and join the GDP/capita data -------------------------------------------

f <- '../data/wdi/gdp_percapita/API_NY.GDP.PCAP.CD_DS2_en_csv_v2.csv'
wdi_gdp <- read.csv(f, stringsAsFactors=FALSE)

# remove all '.' and 'X' in the column names 
colnames(wdi_gdp) <- gsub("[.]|X","",colnames(wdi_gdp))


wdi_gdp<- wdi_gdp %>%
  gather(year, NY.GDP.PCAP.CD, 5:ncol(wdi_gdp)) %>%
  select(CountryName, CountryCode, year, NY.GDP.PCAP.CD) %>%
  mutate(year= as.numeric(year)) %>%
  filter(!is.na(NY.GDP.PCAP.CD),
         year >= 1995) %>%
  group_by(CountryName, CountryCode) %>%
  summarize(meanNY.GDP.PCAP.CD = mean(NY.GDP.PCAP.CD))


appcon <- appcon %>%
  left_join(., wdi_gdp, by=c('country_code' = 'CountryCode')) %>%
  filter(!is.na(meanNY.GDP.PCAP.CD))




### Subset ---------------------------------------------------------------------

#continent != 'Europe',
#!country %in% c('Canada', 'United States'))
#,'Cuba','Bahamas','Jamaica', 'Cyprus', 'Trinidad and Tobago','Japan','Haiti')

### save a limited 
appcon_unfilt<- appcon %>%
  select(country_code, clpa, meanNY.GDP.PCAP.CD) %>%
  filter(!is.na(clpa), clpa != Inf)

appcon <- appcon %>%
  filter(F_tototal > 0, 
         !is.na(clpa), 
         clpa != Inf,
         clpa < 500,
         meanNY.GDP.PCAP.CD < 10000)





### ---------------------------------------------------
library(mgcv)

# create temp 3Dimensions
x<-appcon$clpa
y<-appcon$meanNY.GDP.PCAP.CD
z<-appcon$F_tototal

appcon$x<-appcon$clpa
appcon$y<-appcon$meanNY.GDP.PCAP.CD
appcon$z<-appcon$F_tototal


# fit GAM model
b1 <- gam(z ~ s(x, k=7)+ s(y, k=7),
          #fit=TRUE,
          family = gaussian)

summary(b1)
gam.check(b1)

### bootstrap the GAM model
data<-appcon[,c('clpa', 'meanNY.GDP.PCAP.CD', 'F_tototal')]
names(data) <- c('x','y','z')


sampled_models <- data.frame(nrep = seq_len(100)) %>% 
  group_by(nrep) %>% 
  do(data[data %>% nrow %>% sample.int(replace = TRUE),]) %>% 
  do(model = gam(z ~ s(x, k=7)+ s(y, k=7), data=.))


#lm(z ~ poly(x, 3) + poly(y, 3)
#method="lm", formula=y~poly(x, 2)




### Make prediction over countries, for use in survey calculation
boot_pred <- sampled_models %>% 
  rowwise %>% 
  do(data.frame(z_hat = predict(.$model,list(x = appcon_unfilt$clpa, 
                                             y = appcon_unfilt$meanNY.GDP.PCAP.CD)))) %>%
  ungroup %>%
  cbind(country_code = appcon_unfilt$country_code,
        clpa = appcon_unfilt$clpa,
        gdp_percap = appcon_unfilt$meanNY.GDP.PCAP.CD) %>%
  group_by(country_code, clpa, gdp_percap) %>%
  summarize(mean = mean(z_hat),
            up = quantile(z_hat, probs = 0.975),
            lo = quantile(z_hat, probs = 0.025))




write.csv(boot_pred, '../output/bootpred_inland_consump_fraction_gam_pred.csv')



# 
# # Predictions for the 0-1 
# boot_pred_even <- sampled_models %>% 
#   rowwise %>% 
#   do(data.frame(z_hat = predict(.$model,list(x = xs_evendist, y = ys_evendist)))) %>%
#   ungroup %>%
#   cbind(xs=xs_evendist, ys=ys_evendist) %>%
#   group_by(xs, ys) %>%
#   #mutate(run = rep(1:100, each = 500)) %>%
#   summarize(mean = mean(z_hat),
#             p975 = quantile(z_hat, probs = 0.975),
#             p025 = quantile(z_hat, probs = 0.025))
# 
# 
# 
# # 2D plot prediction on grid
# ggplot(boot_pred_even) + 
#   
#   geom_ribbon(aes(x = xs, ymin=p025, ymax=p975), color='grey', alpha=0.3) +
#   coord_cartesian(ylim=c(0,1))
# 
# #+
#   geom_point(data=boot_pred, aes(x = x, y = z), color='red')+
#   geom_line(aes(x = xs, y = mean))  +
#   geom_abline(intercept=0, slope=0, color= 'grey15')+
# 
# 
# 


### plot
#source('./consumption/data_proc/fm_gam_model_plotting.r')




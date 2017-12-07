
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


### Subset ---------------------------------------------------------------------
appcon_unfilt<- appcon %>%
  select(country_code, clpa) %>%
  filter(!is.na(clpa), clpa != Inf)


#continent != 'Europe',
#!country %in% c('Canada', 'United States'))
#,'Cuba','Bahamas','Jamaica', 'Cyprus', 'Trinidad and Tobago','Japan','Haiti')



### read and join the GDP/capita data -------------------------------------------

# read the code-assigned product list
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


### Subset so mo' --------------------------------------------------------------
appcon <- appcon %>%
  filter(F_tototal > 0, 
         !is.na(clpa), 
         clpa != Inf,
         clpa < 215,
         meanNY.GDP.PCAP.CD < 10000)


### ---------------------------------------------------
library(mgcv)

# create temp 3Dimensions
x<-appcon$clpa
y<-appcon$meanNY.GDP.PCAP.CD
z<-appcon$F_tototal

# fit GAM model
b1 <- gam(z ~ s(x) + s(y),
          fit=TRUE,
          family = gaussian)

summary(b1)
gam.check(b1)



## plot in RGL ---------------------------------------------------------------

# create grid on which predictions will be made
grid <- expand.grid(x = seq(0, max(x), length=50), 
                    y = seq(0, max(y), length=50))

# predict value at grids
grid$z <- predict(b1, newdata=grid) 

matrixz <- matrix(grid$z, 
                  nrow=length(unique(grid$x)), 
                  ncol=length(unique(grid$y)))



# rotatable 3D plot of points and spline surface -----------------------------
library(rgl)
open3d(windowRect=c(100, 100, 500, 500))

surface3d(unique(grid$x), unique(grid$y),  z= matrixz, alpha=0.5,  back= "lines", lit=FALSE) 
#wire3d(unique(grid$x), unique(grid$y),  z= matrixz) 
points3d(x,y,z, col='red', size=8)
aspect3d(1, 1, 1)
title3d(xlab="clpa",ylab="gdp",zlab="F:M")
axes3d()

rgl.viewpoint(theta=0, phi=15)

Sys.sleep(1)
#rgl.snapshot(filename = "persp3dd.pdf")
rgl.postscript("persp3dd.pdf",fmt="pdf")

#rgl.postscript("../output/figures/persp3dd.pdf","pdf")
#rgl.postscript("persp3dd.pdf","pdf")




### Fit LOESS ---------------------------------------------------
F_tototal<-appcon$F_tototal
clpa<-appcon$clpa


lo <- loess(F_tototal ~ clpa, span=0.8)


lo.b <- loess.boot(lo,  R=10000, rows=TRUE)#, ngrid = 90, new.xpts = NULL, weights = NULL)

# get fitted values
f <- as.data.frame(samples(lo.b, name = c("fitted")))

conf_97.5 <- apply(as.data.frame(f), 1, function(x) quantile(x, .975, na.rm=TRUE))
conf_2.5 <- apply(as.data.frame(f), 1, function(x) quantile(x, .025, na.rm=TRUE))

# append the 95% c.i. bounds to the df
loess_output <- as.data.frame(cbind(predict(lo), conf_2.5, conf_97.5))
names(loess_output) <- c("loess_pred","loess_conf_2.5", "loess_conf_97.5") 


# combined colums of appcon and loess
appcon <- cbind(appcon, loess_output)


# apply a filter for labels in the plot
appcon <- appcon %>%
  mutate(label= ifelse(F_tototal > loess_conf_97.5 * 2 | F_tototal < loess_conf_2.5 / 2 , 
                       'Y', 'N'))



# 
# ### 2D Scatterplot of countries ---------------------------------------------------
# source('./consumption/plots/themes/custom_scatterplot_theme.r')
# 
# ggplot(appcon, aes(x=clpa, y=F_tototal)) +
#   
#   geom_ribbon(aes(x=appcon$clpa, ymin=appcon$loess_conf_2.5, ymax=appcon$loess_conf_97.5), fill='blue', alpha=0.1) +
#   geom_line(aes(x=appcon$clpa, y=loess_pred), color='blue',size= 1, alpha=0.6) +
#   geom_point(aes(color=continent), size=3, shape=21, stroke=2, alpha=1) +
#   
#   
#   xlab('Coastline length per unit surface water area (GIEMS-D15; MAMax)') +
#   ylab('Percentage freshwater in total fish \napparent consumption FAO (avg:1995-2014)') +
#   coord_cartesian(ylim=c(0,1))+
#   #facet_wrap(~continent) +
#   geom_text_repel(aes(label=Country, x= clpa, y= F_tototal),
#                   data = subset(appcon, label =='Y'),
#                   size = 3,
#                   colour='gray15',
#                   force = 8,
#                   segment.size = 0.25,
#                   segment.color='gray55',
#                   box.padding = unit(0.2, 'lines'),
#                   point.padding = unit(0.2, 'lines'))  + 
#   custom_scatterplot_theme +
#   theme(legend.position = c(0.75, 0.75))
# #annotate("text", x = 150, y = .85, label = "f(x)= 0.69 * e^(-0.037x) \n R^2=0.74")
# 
# 
# ### save plot ------------------------------------------------------------------
# ggsave("../Output/Figures/FtoM_withFAO_apparentconsump_bootloess.png",
#        dpi=600, width=210, height=180, units='mm', type = "cairo-png")
# dev.off()
# 


### 
# because this script is sourced in data processing
# remove 

rm(lo.b, loess_output, w_area, coast_length, appcon, F_tototal, 
   custom_scatterplot_theme, conf_2.5, conf_97.5)

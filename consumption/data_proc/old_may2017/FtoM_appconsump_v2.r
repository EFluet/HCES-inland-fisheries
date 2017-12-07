
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



### Wet area -------------------------------------------------------------------
f<-'../data/water_area/GSWD_GlobalEstimatesCalc_CoastCorrected_v2_23Dec2013_bycountry.csv'
w_area <- read.csv(f, stringsAsFactors=FALSE)

# Match country name with code, for later joiningO
w_area$country_code <- countrycode(w_area$Country, 
                                   'country.name', 'iso3c', warn = F)
w_area <- w_area %>%
  dplyr::select(Country, GLWD, MAMax, country_code) %>%
  filter(!is.na(country_code))




### Read and format tr table ---------------------------------------------------
path <- "../data/consumption/FAOSTAT_data_4-4-2017_fish_apparent_consump.csv"
appcon <- read.csv(path)
rm(path)

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


appcon_unfilt <- appcon %>%
                 select(country_code, clpa) %>%
                 filter(!is.na(clpa), clpa != Inf)


appcon <- appcon%>%
          filter(F_tototal > 0, 
                 clpa < 215, 
                 continent != 'Europe')



###   Island codes
#island_codes <- c('CUB','SJM',)

#loess(formula, data, weights)

F_tototal<-appcon$F_tototal
clpa<-appcon$clpa



lo <- loess(F_tototal ~ clpa, span=.8)

plot(F_tototal ~ clpa, data=appcon,pch=19,cex=0.1)
j <- order(appcon$clpa)
lines(appcon$clpa[j],l$fitted[j],col="red",lwd=3)

lo.b <- loess.boot(lo,  R=100, ngrid = 90, new.xpts = NULL, ngrid=1000, weights = NULL)
                  #, rows = TRUE, new.xpts = NULL, ngrid = 100,
           #weights = NULL)

## Plot original fit with +/- 2 std. errors
plot(lo)

## Plot all loess bootstrap fits
plot(lo.b, all.lines = TRUE)

conf_97.5 <- apply(loess_boot$t, 2, function(x) quantile(x, .975))
conf_2.5 <- apply(loess_boot$t, 2, function(x) quantile(x, .025))



#################################################

plot(appcon$clpa, appcon$F_tototal, pch = 20, col = "black", xlab = "Car speed in mph",
     ylab = "Stopping distance in ft", main = "Speed and stopping distances of cars")

for(i in sample(nrow(lo.b$t), 20)) {
  lines(seq(4, 25, 1), loess_boot$t[i,], col = "gray")
}

lines(cars$speed, loess(dist ~ speed, cars)$fitted, lwd = 3,
      col = "tomato")




plot(lo.b)

############################################################################
##    Below is code for testing regressions
############################################################################

# fit model
m <- nls(data=appcon, F_tototal ~ a * exp(b*clpa), start=list(a=1,b=0))

# get model parameters
summary(m)
# get correction r^2
cor(appcon$F_tototal,predict(m))


# bootstrap the predictions
# contains the bootstrapped parameters (i think)
boots <- bootCase(m, B=10000)

# create function of the formulat and apply the bootstrapped parameters for predictions
fx <- function(clpa, a, b){a * exp(b*clpa)}
appcon_boot_preds <- apply(appcon[,'clpa'], 1, function(x) fx(x, a=boots[,"a"], b=boots[,"b"]))

# calculate 95% c.i. bounds
q <- t(apply(appcon[,'clpa'], 1, function(x) quantile(fx(x, a=boots[,"a"], b=boots[,"b"]),probs=c(0.025, 0.975))))
q <- as.data.frame(round(q,5))
names(q) <- c('p025','p975')

# append the 95% c.i. bounds to the df
appcon <- cbind(appcon, predict(m), q)



# apply a filter for labels in the plot
appcon <- appcon %>%
          mutate(label= ifelse(F_tototal > p975 * 2 | F_tototal < p025 / 2 , 
                               'Y', 'N'))





### Scatterplot of countries ---------------------------------------------------

ggplot(appcon, aes(x=clpa, y=F_tototal)) +

  geom_ribbon(aes(x=appcon$clpa, ymin=appcon$p025, ymax=appcon$p975), fill='blue', alpha=0.1) +
  geom_line(aes(x=appcon$clpa, y=predict(m)), color='blue',size= 1, alpha=0.6) +
  geom_point(aes(color=continent), size=3, shape=21, stroke=2, alpha=1) +
    
  xlab('Coastline length per unit surface water area (GIEMS-D15; MAMax)') +
  ylab('Percentage of freshwater fish weight in total fish \napparent consumption from FAO (avg:1995-2014)') +
  coord_cartesian(ylim=c(0,1))+
  #facet_wrap(~continent)
  geom_text_repel(aes(label=Country, x= clpa, y= F_tototal),
                  data = subset(appcon, label =='Y'),
                  size = 4,
                  colour='gray15',
                  force = 8,
                  segment.size = 0.25,
                  segment.color='gray55',
                  box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines')) +
  annotate("text", x = 150, y = .85, label = "f(x)= 0.69 * e^(-0.037x) \n R^2=0.74")


### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/FtoM_withFAO_apparentconsump.png",
       dpi=600, width=210, height=180, units='mm', type = "cairo-png")
dev.off()

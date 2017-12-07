
library(nlstools)
library(nls2)
library(car)




### Read coastline length -------------------------------------------------------
coast_length <- read.csv("../data/consumption/country_coastline_length.csv", 
                         stringsAsFactors = F)
coast_length <- coast_length %>%
  mutate(country_code = countrycode(country,'country.name',
                                    'iso3c',warn=F)) #%>%




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
          mutate(country_code = countrycode(Country, 'country.name', 'iso3c', warn = TRUE)) %>%
          filter(Country != 'China, mainland')


rm(coast_length)

          #       continent = countrycode(Country, 'country.name', 'continent', warn = TRUE))
          # group_by(Country) %>%
          # summarize(F_tototal = mean(F_tototal)) %>%

          # left_join(., coast_length, by='country_code') %>%
          # filter(coastline_lenght_perarea_km < 0.2) %>%
          # mutate(clpa = coastline_lenght_perarea_km*1000)


############################################################################
##    Below is code for testing regressions
############################################################################
# 
# # fit model
# m <- nls(data=appcon, F_tototal ~ a * exp(b*clpa), start=list(a=1,b=0))
# 
# summary(m)
# cor(appcon$F_tototal,predict(m))
# nlsResiduals(m)
# # diagnostic plto
# plot(nlsResiduals(m))
# 
# # parameter confidence intervals
# confint2(m,level = 0.95, method = "asymptotic")
# # confidence regions
# x<-nlsConfRegions (m, length = 1000, exp = 1.5)
# ## S3 method for class 'nlsConfRegions'
# plot(x, bounds = FALSE, ask = FALSE)
# 
# 
# 
# # bootstrap the predictions
# fx <- function(clpa, a, b){a * exp(b*clpa)}
# boots <- bootCase(m, B=999)
# #v <- fx(clpa=22.5, a=boots[,"a"], b=boots[,"b"])
# 
# q <- t(apply(appcon[,'clpa'], 1, function(x) quantile(fx(x, a=boots[,"a"], b=boots[,"b"]),probs=c(0.025, 0.975))))
# 
# q <- as.data.frame(round(q,5))
# names(q) <- c('p025','p975')
# 
# appcon <- cbind(appcon, predict(m), q)
# 
# 
# 
# #a<-predict(m, se.fit=T, interval='confidence', level=0.95)
# #predict(as.lm(m), interval = "confidence")
# 
# ggplot(appcon, aes(x=clpa, y=F_tototal)) +
#   geom_point(aes(color=continent), size=3, alpha=0.6) +   
#   geom_line(aes(x=appcon$clpa, y=predict(m))) +
#   geom_line(aes(x=appcon$clpa, y=appcon$p025), color='blue') +
#   geom_line(aes(x=appcon$clpa, y=appcon$p975), color='blue') +
#   xlab('Coastline per unit land area') +
#   ylab('Percentage of freshwater fish weight in total fish \napparent consumption from FAO (avg:1995-2014)')+
#   facet_wrap(~continent)
#   # geom_text_repel(aes(label=Country, x= clpa, y= F_tototal),
#   #                 data = subset(appcon, 
#   #                               clpa>0.08 | F_tototal >0.6),
#   #                 size = 4, 
#   #                 colour='gray15',
#   #                 force = 8,
#   #                 segment.size = 0.25, 
#   #                 segment.color='gray55',
#   #                 box.padding = unit(0.2, 'lines'),
#   #                 point.padding = unit(0.2, 'lines')) 
# 
# 
# ### save plot ------------------------------------------------------------------
# ggsave("../Output/Figures/FtoM_withFAO_apparentconsump.png", 
#        dpi=600, width=210, height=180, units='mm', type = "cairo-png")
# dev.off()

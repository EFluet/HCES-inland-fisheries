options("scipen"=100, digits=4)

# calculate the fraction of product weights identified F and M


coast_length <- read.csv("../data/consumption/country_coastline_length.csv", 
                         stringsAsFactors = F)

coast_length <- coast_length %>%
                mutate(country_code = countrycode(country,'country.name','iso3c',warn=F)) #%>%
                # mutate(clpa = as.numeric(coastline_lenght_perarea_km)) %>%
                # select(country_code, clpa)
  




# Read the area table for 
w_area <- read.csv('../data/water_area/GSWD_GlobalEstimatesCalc_CoastCorrected_v2_23Dec2013_bycountry.csv',
                   stringsAsFactors=FALSE)


# Match country name with code, for later joiningO
w_area$country_code <- countrycode(w_area$Country, 
                                   'country.name', 'iso3c', warn = TRUE)

w_area <- w_area %>%
  dplyr::select(Country, GLWD, country_code) %>%
  filter(!is.na(country_code))








# count the number of F/M products per country  to filter countries ------------
df_src_fm_sum <-  consump_df %>%
          group_by(country.x, prod_src_fm) %>%
          summarise(prod_src_fm_sum = sum(Averageediblequantityconsumedgpersonday)) %>%
          ungroup() %>%
          mutate(prod_src_fm=ifelse(prod_src_fm=='F/M','FM',prod_src_fm)) %>% 
          spread(prod_src_fm, prod_src_fm_sum) %>%
          mutate_each(funs(replace(., is.na(.), 0))) %>%
          mutate(tot= rowSums(.[2:4])) %>%
          filter(F != 0 | FM != 0) %>%
          mutate(country_code = countrycode(country.x,'country.name','iso3c')) %>%
          left_join(., coast_length, by='country_code') %>%
          left_join(., w_area, by='country_code') %>%
          #filter(!is.na(GLWD)) %>%
          mutate(clpa = coastline_lenght_perarea_km) %>%
          mutate(continent = countrycode(country.x, 'country.name', 'continent', warn = TRUE)) %>%
          mutate(continent = ifelse(country.x %in% c('Kazakhstan','Georgia', 'Moldova','Azerbaijan'),
                                    'Central Eurasia',continent))


# calculate ratios
df_src_fm_sum_wratio <- df_src_fm_sum %>%                        
                              mutate(FtoFandM_ratio = F / (F+M),
                                     FnM_aspercoftot = (F+M)/(F+FM+M)) %>%
                        filter(!is.na(FtoFandM_ratio)) 
                        #filter(FtoFandM_ratio > 0) %>%
                        #filter(clpa > 0)%>%
                        #filter(clpa < 0.02) %>%
                        #filter(FnM_aspercoftot > 0.1)
#filter(FtoFandM_ratio>0)



### plot freq histogram --------------------------------------------------------
ggplot(df_src_fm_sum_wratio, aes(x=FtoFandM_ratio)) + 
  geom_histogram() +
  scale_y_continuous(breaks=pretty_breaks())

ggsave("../Output/Figures/freqhist_src_fmratio.png",
       dpi=400, width=90, height=70, units='mm', type = "cairo-png")
dev.off()


### barplot of survey source composition ---------------------------------------
ggplot(consump_df_src_fm_sum, aes(x=country.x, y=value, fill=src))+ 
  geom_bar(stat='identity') + 
  coord_flip() + xlab("") 

ggsave("../Output/Figures/barplot_src_fm_compos.png",
       dpi=400, width=120, height=180, units='mm', type = "cairo-png")
dev.off()






### fit regression model -------------------------------------------------------


# fit linear model
fm_lm <- lm(FtoFandM_ratio~clpa,
        data=subset(df_src_fm_sum_wratio[]),
        weights=(df_src_fm_sum_wratio$FnM_aspercoftot^2))

summary(fm_lm)


# plot model diagnostic
# par(mfrow = c(2, 2), oma = c(0, 0, 2, 0)) 
# plot(fm_lm)



# predict values and append to dataframe
mp <- as.data.frame(cbind(clpa = df_src_fm_sum_wratio$clpa, 
                          predict(fm_lm, interval = 'confidence')))




# scatter plot ----------------------------------------------------------------
s <- ggplot(df_src_fm_sum_wratio, aes(x= clpa, y= FtoFandM_ratio)) +
 
  #geom_line(data = mp, aes(x = clpa, y = fit), size = 1, color = 'blue') +
  #geom_ribbon(data = mp, aes(x = clpa, ymin = lwr, ymax = upr), fill='blue', alpha=0.1) +
  stat_smooth(method='lm', weights=df_src_fm_sum_wratio$FnM_aspercoftot)+
  
  geom_point(aes(x= clpa, y= FtoFandM_ratio, size= FnM_aspercoftot), 
             shape=16, alpha=0.2) + 
  scale_size(range = c(0, 12)) +
   
  coord_cartesian(ylim = c(0, 1))+#, xlim = c(0, 0.05)) + 
  scale_x_continuous(breaks = pretty_breaks(5)) +
  scale_y_continuous(breaks = pretty_breaks(5)) +
  
  xlab('Coastline per unit area (km/km^2)') +
  ylab('Proportion of inland fish in consumed weight\n with idenfiable source') +
  
  
  geom_text_repel(aes(label=country.x, x= clpa, y= FtoFandM_ratio),
                  data = subset(df_src_fm_sum_wratio),
                  size = 4, 
                  colour='gray15',
                  force = 8,
                  segment.size = 0.25, 
                  segment.color='gray55',
                  box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines')) +
  facet_wrap(~continent, scales="free_x")

s



ggsave("../Output/Figures/scatter_src_fmratio.png",
       dpi=400, width=210, height=150, units='mm', type = "cairo-png")
dev.off()




# scatter plot ----------------------------------------------------------------
ggplot(df_src_fm_sum_wratio) +
  geom_point(aes(x= continent, y= FtoFandM_ratio, size= FnM_aspercoftot), alpha=0.2)+ 
  scale_size(range = c(-2, 18))

#  geom_boxplot(aes(x= continent, y= FtoFandM_ratio), alpha=0.2)

 +
  
  coord_cartesian(ylim = c(0, 1))+#, xlim = c(0, 0.05)) + 
  scale_x_continuous(breaks = pretty_breaks(5)) +
  scale_y_continuous(breaks = pretty_breaks(5)) +
  
  xlab('Coastline per unit area (km/km^2)') +
  ylab('Proportion of inland fish in consumed weight\n with idenfiable source') +
  
  
  geom_text_repel(aes(label=country.x, x= clpa, y= FtoFandM_ratio),
                  data = subset(df_src_fm_sum_wratio),
                  size = 4, 
                  colour='gray15',
                  force = 8,
                  segment.size = 0.25, 
                  segment.color='gray55',
                  box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines')) +
  facet_wrap(~continent)


s
















### -----------------------------------------------------------------------

fm_lm <-nls(FtoFandM_ratio ~ a*exp(b*clpa), data= df_src_fm_sum_wratio, 
            start = list(a =1, b = 0), weights=FnM_aspercoftot^4)


# predict values and append to dataframe
mp <- as.data.frame(cbind(clpa = df_src_fm_sum_wratio$clpa, 
                          predict(fm_lm, interval = 'confidence')))

#fm_lm <- nlm(clpa ~ explog(FtoFandM_ratio), data = df_src_fm_sum_wratio)
summary(fm_lm)

library(nlstools)
confint2(fm_lm, level = 0.95, method = c("asymptotic"))

library(nls2)  
#summary(as.lm(fm_lm))
predict(as.lm(fm_lm), interval = "confidence") 
# 
# # fit logistic
# fm_lm <- glm(FtoFandM_ratio ~ clpa,
#              data=df_src_fm_sum_wratio,
#              family=binomial(link = "logit"),
#              weights=(round(df_src_fm_sum_wratio$FnM_aspercoftot*100,0)))
# 
# summary(fm_lm)
# 
# 
# 
# # beta reg: data must be between 
# library(betareg)
# betareg(FtoFandM_ratio ~ clpa,
#         data= df_src_fm_sum_wratio,
#         weights= round(df_src_fm_sum_wratio$FnM_aspercoftot,0))




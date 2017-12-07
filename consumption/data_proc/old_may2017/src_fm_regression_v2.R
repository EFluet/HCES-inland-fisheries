options("scipen"=100, digits=4)

# calculate the fraction of product weights identified F and M


### Read coastline length -------------------------------------------------------
coast_length <- read.csv("../data/consumption/country_coastline_length.csv", 
                         stringsAsFactors = F)
coast_length <- coast_length %>%
                mutate(country_code = countrycode(country,'country.name',
                                                  'iso3c',warn=F)) #%>%


# Read the area table for ------------------------------------------
f<-'../data/water_area/GSWD_GlobalEstimatesCalc_CoastCorrected_v2_23Dec2013_bycountry.csv'
w_area <- read.csv(f, stringsAsFactors=FALSE)

# Match country name with code, for later joiningO
w_area$country_code <- countrycode(w_area$Country, 
                                   'country.name', 'iso3c', warn = TRUE)
w_area <- w_area %>%
  dplyr::select(Country, GLWD, country_code) %>%
  filter(!is.na(country_code))





# count the number of F/M products per country  to filter countries ------------
df_src_fm_sum <-  consump_df %>%
  group_by(country.x, prod_src_fm, tot_pop_both_sexes) %>%
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
  filter(!is.na(FtoFandM_ratio)) %>%
  #filter(FtoFandM_ratio > 0) %>%
  #filter(clpa > 0)%>%
  #filter(clpa < 0.02) %>%
  filter(FnM_aspercoftot > 0.5) %>%
  mutate(geo_pos = ifelse(clpa == 0, 'Landlocked', NA),
         geo_pos = ifelse(clpa > 0.01, 'Islands', geo_pos),
         geo_pos = ifelse(is.na(geo_pos), 'Coastal', geo_pos))
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
#df_src_fm_sum_wratio_af <- subset(df_src_fm_sum_wratio, continent=='Americas')

# fit linear model
fm_lm <- lm(FtoFandM_ratio~clpa, 
            data=df_src_fm_sum_wratio,
            weights=(df_src_fm_sum_wratio$FnM_aspercoftot))

summary(fm_lm)
predict.lm(fm_lm)

# plot model diagnostic
# par(mfrow = c(2, 2), oma = c(0, 0, 2, 0)) 
# plot(fm_lm)

# predict values and append to dataframe
mp <- as.data.frame(cbind(clpa = df_src_fm_sum_wratio$clpa, 
                          predict(fm_lm, interval = 'prediction')))




# scatter plot ----------------------------------------------------------------
s <- ggplot() +
  
  # geom_line(data = mp, aes(x = clpa, y = fit), size = 1, color = 'blue') +
  # geom_ribbon(data = mp, aes(x = clpa, ymin = lwr, ymax = upr), fill='blue', alpha=0.1) +
  #stat_smooth(method='lm', weights=df_src_fm_sum_wratio$FnM_aspercoftot)+
  #FtoFandM_ratio
  geom_point(data = df_src_fm_sum_wratio, aes(x= clpa, y= FtoFandM_ratio, size= FnM_aspercoftot, colour=continent), 
             shape=16, alpha=0.7) + 
  scale_size(range = c(0, 12)) +
  
  #coord_cartesian(ylim = c(0, 1))+#, xlim = c(0, 0.05)) + 
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
                  point.padding = unit(0.2, 'lines')) 
#facet_wrap(~continent, scales="free_x")


s



ggsave("../Output/Figures/scatter_src_fmratio.png",
       dpi=400, width=210, height=150, units='mm', type = "cairo-png")
dev.off()




summary(lm(FtoFandM_ratio~tot_pop_both_sexes+continent, data =df_src_fm_sum_wratio))





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



ggplot() + 
  geom_point(data=df_src_fm_sum_wratio, aes(x= geo_pos, y= FtoFandM_ratio, colour=continent, size= FnM_aspercoftot))


ggplot(data=df_src_fm_sum_wratio, aes(x=FtoFandM_ratio)) + 
  geom_histogram(aes(fill=FnM_aspercoftot)) +
  facet_wrap(~geo_pos, ncol=1)



####  Kernel density

df_src_fm_sum_wratio_sub <- subset(df_src_fm_sum_wratio, geo_pos=='Landlocked')
# landlocked- estiate density kernel -------------------------------------------
l <- density(df_src_fm_sum_wratio_sub$FtoFandM_ratio, bw = "nrd0", adjust = 1.5, 
             weights=df_src_fm_sum_wratio_sub$FnM_aspercoftot,
             kernel = c("gaussian"), n=10000, from=0, to=1)



df_src_fm_sum_wratio_sub <- subset(df_src_fm_sum_wratio, geo_pos=='Coastal')
# landlocked- estiate density kernel -------------------------------------------
c <- density(df_src_fm_sum_wratio_sub$FtoFandM_ratio, bw = "nrd0", adjust = 1.5, 
             weights=df_src_fm_sum_wratio_sub$FnM_aspercoftot,
             kernel = c("gaussian"), n=10000, from=0, to=1)


df_src_fm_sum_wratio_sub <- subset(df_src_fm_sum_wratio, geo_pos=='Islands')
                               
# landlocked- estiate density kernel -------------------------------------------
i <- density(df_src_fm_sum_wratio_sub$FtoFandM_ratio, bw = "nrd0", adjust = 1.5, 
             weights=df_src_fm_sum_wratio_sub$FnM_aspercoftot,
             kernel = c("gaussian"), n=10000, from=0, to=1)

plot(i$x, i$y)


ggplot()+
  geom_density(data=i, aes(x,y))

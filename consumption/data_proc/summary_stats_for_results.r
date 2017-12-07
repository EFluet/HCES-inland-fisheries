source('./consumption/import_libraries.r')


# get survey catch & join to fao catch data  ----------------------------
filename<-'../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv'

surv_data <- read.csv(filename, stringsAsFactors=F)


surv_data <-  surv_data %>%
              mutate(hces_surv_asprcfao = tr_corr_consump/sum_catch *100) %>%
              filter(tr_corr_consump > 0)


# prep data for statistical tests  ----------------------------------------------
m <- surv_data[,c('tr_corr_consump','sum_catch')]
m$diff <- m$tr_corr_consump - m$sum_catch
m$diff_perc <- m$diff - m$sum_catch



### Test for normality -----------------------------------------------------
shapiro.test(m$diff)
shapiro.test(boxcox(m$diff))
shapiro.test(m$diff_perc)
# H0 = pop is normally distributed. If p-value < alpha, reject H0, data  not normally distributed
# p>a, cannot reject H0


# Paired T-test on difference -----------------------------------------------
t.test(m$tr_corr_consump, m$sum_catch, 
       paired=TRUE, alternative = "greater", conf.level = 0.95)
#alternative = c("two.sided", "less", "greater"))

# Paired Wilcoxon  (non-parametric paired t-test ) -------------
# between survey-estimated catch and reported catch
wilcox.test(surv_data$tr_corr_consump, surv_data$sum_catch, 
            paired = TRUE, alternative = "two.sided", conf.level = 0.95)


### Kendall Tau-b test on difference -------------------------------------------
#cor(m, method="kendall", use="pairwise") 

# kendall correlation and 
cor.test(m$tr_corr_consump, m$sum_catch, method="kendall") 





### Calculate summary statistics for results section ------------------------
# sum 
m <- round(sum(surv_data$tr_corr_consump),2)
l <- round(sum(surv_data$p025),2)
u <- round(sum(surv_data$p975),2)
f <- round(sum(surv_data$sum_catch),2)

print(paste("HCES total catch for" , length(surv_data$tr_corr_consump) , 
            " countries is", m , "  range: ", l, " to ", u))
print(paste("while fao is ", f))
print(paste('survey is thus ', (m-f)/f*100, "%  larger than FAO, with range of", 
            (l-f)/f*100, "  -  ", (u-f)/f*100))


print(paste("the difference is ", m-f))



# negative count
cn <- length(which(surv_data$tr_corr_consump < surv_data$sum_catch))
print(paste("count of negative countries ", cn))

# Sum differencec for countries with Surv<FAO
sum(surv_data$mean_dif[which(surv_data$tr_corr_consump < surv_data$sum_catch)])
sum(surv_data$p025_dif[which(surv_data$tr_corr_consump < surv_data$sum_catch)])
sum(surv_data$p975_dif[which(surv_data$tr_corr_consump < surv_data$sum_catch)])


# sum of difference for countries with Surv>FAO 
sum(surv_data$mean_dif[which(surv_data$tr_corr_consump > surv_data$sum_catch)])
sum(surv_data$p025_dif[which(surv_data$tr_corr_consump > surv_data$sum_catch)])
sum(surv_data$p975_dif[which(surv_data$tr_corr_consump > surv_data$sum_catch)])


# subset of large surplus countries
countries<- c('Zambia (2002-03)','Bangladesh (2010)', 'Congo Dem Rep (2004-05)')

sum(surv_data$mean_dif[which(surv_data$tr_corr_consump > surv_data$sum_catch &
                               surv_data$country_label %in% countries)])
sum(surv_data$p025_dif[which(surv_data$tr_corr_consump > surv_data$sum_catch &
                               surv_data$country_label %in% countries)])
sum(surv_data$p975_dif[which(surv_data$tr_corr_consump > surv_data$sum_catch &
                               surv_data$country_label %in% countries)])





# U.I. range as % of HCES estimate ----------------------------------
surv_data <- surv_data %>%
                  mutate(rng_ui = (p975 - p025)/tr_corr_consump)

mean(surv_data$rng_ui, na.rm = T)
sd(surv_data$rng_ui , na.rm = T)
min(surv_data$rng_ui, na.rm = T)
max(surv_data$rng_ui, na.rm = T)




### Percentage of global catch in survey countries -----------------------------


filename<-'../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv'
surv_consump_df <- read.csv(filename, stringsAsFactors=F)

# read catch data from FAO
source('./fishstatj_data/read_nat_catch_data.R', print.eval = TRUE)

# 
surv_data<- surv_consump_df %>%
            filter(!is.na(tr_corr_consump)) %>%
            select(country_code, sum_catch, tr_corr_consump)

# subset to 2014
ca_sum_bycountry2 <- ca_sum_bycountry %>%
                  filter(year==2008, source=='Inland') %>%
                  mutate(sum_catch2014 = sum_catch/10^6) %>%
                  select(-sum_catch) %>%
                  left_join(., surv_data, by=c('country_code'='country_code')) %>%
                  # replace value with sum catch 2014
                  mutate(sum_catch = ifelse(!is.na(sum_catch), sum_catch2014, 0))



# percentage of catch in 2014 from surveyed countries ------------------------
glob_ca2014 <- sum(ca_sum_bycountry2$sum_catch2014)
surv_ca2014 <-sum(ca_sum_bycountry2$sum_catch)

print(paste("percentage of catch in 2014 from surveyed countries", surv_ca2014/glob_ca2014))



# percentage of catch in 2014 from surveyed countries ----------------------------
glob_ca2014 <- sum(ca_sum_bycountry2$sum_catch2014)
surv_ca2014 <- sum(subset(ca_sum_bycountry2, tr_corr_consump>0)[,"sum_catch2014"])

print(paste("percentage of catch in 2014 from surveyed countries", surv_ca2014/glob_ca2014))



# compare growth of survey vs all countries -----------------------------

#~~ calc growth of all countries ----------------------
ca_sum_bycountry_growth <- ca_sum_bycountry %>%
  filter(source == 'Inland') %>%
  filter(year == 2004 | year == 2014) %>%
  group_by(year) %>%
  summarize(sum_catch = sum(sum_catch))


# global rate of growth per year :  322552 tonnes / year
#(11898482-8672960)/10/8672960 = 0.0371906 % / year

surv_data_pos <- surv_data %>% filter(tr_corr_consump > 0)


#~~ calc growth of survey countries ----------------------
ca_sum_bycountry_growth_surv <- ca_sum_bycountry %>%
  filter(source == 'Inland') %>%
  filter(country_code %in% surv_data_pos$country_code) %>%
  filter(year == 2004 | year == 2014) %>%
  group_by(year) %>%
  summarize(sum_catch = sum(sum_catch))

# 187331 tonnes / year 
#(6474355-4579482) / 10 / 4579482 = 0.0413775 % year



# calc nb countries within a certain difference --------------------------------

sum(abs(surv_data$mean_dif*10^6) < 125000)

sum(surv_data$hces_surv_asprcfao >200)
sum(surv_data$hces_surv_asprcfao >400)
sum(surv_data$hces_surv_asprcfao >50 & surv_data$hces_surv_asprcfao <150)


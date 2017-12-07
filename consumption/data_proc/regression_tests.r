source('./consumption/import_libraries.r')


### read the survey catch & join to fao catch data  ----------------------------
filename<-'../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc.csv'
surv_consump_df <- read.csv(filename, stringsAsFactors=F)


surv_means <- surv_consump_df %>%
  #filter(datatype == 'tr_corr_consump') %>%
  #mutate(country_code = countrycode(country.x, 'country.name', 'iso3n', warn=TRUE)) %>%
  select(country_code, country.x, datatype, mean, year_start, year_end) %>%
  filter(datatype %in% c('sum_catch', 'tr_corr_consump')) %>%
  #unite(comb, p025, mean, p975, datatype) %>%
  spread(datatype, mean) 

surv_consump_df<- surv_consump_df %>% 
  dplyr::select(country_code, datatype, p025, p975) %>%
  filter(datatype %in% c('tr_corr_consump')) %>%
  left_join(., surv_means, by='country_code') %>%
  mutate( mean_dif = tr_corr_consump-sum_catch,
          p025_dif = p025-sum_catch,
          p975_dif = p975-sum_catch,
          mean_dif_perc = mean_dif/sum_catch *100,
          p025_dif_perc = p025_dif/sum_catch *100,
          p975_dif_perc = p975_dif/sum_catch *100) %>%
  mutate(year_end= ifelse(year_end==0,NA,year_end),
         surv_year= ifelse(is.na(year_end), year_start, paste(year_start, year_end, sep='-')))


rm(surv_means)


# write out modified survey df
filename<-'../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_diff.csv'
write.csv(surv_consump_df, filename)




# read the predictors and join them to the catch difference df 
source('./consumption/data_proc/read_predictors_for_regressions.r')


#### filter 
surv_consump_df_forreg <- surv_consump_df_forreg %>%
                          filter(tr_corr_consump > 0) %>%#, sum_catch.x > 0.002) %>%
                          mutate(continent = countrycode(country_code, 'iso3c', 'continent',warn=FALSE))


####################
ggplot() + geom_histogram(data=surv_consump_df_forreg,
                          aes(x=mean_dif_perc))

ggplot() +
  geom_point(data= surv_consump_df_forreg,
             aes(x=GLWD,#pop_dens_per_glwd,#SP.RUR.TOTL / (GLWD*1000),
                 y=mean_dif_perc, color=continent), size=3)+
  geom_hline(yintercept=0)



ggplot() +
  geom_point(data= surv_consump_df_forreg,
             aes(x=cut(sum_catch.x, seq(0, 1.2, 0.1)),
                 y=mean_dif_perc, color=continent), size=3)

ggplot() +
  geom_boxplot(data= surv_consump_df_forreg,
             aes(x=continent,
                 y=mean_dif_perc, color=continent), size=1)

ggplot() +
  geom_point(data= surv_consump_df_forreg,
             aes(x=cut(mean_dif_perc, c(-10000,0,200000)),
                 y=mean_dif_perc, color=continent), size=3) +
  facet_wrap(~continent)



### Regression models -------------------------------------------------------

cols_for_reg <- c("AG.LND.TOTL.K2", "EN.POP.DNST", "NE.CON.PRVT.KD", 
                  "SL.EMP.TOTL.SP.NE.ZS", "SP.RUR.TOTL.ZS", "aquaculture_flag_perc",
                  "catch_flag_perc", "export_flag_perc", "import_flag_perc",
                  "GLWD_dens", "pop_dens","pop_dens_per_glwd")


reg_res <- data.frame(matrix(NA, nrow = 0, ncol = 0))

# loop through columns, fit spearman correlation and 
for (i in cols_for_reg){
  
  temp_for_reg <- surv_consump_df_forreg[,c('mean_dif_perc', i)]
  temp_for_reg <- temp_for_reg[complete.cases(temp_for_reg),]
  
  # rho ranges from −1 to +1
  # H0: no [monotonic] association between the two variables
  r <- cor.test(unlist(c(temp_for_reg['mean_dif_perc'])),
                unlist(c(temp_for_reg[i])),
                method = "spearman")
  
  r['var'] <- i
  r['n'] <- length(!is.na(temp_for_reg[i]))
  r <-as.data.frame(t(unlist(r)))
  reg_res<- plyr::rbind.fill(reg_res, r)
}

reg_res<- reg_res %>%
  #mutate(p.value2 = as.numeric(p.value)) %>%
  select_("p.value","var","estimate.rho","n") %>%
  arrange(p.value)



write.csv(reg_res, '../output/consumption/spearman_regression_catchdiff.csv')







# Classification Tree with rpart -----------------------------------------------
library(rpart)

# grow tree 
fit <- rpart(mean_dif_perc ~ SP.RUR.TOTL.ZS + NE.CON.PRVT.KD + SL.EMP.TOTL.SP.NE.ZS,
             method="class", data=kyphosis)

# grow tree 
fit <- rpart(mean_dif_perc ~ SP.RUR.TOTL.ZS + NE.CON.PRVT.KD + SL.EMP.TOTL.SP.NE.ZS,
             method="class", data=surv_consump_df_forreg)


printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "c:/tree.ps", 
     title = "Classification Tree for Kyphosis")







####
library(GGally)
library(ggplot2)
surv_lbl <- surv_data %>% 
            dplyr::select(c(country_code), prc_categ)

cols_for_plotmatrix <- c("EN.POP.DNST", "NE.CON.PRVT.KD", 
                         "SL.EMP.TOTL.SP.NE.ZS", "SP.RUR.TOTL.ZS",
                         "GLWD_dens", "pop_dens","pop_dens_per_glwd")



# make matrix of scatterplots between all covariates
ggpairs(surv_consump_df_forreg[c(cols_for_plotmatrix, 'mean_dif_perc')])






# test GAM model ----------------------------------------------------------
require("mgcv")
m <- gam(mean_dif ~ s(GLWD_dens), data = surv_consump_df_forreg)
summary(m)
plot(m,pages=1,residuals=TRUE) ## show partial residuals
plot(m,pages=1,seWithMean=TRUE) ## `with intercept' CIs
gam.check(m)
gam.check(m, type='deviance')

m <- gam(mean_dif ~ s(GLWD_dens)+s(aquaculture_flag_perc)+s(catch_flag_perc)+s(export_flag_perc)+s(import_flag_perc), data = surv_consump_df_forreg)

m <- gam(mean_dif ~ +s(aquaculture_flag_perc)+s(catch_flag_perc), data = surv_consump_df_forreg)
gam.check(m)

m <- gam(mean_dif ~ s(GLWD_dens), data = surv_consump_df_forreg)


### GLM ------------------------------------------------------------------------

fit <- lm(mean_dif ~ GLWD_dens + aquaculture_flag_perc, data=surv_consump_df_forreg)


fit <- glm(mean_dif ~ GLWD_dens + aquaculture_flag_perc + catch_flag_perc + export_flag_perc + import_flag_perc, data = surv_consump_df_forreg, family=Gamma)

summary(fit) # display results
confint(fit) # 95% CI for the coefficients
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients
predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals




### LM -------------------------------------------------------------------------

hist(surv_consump_df_forreg$mean_dif_perc)
fit <- lm(mean_dif_perc ~ SP.RUR.TOTL.ZS, data=surv_consump_df_forreg)

summary(fit)
par(mar = rep(2, 4))
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(fit)

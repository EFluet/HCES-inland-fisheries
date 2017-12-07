
### Extract years after surveys to investigate if large changes have occured

# read survey data, incl. catch difference with FAO stats 
f <- '../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv'
sum_surv <- read.csv(f, stringsAsFactors=FALSE)
sum_surv <- sum_surv %>%
  mutate(hces_surv_asprcfao = tr_corr_consump/sum_catch *100) %>%
  mutate(sum_catch_hcesyr = sum_catch) %>%
  select(country_code, country_label, year_start, 
         hces_surv_asprcfao, sum_catch_hcesyr, tr_corr_consump) %>%
  distinct


### get FishStatJ catch data  -------------------------------------------------
source("./fishstatj_data/read_nat_catch_data.R")
# RENAME sum_catch  to  sum_catch_fao
rm(f, ca)


ca_sum_bycountry <- ca_sum_bycountry %>%
                    ungroup() %>%
                    filter(source == 'Inland',
                           year == max(year),
                           country_code %in% sum_surv$country_code)

sum_surv_2 <- sum_surv %>%
            left_join(., ca_sum_bycountry, by = c('country_code'='country_code'))


# protip: sum_catch is 2014 (i.e. pres) catch
sum_surv_2 <- sum_surv_2 %>%
              mutate(pres_minus_reponhcesyr = ((sum_catch/10^6) -sum_catch_hcesyr)/sum_catch_hcesyr) %>%
              mutate(reponhcesyr_minus_hces = (sum_catch_hcesyr - tr_corr_consump)/sum_catch_hcesyr) %>%
              mutate(pres_aspercof_reponhcesyr = (sum_catch/10^6) /sum_catch_hcesyr *100) %>%
              mutate(survey_aspercof_reponhcesyr = tr_corr_consump/sum_catch_hcesyr *100) %>%
              filter(tr_corr_consump > 0, 
                     survey_aspercof_reponhcesyr < 2000)

#    ( R_p - R_h ) / R_h
#    ( R_p - R_h ) / R_h




#~~ create formula ------------------------------------------
# f <- pres_minus_reponhcesyr ~ reponhcesyr_minus_hces
f <- pres_aspercof_reponhcesyr ~ survey_aspercof_reponhcesyr
  
# #~~ subset df  -----------------------------------------------------------------
# cols<-c("SP.RUR.TOTL.ZS", "NY.GDP.PCAP.KD", "AG.LND.TOTL.K2", 'MAMax_dens')
# surv_data_sel <- surv_data[complete.cases(surv_data[,cols]),]

#~~  get VIH --------------
library('car')
g <- glm(f, data=sum_surv_2, family=gaussian(log), 
         control = list(maxit = 500))#, start=c(1,1))
summary(g)
vif(g)

temp_rsqGLM <- RsqGLM(model = g)


corr <- cor.test(x= sum_surv_2$pres_aspercof_reponhcesyr, 
                 y= sum_surv_2$survey_aspercof_reponhcesyr, 
                 method = 'pearson')





ggplot(sum_surv_2, aes(x=pres_aspercof_reponhcesyr, y=survey_aspercof_reponhcesyr))+  
  # reponhcesyr_minus_hces
  # hces_surv_asprcfao
  
  geom_vline(xintercept=100, color='grey90', size=1.4) +
  geom_hline(yintercept=100, color='grey90', size=1.4) +
  
  geom_point(aes(size=sum_catch, color=country_code)) +
  ylim(0,1300) +
  #coord_fixed(ratio = 1) +
  scale_x_log10() +
  scale_y_log10() +

  geom_text_repel(data=subset(sum_surv_2, pres_aspercof_reponhcesyr>150 | survey_aspercof_reponhcesyr>300),
                  aes(label=country_code, color=factor(country_code)),
                  size = 4,
                  #colour='gray15',
                  segment.size = 0.25,
                  segment.color='gray55',
                  force = 8,
                  box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines')) +

  guides(color=FALSE, size=FALSE) +
  xlab('%diff of present reported catch \n from reporting on survey year')+
  ylab('%diff of survey estimated catch \n from reporting on survey year')
  


### Save figure to file --------------------------------------------------------
ggsave('../output/figures/perc_diff_hces_and_reportedsincesurveyyear.png',
       width=120, height=210, dpi=400, units="mm", type = "cairo-png")
dev.off()



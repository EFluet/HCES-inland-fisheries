
# get data -----------------
#~~ get flags ca, aq, tr ------------------------------------------------
all_flag_tally_agg <- read.csv('../output/consumption/all_flag_tally_agg.csv',
                               stringsAsFactors = F)

all_flag_tally_agg <- all_flag_tally_agg %>%
  mutate(flag_perc= as.factor(ifelse(flag_perc>0.2, 1, 0))) %>%
  dplyr::select(country_code, TradeflowTradeflow, flag_perc) %>%
  spread(TradeflowTradeflow, flag_perc) 

colnames(all_flag_tally_agg)[2:5] <- paste(colnames(all_flag_tally_agg)[2:5],"flag", sep="_")



#~~ get standardized FAO stats ------------------------------------------
surv_fao_stdzd <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm.csv',
                           stringsAsFactors = FALSE)

surv_fao_stdzd <- surv_fao_stdzd %>%
  filter(!datatype %in% c('uncert_fwae_ur', 'uncert_prodfm_ur',
                          'uncert_fwae_matched_mc', 'uncert_fwae_assumed_mc')) %>%
  dplyr::select(-one_of("country.x","grp","X", "confidence_lvl","p025","p975")) %>%
  spread(datatype, mean) %>%
  dplyr::select(-one_of("cons_pretrcorr","tr_corr_consump")) %>%
  mutate_at(.cols = vars(export, import, sum_aquacul, sum_catch),
            .funs = funs(./sum_catch))

colnames(surv_fao_stdzd)[5:8] <- paste(colnames(surv_fao_stdzd)[5:8],"fao_stdzd", sep="_")



#~~ calculate survey characteristics ----------------------------------
f <- '../output/consumption/hh_survey_consump_sep_prods_refuse_fwaes_manual_prod_match.csv'
consump_prod_fwaes <- read.csv(f, stringsAsFactors = F)

surv_char_assumps <- consump_prod_fwaes %>%
  
  mutate(product = ifelse(product %in% c('fresh','fresco','frais'), 'Fresh', product),
         product = ifelse(product %in% c('smoked','humado','fumé',
                                         'dried', 'seco','séché'), 'Dried\nsmoked', product),
         product = ifelse(product %in% c('frozen'), 'Frozen', product),
         largest_f_fwae_method = product) %>%
  
  filter(prod_src_fm %in% c('F', 'F/M')) %>%
  dplyr::select(country_code, prod_src_fm, prod_name, product, consump_million.tons.yr) %>%
  mutate(product = ifelse(is.na(product), 'Assumed\nfresh', product)) %>%
  
  # sum total weight per country
  group_by(country_code) %>%
  mutate(sum_weight = sum(consump_million.tons.yr)) %>%
  mutate(perc_weight_assumed_fresh = sum(consump_million.tons.yr[product=='Assumed\nfresh'])/sum(consump_million.tons.yr)) %>%
  mutate(perc_weight_fm = sum(consump_million.tons.yr[prod_src_fm=='F/M'])/sum(consump_million.tons.yr)) %>%
  ungroup %>%
  
  dplyr::select(country_code, perc_weight_assumed_fresh, perc_weight_fm) %>%
  unique() %>%
  filter(!is.na(perc_weight_assumed_fresh))


### other predictors
#source('./consumption/data_proc/read_predictors_for_regressions.r')


#~~ get world bank indices -------------------------------------------------------
wdi <- read.csv('../data/socio_demo/wdi/WDI_Data.csv', stringsAsFactors=F)

colstokeep <- c('GDP per capita (constant 2010 US$)',
                'Land area (sq. km)',
                'Population density (people per sq. km of land area)',
                'Rural population',
                'Rural population (% of total population)',
                "Household final consumption expenditure (constant 2010 US$)" ,
                "Employment to population ratio, 15+, total (%) (national estimate)"  )


wdi_sel <- wdi %>%
  filter(Indicator.Name %in% colstokeep) %>%
  dplyr::select(Country.Name, Country.Code, Indicator.Code, X2012) %>%
  spread(Indicator.Code, X2012) %>%
  mutate(country_code = countrycode(Country.Name, 'country.name', 
                                    'iso3c', warn = F))


#~~ get Inland water surface area -------------------------------------------------------------------
f<-'../data/water_area/GSWD_GlobalEstimatesCalc_CoastCorrected_v2_23Dec2013_bycountry.csv'
w_area <- read.csv(f, stringsAsFactors=FALSE)

w_area <- w_area %>%
  mutate(country_code = countrycode(Country, 'country.name', 'iso3c', warn = F)) %>%
  dplyr::select(country_code, GLWD, MAMax) %>%
  filter(!is.na(country_code))



# prep data --------------------------------------------------------------------

#~~ join all together ----------------------------------------------------------
f<-'../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv'
surv_data <- read.csv(f, stringsAsFactors = FALSE)

surv_data <-  surv_data %>%
  left_join(., surv_char_assumps, by="country_code") %>%
  left_join(., all_flag_tally_agg, by="country_code") %>%
  left_join(., surv_fao_stdzd, by="country_code") %>%
  left_join(., wdi_sel, by="country_code") %>%
  left_join(., w_area, by="country_code") %>%
  
  filter(tr_corr_consump > 0) %>%
  filter(mean_dif_perc < 2000) %>%
  
  mutate(hces_surv_asprcfao = tr_corr_consump/sum_catch *100) %>%
  mutate(MAMax_dens = MAMax / AG.LND.TOTL.K2)


#~~ Cut hces_surv_asprcfao_cut into bins  --------------------------------------------------
my_breaks = c(0, 50, 150, 200, 500, 2000)
surv_data$hces_surv_asprcfao_cut <- cut(surv_data$hces_surv_asprcfao, breaks = my_breaks,
                                        dig.lab=10)

# replace the categories stings to make them nicer in the legend
surv_data$hces_surv_asprcfao_cut <- gsub("\\(|\\]", "", surv_data$hces_surv_asprcfao_cut)
surv_data$hces_surv_asprcfao_cut <- gsub("\\,", " to ", surv_data$hces_surv_asprcfao_cut)
surv_data <- surv_data %>% mutate(hces_surv_asprcfao_cut=ifelse(hces_surv_asprcfao_cut=="500 to 2000",
                                                                "over 500",hces_surv_asprcfao_cut))

lengend_order <- rev(c("200 to 500", "150 to 200", "over 500", "50 to 150", "0 to 50"))

surv_data$hces_surv_asprcfao_cut <- factor(surv_data$hces_surv_asprcfao_cut, levels = lengend_order)
#levels(surv_data$hces_surv_asprcfao_cut)





# GLM Part 1: test effet of surveys  ---------------------------------------------
# the surveys do not bias in the percentage difference
formula<- hces_surv_asprcfao ~ perc_weight_assumed_fresh + perc_weight_fm

fit <- glm(formula, data=surv_data, family=gaussian(log), 
           start=c(5,5,5), control = list(maxit = 50))

summary(fit) # show results

# https://modtools.wordpress.com/2013/08/14/dsquared/
# https://rdrr.io/rforge/modEvA/man/Dsquared.html
library('modEvA')

# D^2 (same as CoxSnell?)
Dsquared(model = fit, 
         obs = surv_data$hces_surv_asprcfao,
         family = "gaussian")


RsqGLM(model = fit)





# GLM Part 2 - all predictors ---------------------------------------------------

#~~ create formula ------------------------------------------
f <- hces_surv_asprcfao ~  
  export_fao_stdzd * Export_flag +
  sum_aquacul_fao_stdzd * Aquaculture_flag  +
  import_fao_stdzd * Import_flag  +
  Catch_flag +
  AG.LND.TOTL.K2  + NY.GDP.PCAP.KD +
  SP.RUR.TOTL.ZS +
  MAMax_dens
#+ sum_catch # + SP.RUR.TOTL

#~~ subset df  -----------------------------------------------------------------
cols<-c("EN.POP.DNST", "sum_catch",
        "SP.RUR.TOTL.ZS", "NY.GDP.PCAP.KD", "AG.LND.TOTL.K2", "sum_catch",
        'Aquaculture_flag', 'Catch_flag', 'Export_flag', 
        'Import_flag', 'export_fao_stdzd','import_fao_stdzd',
        'sum_aquacul_fao_stdzd', 'MAMax_dens')#, 'hces_surv_asprcfao')#SP.RUR.TOTL

surv_data_sel <- surv_data[complete.cases(surv_data[,cols]),]


#~~  get VIH --------------
library('car')
g <- glm(f, data=surv_data_sel, family=gaussian(log), 
         control = list(maxit = 50))

vif(g)


#~~ glmulti --------------------------------------------------------
library('glmulti')

obj <- glmulti(f, data=surv_data_sel, family = gaussian(link='log'), 
               maxit = 500,
               confsetsize = 23,        # Keep N best models
               plotty = T,              # plot the progress of the IC profile when running
               level = 2,               # interaction considered, lvl=1 is main effect
               method = "g",            # 'h' Exhaustive approach;  'g' is genetic algorithm
               crit = "aicc",           # AIC as criteria
               includeobjects=1)        # supposedly returns  model object...  

# get formula of 1st model kept
obj@formulas
obj@formulas[[1]]

# summary of best model
summary(obj@objects[[2]])

obj@K

plot(obj) # plot the IC profile 

tmp <- weightable(obj)
tmp <- tmp[tmp$aicc <= min(tmp$aicc) + 2,]
tmp

# plot variable importance
#importances <- summary(obj)$termweights

pdf('../output/figures/glm_importance.pdf')
plot(obj, type="s")

dev.off()


round(coef(obj), 4)


#~~ average pseudo R^2 of all models ---------------------------------------------
df_RsqGLM <-  data.frame(CoxSnell=double(),
                         Nagelkerke=double(),
                         Factors=double(),
                         McFadden=double(),
                         Tjur=double(),
                         sqPearson=double())


# loop through models kept
for (i in seq(1:23)) {
  
  # run single glm
  temp_glm <- glm(formula = obj@formulas[[i]],
                  data=surv_data_sel,
                  family=gaussian(log))
  
  # get R^2
  temp_rsqGLM <- RsqGLM(model = temp_glm)
  # add R^2 to df
  df_RsqGLM <- rbind(df_RsqGLM, temp_rsqGLM)
}
# average model
df_RsqGLM <- summarise_each(df_RsqGLM, funs(mean))





# part 3: socio-demo --------------------------------

#~~ create formula ------------------------------------------
f <- hces_surv_asprcfao ~  
                AG.LND.TOTL.K2  + NY.GDP.PCAP.KD +
                SP.RUR.TOTL.ZS +
                MAMax_dens

#~~ subset df  -----------------------------------------------------------------
cols<-c("SP.RUR.TOTL.ZS", "NY.GDP.PCAP.KD", "AG.LND.TOTL.K2", 'MAMax_dens')

surv_data_sel <- surv_data[complete.cases(surv_data[,cols]),]


#~~  get VIH --------------
library('car')
g <- glm(f, data=surv_data_sel, family=gaussian(log), 
         control = list(maxit = 50))
vif(g)  # wow- much lower than with all predictors


#~~ glmulti --------------------------------------------------------
library('glmulti')

obj <- glmulti(f, data=surv_data_sel, family = gaussian(link='log'), 
               maxit = 500,
               confsetsize = 23,        # Keep N best models
               plotty = T,              # plot the progress of the IC profile when running
               level = 2,               # interaction considered, lvl=1 is main effect
               method = "g",            # 'h' Exhaustive approach;  'g' is genetic algorithm
               crit = "aicc",           # AIC as criteria
               includeobjects=1)        # supposedly returns  model object...  

# get formula of 1st model kept
obj@formulas[[1]]
# summary of best model
summary(obj@objects[[2]])
obj@K

plot(obj) # plot the IC profile 

tmp <- weightable(obj)
tmp <- tmp[tmp$aicc <= min(tmp$aicc) + 2,]
tmp

# plot variable importance
#importances <- summary(obj)$termweights
plot(obj, type="s")

round(coef(obj), 4)


#~~ average pseudo R^2 of all models ---------------------------------------------
df_RsqGLM <-  data.frame(CoxSnell=double(),
                         Nagelkerke=double(),
                         Factors=double(),
                         McFadden=double(),
                         Tjur=double(),
                         sqPearson=double())


# loop through models kept
for (i in seq(1:23)) {
  
  # run single glm
  temp_glm <- glm(formula = obj@formulas[[i]],
                  data=surv_data_sel,
                  family=gaussian(log))
  
  # get R^2
  temp_rsqGLM <- RsqGLM(model = temp_glm)
  # add R^2 to df
  df_RsqGLM <- rbind(df_RsqGLM, temp_rsqGLM)
}
# average model
df_RsqGLM <- summarise_each(df_RsqGLM, funs(mean))







# Part 4 - only flags & FAO stats ----------------------------------------------

#~~ create formula ------------------------------------------
f <- hces_surv_asprcfao ~  
                export_fao_stdzd * Export_flag +
                sum_aquacul_fao_stdzd * Aquaculture_flag  +
                import_fao_stdzd * Import_flag  +
                Catch_flag

#~~ subset df  -----------------------------------------------------------------
cols<-c('Aquaculture_flag', 'Catch_flag', 'Export_flag', 
        'Import_flag', 'export_fao_stdzd','import_fao_stdzd',
        'sum_aquacul_fao_stdzd')

surv_data_sel <- surv_data[complete.cases(surv_data[,cols]),]


#~~  get VIH --------------
library('car')
g <- glm(f, data=surv_data_sel, family=gaussian(log), 
         control = list(maxit = 50))

vif(g)


#~~ glmulti --------------------------------------------------------
library('glmulti')

obj <- glmulti(f, data=surv_data_sel, family = gaussian(link='log'), 
               maxit = 500,
               confsetsize = 23,        # Keep N best models
               plotty = T,              # plot the progress of the IC profile when running
               level = 2,               # interaction considered, lvl=1 is main effect
               method = "g",            # 'h' Exhaustive approach;  'g' is genetic algorithm
               crit = "aicc",           # AIC as criteria
               includeobjects=1)        # supposedly returns  model object...  

# get formula of 1st model kept
obj@formulas
obj@formulas[[1]]

# summary of best model
summary(obj@objects[[2]])

obj@K

plot(obj) # plot the IC profile 

tmp <- weightable(obj)
tmp <- tmp[tmp$aicc <= min(tmp$aicc) + 2,]
tmp

# plot variable importance
#importances <- summary(obj)$termweights
plot(obj, type="s")

round(coef(obj), 4)


#~~ average pseudo R^2 of all models ---------------------------------------------
df_RsqGLM <-  data.frame(CoxSnell=double(),
                         Nagelkerke=double(),
                         Factors=double(),
                         McFadden=double(),
                         Tjur=double(),
                         sqPearson=double())


# loop through models kept
for (i in seq(1:23)) {
  
  # run single glm
  temp_glm <- glm(formula = obj@formulas[[i]],
                  data=surv_data_sel,
                  family=gaussian(log))
  
  # get R^2
  temp_rsqGLM <- RsqGLM(model = temp_glm)
  # add R^2 to df
  df_RsqGLM <- rbind(df_RsqGLM, temp_rsqGLM)
}
# average model
df_RsqGLM <- summarise_each(df_RsqGLM, funs(mean))





# model-averaged estimates 
# of the diff parameters, with unconditional variance, importance, and confidence interval -
v<- coef(obj, select="all", varweighting="Buckland", icmethod="Lukacs", alphaIC=0.05)

anova(fit, fit, test="Chisq")


############################-------------



# library(visreg)
# visreg(fit)
## S3 method for class 'glmulti'
# model-averaged estimates of the different parameters in the models,
#as well as their unconditional variance, importance, and confidence interval
coef(obj, select="all", varweighting="Buckland", icmethod="Lukacs", alphaIC=0.05)


# ~~~ glmulti predict percentage for the surveyed countries ------------------------------
psmulti<- predict(obj, select="all", newdata=NA, se.fit=FALSE, type = c("response"),
                  varweighting="Buckland", icmethod="Lukacs", alphaIC=0.05)

# ~~~ add average predictions to  df
surv_data_sel$psmulti <- c(psmulti$averages)


# ~~~  Diagnostic plots -------------------------------------

#The first type of plot draws the IC profile (i.e., the ranked IC values of models), together with
#a horizontal line two IC units above the best model.
plot(obj, type = "p", highlight = c("f1:f2"))
plot(obj, type = "w", highlight = c("f1:f2"))
plot(obj, type = "s")
plot(obj, type = "r")












# predict with full model ------------------------------------------------

# model-averaged estimates of the diff parameters, with unconditional variance, importance, and confidence interval
coef(obj, select="all", varweighting="Buckland", icmethod="Lukacs", alphaIC=0.05)


# ~~~ glmulti predict percentage for the surveyed countries ------------------------------
psmulti<- predict(obj, select="all", newdata=NA, se.fit=FALSE, type = c("response"),
                  varweighting="Buckland", icmethod="Lukacs", alphaIC=0.05)

# ~~~ add average predictions to  df
surv_data_sel$psmulti <- c(psmulti$averages)


# ~~~  Diagnostic plots -------------------------------------

#The first type of plot draws the IC profile (i.e., the ranked IC values of models), together with
#a horizontal line two IC units above the best model.
plot(obj, type = "p", highlight = c("f1:f2"))
plot(obj, type = "w", highlight = c("f1:f2"))
plot(obj, type = "s")
plot(obj, type = "r")






### ggplot2 scatterplot HCES estimated and predicted -------------------------------
source('./consumption/plots/themes/custom_scatterplot_theme.r')

library(ggrepel)


s <- 
ggplot() +
 
  geom_hline(yintercept = 100, color='grey50', size=0.2) + 
  geom_vline(xintercept = 100, color='grey50', size=0.2) + 
  geom_abline(slope=1, intercept=0, color='grey50', size=0.2) +
  
  geom_point(data=surv_data_sel,
             aes(x=hces_surv_asprcfao, y=psmulti, size=sum_catch), color='dodgerblue', alpha=0.5) +   
  #, color= hces_surv_asprcfao_cut
  
  # add labels
  geom_text_repel(data= subset(surv_data_sel, (hces_surv_asprcfao > 210 | psmulti > 250)),
                  aes(label=country_label.x, x=hces_surv_asprcfao, y=psmulti), 
                  size = 2, 
                  colour='gray15', 
                  segment.size = 0.25, 
                  segment.color='gray55',
                  force = 8,
                  box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines')) +
  
  custom_scatterplot_theme +
  #coord_fixed() +
  # scale_color_manual(values =  c("-100 to -50" = '#1a1aff',
  #                               "-50 to -10" = '#ccccff',
  #                               "-10 to 10" = '#ffff99',
  #                               "10 to 50" = '#ffcccc',
  #                               "50 to 100" = '#ff6666',
  #                               "100 to 200" = '#ff0000',
  #                               "200 to 500" = '#990000',
  #                               "over 500" = '#330000'),
  #                   name = "Difference as percentage\nof reported statistics") +
  
  #xlim(0, 1350) + ylim(0, 1350) +
  xlab('Ratio of survey-estimated over reported catch') +
  ylab('Predicted ratio') +

  # scale_x_log10() + scale_y_log10() +
  scale_x_continuous(breaks=c(0, 50, 100, 200, 500, 1000)) +  
  scale_y_continuous(breaks=c(0, 50, 100, 200, 500, 1000)) +
  
  custom_scatterplot_theme +
  theme(axis.line = element_line(colour = "black"),
        legend.position = 'none', #c(0.115, .83), 
        text = element_text(size=7),
        legend.key = element_rect(colour = "white"),
        legend.background = element_rect(colour = "black", size=0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank())


# save plot 
ggsave("../Output/Figures/fig_scatter_dif_predict_v2.png", s,
       dpi=800, width=90, height=80, units='mm', type = "cairo-png")

dev.off()




### GGpair ------------------------------------------
# library(GGally)
# 
# cols_for_plotmatrix <- names(surv_data[,c(15:20,24:27)])
# 
# # make matrix of scatterplots between all covariates
# ggpairs(surv_data[c(cols_for_plotmatrix, 'mean_dif_perc')])


### get flags ca, aq, tr ------------------------------------------------
all_flag_tally_agg <- read.csv('../output/consumption/all_flag_tally_agg.csv',
                               stringsAsFactors = F)

all_flag_tally_agg <- all_flag_tally_agg %>%
                      mutate(flag_perc= as.factor(ifelse(flag_perc>0.2, 1, 0))) %>%
                      dplyr::select(country_code, TradeflowTradeflow, flag_perc) %>%
                      spread(TradeflowTradeflow, flag_perc) 

colnames(all_flag_tally_agg)[2:5] <- paste(colnames(all_flag_tally_agg)[2:5],"flag", sep="_")



### Add the standardized FAO stats
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



# ### calculate survey characteristics
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


### Read world bank indices -------------------------------------------------------
wdi <- read.csv('../data/socio_demo/wdi/WDI_Data.csv', stringsAsFactors=F)

colstokeep <- c('Land area (sq. km)',
                'Population density (people per sq. km of land area)',
                'Rural population',
                'Rural population (% of total population)',
                "Household final consumption expenditure (constant 2010 US$)" ,
                "Employment to population ratio, 15+, total (%) (national estimate)"  )


wdi_sel <- wdi %>%
          filter(Indicator.Name %in% colstokeep) %>%
          select(Country.Name, Country.Code, Indicator.Code, X2012) %>%
          spread(Indicator.Code, X2012) %>%
          mutate(country_code = countrycode(Country.Name, 'country.name', 
                                            'iso3c', warn = TRUE))


### Call script that calculates the prod percapita per country 
# It uses inland catch and population from FAO and area from GLWD.
source('./fishstatj_data/join_nat_catch_pop_wat_2012.R', print.eval=TRUE)

# add the numeric country code to table
prod_percap_df <- prod_percap_df %>%
                  mutate(iso3n = countrycode(prod_percap_df$Country, 'country.name', 
                                             'iso3n', warn = TRUE)) %>%
                  # somehow got duplicated rows of country_code
                  group_by(country_code) %>%
                  filter(row_number()==1) %>%
                  select(country_code, GLWD)


### join all together -----------------------------------------------------------
f<-'../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv'
surv_data <- read.csv(f, stringsAsFactors = FALSE)

surv_data <-  surv_data %>%
              left_join(., surv_char_assumps, by="country_code") %>%
              left_join(., all_flag_tally_agg, by="country_code") %>%
              left_join(., surv_fao_stdzd, by="country_code") %>%
              left_join(., wdi_sel, by="country_code") %>%
              left_join(., prod_percap_df, by="country_code") %>%
              filter(mean_dif_perc < 2000) %>%
              filter(tr_corr_consump > 0) %>%
              
              mutate(hces_surv_asprcfao = tr_corr_consump/sum_catch *100)




### Cut hces_surv_asprcfao_cut into bins  --------------------------------------------------
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
levels(surv_data$hces_surv_asprcfao_cut)





### Part 1: test effet of surveys  -------------------------------------------------
# the surveys do not bias in the percentage difference
formula<- hces_surv_asprcfao ~ perc_weight_assumed_fresh + perc_weight_fm
#formula<- mean_dif_perc ~ perc_weight_assumed_fresh + perc_weight_fm

fit <- glm(formula, data=surv_data_sel, family=gaussian(log), 
           start=c(5,5,5), control = list(maxit = 50))

# show results
summary(fit) 



### Part 2 - all predictors ---------------------------------------------------

cols<-c("EN.POP.DNST", "sum_catch",
        "SP.RUR.TOTL.ZS", "GLWD", "AG.LND.TOTL.K2", "sum_catch",
        'Aquaculture_flag', 'Catch_flag', 'Export_flag', 
        'Import_flag', 'export_fao_stdzd','import_fao_stdzd',
        'sum_aquacul_fao_stdzd')#, 'hces_surv_asprcfao')#SP.RUR.TOTL

# Employment to population ratio, 15+, total (%) (national estimate)   SL.EMP.TOTL.SP.NE.ZS
# Household final consumption expenditure (constant 2010 US$)     NE.CON.PRVT.KD
# Land area (sq. km)    AG.LND.TOTL.K2
# Population density (people per sq. km of land area)   EN.POP.DNST
# Rural population  SP.RUR.TOTL
# Rural population (% of total population)  SP.RUR.TOTL.ZS





# ~~~ set formula ---------------------------


# create formula
f <- hces_surv_asprcfao ~ Export_flag * export_fao_stdzd +
  Aquaculture_flag * sum_aquacul_fao_stdzd +
  Catch_flag +
  Import_flag * import_fao_stdzd +
  AG.LND.TOTL.K2  +  GLWD + SP.RUR.TOTL.ZS + sum_catch# + SP.RUR.TOTL

# create formula
f <- hces_surv_asprcfao ~ export_fao_stdzd + Aquaculture_flag * sum_aquacul_fao_stdzd +
  Catch_flag




a <- surv_data[complete.cases(surv_data[,cols]),cols]

surv_data_sel <- surv_data[complete.cases(surv_data[,cols]),]



### glmulti --------------------------------------------------------

library('glmulti')

obj <- glmulti(f, data=surv_data, family = gaussian(link='log'), 
               maxit = 30,
               confsetsize = 5,         # Keep 5 best models
               plotty = F,
               level = 2,               # interaction considered
               method = "g",            # Exhaustive approach
               crit = "aic")          # AIC as criteria)#,  method = "g")

# get formulas
obj@formulas

# summary of best model
summary(obj@objects[[1]])

obj@K
obj@formulas


bestmodel <- obj@formulas[1]
#importances <- summary(obj)$termweights


# ~~~ predict percentage for the surveyed countries ------------------------------
psmulti<- predict(obj, select="all", newdata=NA, se.fit=FALSE, type = c("response"),
                                varweighting="Buckland", icmethod="Lukacs", alphaIC=0.05)

# ~~~ add predictions to  df
surv_data$psmulti <- c(psmulti$averages)



# ~~~  Diagnostic plots -------------------------------------

#The first type of plot draws the IC profile (i.e., the ranked IC values of models), together with
#a horizontal line two IC units above the best model.
plot(obj, type = "p", highlight = c("f1:f2"))
plot(obj, type = "w", highlight = c("f1:f2"))
plot(obj, type = "s")
plot(obj, type = "r")

plot(seq_len(nrow(bestBIC$Subsets)) - 1, bestBIC$Subsets[,"BIC"], type="b", xlab = "Number of Predictors", ylab = "BIC")




### use best glm ---------------------------------------------------------------
library(bestglm)
library('leaps')
# leaps(x= a[,cols], y= log(surv_data$hces_surv_asprcfao), nbest=10)
# #, wt=rep(1, NROW(x)), int=TRUE, method=c("Cp", "adjr2", "r2"), 
# #      names=NULL, df=NROW(x), strictly.compatible=TRUE)


set.seed(2377723)
#x= data.matrix(a[,cols]), y= log(surv_data$hces_surv_asprcfao)

obj <- bestglm(a, family = gaussian, IC="CV") 
#, start=c(rep(1,1)))#, CVArgs = list(Method='HTF', K=4, REP=10))

bestBIC_test(10) # OK, running
#out <- bestglm(a, family = gaussian(link='log'), IC="CV", CVArgs = list(Method='HTF', K=4, REP=10))
#out <- bestglm(a, IC = "CV", CVArgs = list(Method = "HTF", K = 10, REP = 1))
print.bestglm(out)
summary.bestglm(out)




### stepwise feature selection -------------------------------------------------

# create formula
f <- hces_surv_asprcfao ~ Export_flag * export_fao_stdzd +
                          Aquaculture_flag * sum_aquacul_fao_stdzd +
                          Catch_flag +
                          Import_flag * import_fao_stdzd +
                          AG.LND.TOTL.K2  +  GLWD + SP.RUR.TOTL.ZS + sum_catch# + SP.RUR.TOTL

nothing <- glm(data=surv_data_sel, hces_surv_asprcfao ~ 1, family = gaussian(link='log'))
fullmod <- glm(data=surv_data_sel, formula = f, family = gaussian(link='log'))#
formula(fullmod)
summary(fullmod)


bothways =  step(nothing, list(lower=formula(nothing), upper=formula(fullmod)),
         direction="both", trace=0, maxit=200, epsilon = 1e-14)#, na.action = omit)
formula(bothways)
summary(bothways)

surv_data_sel <- surv_data[complete.cases(surv_data[,cols]),]

surv_data_sel$ps <- predict(bothways, newdata = surv_data_sel,
        type = c("response"),
        se.fit = FALSE, dispersion = NULL, terms = NULL,
        na.action = na.pass)



### LASSO regression -----------------------------------------------------------
library('glmnet')

glmmod <- glmnet(x= data.matrix(a[,cols]), y= log(surv_data$hces_surv_asprcfao), family="gaussian")

# Plot variable coefficients vs. shrinkage parameter lambda.
plot(glmmod, xvar = "lambda", label = TRUE)


# predict
surv_data$ps <- predict(glmmod, newx = data.matrix(a[,cols]), type = "response", s = 0.05)






### ggplot2 scatterplot HCES estimated and predicted -------------------------------
source('./consumption/plots/themes/custom_scatterplot_theme.r')

library(ggrepel)


#s <- 
ggplot(surv_data_sel) +
 
  geom_hline(yintercept = 100, color='grey70', size=0.9) + 
  geom_vline(xintercept = 100, color='grey70', size=0.9) + 
  geom_abline(slope=1, intercept=0, color='grey70', size=0.9) +
  
  geom_point(aes(x=hces_surv_asprcfao, y=psmulti, size=sum_catch), alpha=0.3) +   #, color= hces_surv_asprcfao_cut
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

  scale_x_continuous(breaks=c(0, 50, 100, 200, 500, 1000)) +  
  scale_y_continuous(breaks=c(0, 50, 100, 200, 500, 1000)) +
  
  theme(axis.line = element_line(colour = "black"),
        legend.position = 'none', #c(0.115, .83), 
        text = element_text(size=6),
        legend.key = element_rect(colour = "white"),
        legend.background = element_rect(colour = "black", size=0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank())


#save plot 
ggsave("../Output/Figures/fig_scatter_dif_predict.png", s,
       dpi=800, width=90, height=100, units='mm', type = "cairo-png")

dev.off()




 


### GGpair ------------------------------------------
# library(GGally)
# 
# cols_for_plotmatrix <- names(surv_data[,c(15:20,24:27)])
# 
# # make matrix of scatterplots between all covariates
# ggpairs(surv_data[c(cols_for_plotmatrix, 'mean_dif_perc')])


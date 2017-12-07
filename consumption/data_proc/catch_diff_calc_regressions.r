

# get flag tally -------------------------------------------------------
all_flag_tally_agg <- read.csv('../output/consumption/all_flag_tally_agg.csv',
                               stringsAsFactors = F)

all_flag_tally_agg <- all_flag_tally_agg %>%
                      mutate(flag_perc= as.factor(ifelse(flag_perc>0.2, 1, 0))) %>%
                      dplyr::select(country_code, TradeflowTradeflow, flag_perc) %>%
                      spread(TradeflowTradeflow, flag_perc) 

colnames(all_flag_tally_agg)[2:5] <- paste(colnames(all_flag_tally_agg)[2:5],"flag", sep="_")



### calc survey characteristics ------------------------------
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


# get the standardized FAO stats ----------------------------------------
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


### other predictors
#source('./consumption/data_proc/read_predictors_for_regressions.r')


# get world bank indices -------------------------------------------------------
wdi <- read.csv('../data/socio_demo/wdi/WDI_Data.csv', stringsAsFactors=F)

colstokeep <- c("GDP per capita (constant 2010 US$)",
                'Land area (sq. km)',
                'Population density (people per sq. km of land area)',
                'Rural population',
                'Rural population (% of total population)',
                "Household final consumption expenditure (constant 2010 US$)" ,
                "Employment to population ratio, 15+, total (%) (national estimate)"  )


wdi_sel <- wdi %>%
  filter(Indicator.Name %in% colstokeep) %>%
  select(Country.Name, Country.Code, Indicator.Code, X2012) %>%
  spread(Indicator.Code, X2012) %>%
  mutate(country_code = Country.Code)



# # get catch percapita  ----------------------------------------------------- 
# # It uses inland catch and population from FAO and area from GLWD.
# source('./fishstatj_data/join_nat_catch_pop_wat_2012.R', print.eval=TRUE)
# 
# # add the numeric country code to table
# prod_percap_df <- prod_percap_df %>%
#                   mutate(iso3n = countrycode(prod_percap_df$Country, 'country.name', 
#                                              'iso3n', warn = TRUE)) %>%
#                   # somehow got duplicated rows of country_code
#                   group_by(country_code) %>%
#                   filter(row_number()==1) %>%
#                   select(country_code, GLWD)



### Read Inland water surface area -------------------------------------------------------------------
f<-'../data/water_area/GSWD_GlobalEstimatesCalc_CoastCorrected_v2_23Dec2013_bycountry.csv'
w_area <- read.csv(f, stringsAsFactors=FALSE)

w_area <- w_area %>%
  mutate(countrycode(w_area$Country, 'country.name', 'iso3c', warn = F)) %>%
  select(Country, GLWD, MAMax, country_code) %>%
  filter(!is.na(country_code))



### join all df together -------------------------------------------------------
f <- '../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv'
surv_data <- read.csv(f, stringsAsFactors = FALSE)


surv_data <- surv_data %>%
  left_join(., surv_char_assumps, by="country_code") %>%
  left_join(., all_flag_tally_agg, by="country_code") %>%
  left_join(., surv_fao_stdzd, by="country_code") %>%
  left_join(., wdi_sel, by="country_code") %>%
  left_join(., w_area, by="country_code") %>%
  filter(mean_dif_perc <2000) %>%
  filter(tr_corr_consump >0) %>%
  
  mutate(hces_surv_asprcfao = tr_corr_consump/sum_catch *100)

# del 
rm(wdi, wdi_sel, all_flag_tally_agg, surv_fao_stdzd)



# cut sum_catch into bins for plotting purposes..? ----------------------------
my_breaks = c(-100, -10, 10, 1000000)
surv_data$mean_dif_perc_cut <- cut(surv_data$mean_dif_perc, breaks = my_breaks,
                                   dig.lab=10)

# replace the categories stings to make them nicer in the legend
surv_data$mean_dif_perc_cut <- gsub("\\(|\\]", "", surv_data$mean_dif_perc_cut)
surv_data$mean_dif_perc_cut <- gsub("\\,", " to ", surv_data$mean_dif_perc_cut)
surv_data <- surv_data %>% mutate(mean_dif_perc_cut=ifelse(mean_dif_perc_cut=="500 to 1000000",
                                                           "over 500",mean_dif_perc_cut))





# Part 1: test effet of survey assumption -------------------------------------------------

# https://modtools.wordpress.com/2013/08/14/dsquared/
library('modEvA')


### the surveys do not bias in the percentage difference
formula<- mean_dif_perc ~ perc_weight_assumed_fresh + perc_weight_fm
formula<- hces_surv_asprcfao ~ perc_weight_assumed_fresh + perc_weight_fm
formula<- hces_surv_asprcfao ~ perc_weight_fm + perc_weight_assumed_fresh


fit <- glm(formula, data=surv_data, family=gaussian(log), start=c(5,5,5), control = list(maxit = 50))
summary(fit) # show results

# https://rdrr.io/rforge/modEvA/man/Dsquared.html
Dsquared(model = fit, 
         obs = surv_data$hces_surv_asprcfao,
         family = "gaussian")



# Part 2: Test effect of country geography,etc ---------------------------------
cols<-c("AG.LND.TOTL.K2", "EN.POP.DNST", "NE.CON.PRVT.KD", "SP.RUR.TOTL", "sum_catch",
        "SP.RUR.TOTL.ZS", 'NY.GDP.PCAP.KD', "MAMax", "hces_surv_asprcfao")

#cols<-c("AG.LND.TOTL.K2", "sum_catch", "hces_surv_asprcfao")

a<- surv_data[,cols]

a<-a[complete.cases(a),]

f <- hces_surv_asprcfao ~ AG.LND.TOTL.K2 + EN.POP.DNST + NE.CON.PRVT.KD + 
  SP.RUR.TOTL + SP.RUR.TOTL.ZS + MAMax


f <- hces_surv_asprcfao ~ AG.LND.TOTL.K2 + sum_catch

b<- glm(data = a, formula = f, family = gaussian(link='log'))#
summary(b)

##  scatterplot: large % difference are found for smaller countries. 
ggplot(surv_data) +
  geom_point(aes(x=sum_catch, y=hces_surv_asprcfao, size=4), alpha=0.4) +
  geom_hline(yintercept=0) + geom_hline(yintercept=c(100,200), color= 'grey75') + 
  ylim(0,1500)





### Part 3 - flags and stuff ------
library(bestglm)

cols<-c('Aquaculture_flag', 'Catch_flag', 'Export_flag', 
        'Import_flag', 'export_fao_stdzd','import_fao_stdzd',
        'sum_aquacul_fao_stdzd', 'hces_surv_asprcfao')
a<- surv_data[,cols]

f <- hces_surv_asprcfao ~ Export_flag*export_fao_stdzd +
  Aquaculture_flag*sum_aquacul_fao_stdzd + Catch_flag + Export_flag + 
  Import_flag + export_fao_stdzd + import_fao_stdzd + 
  sum_aquacul_fao_stdzd + 
  Aquaculture_flag*sum_aquacul_fao_stdzd +
  Import_flag*import_fao_stdzd



b<- glm(data = a, formula = f, family = gaussian(link='log'))#
#start = c(rep(1,8)),
#IC = "AIC")
#method = "exhaustive")
summary(b)


res.bestglm <-
  bestglm(Xy = a,
          family = gaussian(link='log'),
          start = c(rep(1,8)),
          IC = "AIC", # Information criteria for
          method = "exhaustive")


summary(res.bestglm$BestModel)
BestModel <- formula(res.bestglm$BestModel)
predict.glm(BestModel, newdata = )





ggplot(surv_data) +
  
  geom_boxplot(aes(x=Catch_flag, y=hces_surv_asprcfao, color=tr_corr_consump), size=0.3) +
  geom_point(aes(x=Catch_flag, y=hces_surv_asprcfao, color=tr_corr_consump), size=4, alpha=0.3) +
  geom_hline(yintercept=0) + geom_hline(yintercept=100, color= 'grey75') 

ggplot(surv_data) +
  geom_boxplot(aes(x=Export_flag, y=hces_surv_asprcfao, color=tr_corr_consump), size=0.3) +
  geom_point(aes(x=Export_flag, y=hces_surv_asprcfao, color=tr_corr_consump), size=4, alpha=0.3) +
  geom_hline(yintercept=0) + geom_hline(yintercept=100, color= 'grey75') 








### Part 4 - alltogether now!  err predictors at once --------------------------------------------
library(bestglm)

cols<-c("EN.POP.DNST", "NE.CON.PRVT.KD", "SP.RUR.TOTL", "sum_catch",
        "SP.RUR.TOTL.ZS", "MAMax", 'NY.GDP.PCAP.KD', "AG.LND.TOTL.K2", "sum_catch",
        'Aquaculture_flag', 'Catch_flag', 'Export_flag', 
        'Import_flag', 'export_fao_stdzd','import_fao_stdzd',
        'sum_aquacul_fao_stdzd', 'hces_surv_asprcfao')


surv_data_sel <- surv_data[complete.cases(surv_data[,cols]), cols]

surv_data_sel <- surv_data[complete.cases(surv_data[,cols]),]



# ~~~ create formula------------
f <- hces_surv_asprcfao ~ Export_flag * export_fao_stdzd +
  Aquaculture_flag * sum_aquacul_fao_stdzd + 
  Catch_flag + 
  Import_flag * import_fao_stdzd +
  AG.LND.TOTL.K2 + sum_catch + SP.RUR.TOTL + MAMax + 
  NY.GDP.PCAP.KD + SP.RUR.TOTL.ZS

nothing <- glm(data=surv_data_sel, hces_surv_asprcfao ~ 1, family = gaussian(link='log'))
fullmod <- glm(data=surv_data_sel, formula = f, family = gaussian(link='log'))#
formula(fullmod)
summary(fullmod)


# backwards = step(fullmod, direction="backward") #, trace=0) #would suppress step by step output.
# formula(backwards)
# summary(backwards)


bothways =  step(nothing, list(lower=formula(nothing), upper=formula(fullmod)),
         direction="both", trace=0)#, na.action = omit)
formula(bothways)
summary(bothways)


surv_data_sel$ps <- predict(bothways, newdata = NULL,
        type = c("response"),
        se.fit = FALSE, dispersion = NULL, terms = NULL,
        na.action = na.pass)


# 
# library(boot)
# k    <- 3
# kfCV <- cv.glm(data=surv_data, glmfit=bothways, K=k)
# kfCV$delta



### 
source('./consumption/plots/themes/custom_scatterplot_theme.r')

library(ggrepel)


s <- ggplot(surv_data_sel) +
 
  geom_hline(yintercept = 100, color='grey70', size=0.9) + 
  geom_vline(xintercept = 100, color='grey70', size=0.9) + 
  geom_abline(slope=1, intercept=0, color='grey70', size=0.9) +
  
  geom_point(aes(x=hces_surv_asprcfao, y=ps))+ #, color=mean_dif_perc_cut)) +
  # add labels
  geom_text_repel(data= subset(surv_data_sel, hces_surv_asprcfao/ps > 2 | hces_surv_asprcfao/ps < 0.5),
                  aes(label=country_label.x, x=hces_surv_asprcfao, y=ps), 
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

  scale_x_continuous(breaks=c(0, 50, 100, 200, 500)) +  
  scale_y_continuous(breaks=c(0, 50, 100, 200, 500)) +
  
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


### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/fig_scatter_dif_predict.png", s,
       dpi=800, width=90, height=100, units='mm', type = "cairo-png")

dev.off()






####
library(GGally)

cols_for_plotmatrix <- names(surv_data[,c(15:20,24:27)])

# make matrix of scatterplots between all covariates
ggpairs(surv_data[c(cols_for_plotmatrix, 'mean_dif_perc')])




#########################
library(rpart)

# grow tree 
fit <- rpart(mean_dif_perc_cut ~ Aquaculture_flag + Catch_flag + Export_flag + 
               Export_flag + export_fao_stdzd + import_fao_stdzd +sum_aquacul_fao_stdzd, 
             method="class",data=surv_data) 
# cp=0.00001, 

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits


# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	

# plot tree 
png(filename="../output/figures/treetest.png")
plot(fit, uniform=TRUE, 
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
dev.off()

# Conditional Inference Tree for Kyphosis
library(party)
fit <- ctree(Kyphosis ~ Age + Number + Start, 
             data=kyphosis)
plot(fit, main="Conditional Inference Tree for Kyphosis")





library(tree)



#real.estate <- read.table("cadata.dat", header=TRUE)
tree.model <- tree(mean_dif_perc ~ Aquaculture_flag + Catch_flag + Export_flag +
                   Import_flag + export_fao_stdzd + import_fao_stdzd +sum_aquacul_fao_stdzd,
                   data=surv_data)
plot(tree.model)
text(tree.model, cex=.75)

# col=grey(10:2/11)[surv_data$mean_dif_perc],
plot(x=surv_data$Catch_flag, y=surv_data$export_fao_stdzd,  pch=20, xlab="Catch_flag",ylab="export_fao_stdzd")
partition.tree(tree.model, ordvars=c("Catch_flag","export_fao_stdzd"), add=TRUE)




formula<- mean_dif_perc ~ Aquaculture_flag + Catch_flag + Export_flag +
                          Import_flag + export_fao_stdzd + import_fao_stdzd + sum_aquacul_fao_stdzd+ 
                          (Aquaculture_flag * sum_aquacul_fao_stdzd) +
                          (Export_flag * export_fao_stdzd) +
                          (Import_flag * import_fao_stdzd)


fit <- glm(formula, start=rep(1, 11), data=surv_data, control = list(maxit = 50))
summary(fit) # show results



f <- mean_dif_perc ~ perc_weight_assumed_fresh + perc_weight_fm + (perc_weight_assumed_fresh * perc_weight_fm)
#fit <- 
step(glm(f, family= gaussian(link='logit'),  start=rep(1,4), data=surv_data), direction="both")

#, family= gaussian(link='log'), start=rep(1,2), data=surv_data)
summary(fit) # show results






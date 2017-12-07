source('./consumption/plots/themes/custom_catch_trend_line_theme.r')




# calc tot flag ---------------------------------------------------------
surv_data <- surv_data %>%
            
             mutate(Aquaculture_flag = as.numeric(Aquaculture_flag),
                    Catch_flag = as.numeric(Catch_flag),
                    Export_flag = as.numeric(Export_flag),
                    Import_flag = as.numeric(Import_flag),
                    flag_tot = (Aquaculture_flag + Catch_flag + Export_flag + Import_flag)-4)


# ggplot - box flag count ------------------------------------------------

b<- ggplot(surv_data) +
  
  geom_hline(yintercept = 100, size= 0.5, color='grey80') +
  geom_boxplot(aes(x=as.factor(flag_tot), y= hces_surv_asprcfao), 
               outlier.alpha = 0, size=0.3, width= 0.3, color= 'grey40') +
  
  geom_point(aes(x=as.factor(flag_tot), y= hces_surv_asprcfao,  
                 color=as.factor(Catch_flag)), alpha=0.75, size=2) + #size=sum_catch

  xlab('Numbrt of FAO statistics flagged') +
  ylab('Percentage difference\nbetween HCES and FAO catch') +
  custom_catch_trend_line_theme

# ~~~ save figure to file ----------------------------------------------------- 
ggsave('../output/figures/fig_boxplot_flagcnt.png', b,
       width=90, height=80, dpi=800, units="mm", type = "cairo-png")
dev.off()





### ggplot - sum catch vs prc diff ------------------------------------------------

s<- ggplot(surv_data) +
  
  geom_hline(yintercept = 100, size= 0.5, color='grey80') +
  #geom_boxplot(aes(x=as.factor(flag_tot), y= hces_surv_asprcfao), 
  #             outlier.alpha = 0, size=0.3, width= 0.3, color= 'grey40') +
  
  geom_point(aes(x=sum_catch, y= hces_surv_asprcfao,  
                 color=as.factor(Catch_flag)), alpha=0.75, size=2) +
  xlab('Reported inland catch') +
  ylab('Percentage difference\nbetween HCES and FAO catch') +
  custom_catch_trend_line_theme

### save figure to file 
ggsave('../output/figures/fig_scatter_sumcatch_prcdiff.png', s,
       width=90, height=80, dpi=800, units="mm", type = "cairo-png")
dev.off()





# Plot each flag type vs diff --------------------------------------------------  

# ~~~ prep df for flag facet plot ----------------------------------------------
surv_data_forplot <- surv_data %>%
                     select(hces_surv_asprcfao, Aquaculture_flag, Catch_flag, Export_flag, Import_flag) %>%
                     gather(flag_type, flag_yn, Aquaculture_flag:Import_flag)


# ~~~ ggplot - flag facet plot --------------------------------------------------
f <- ggplot(surv_data_forplot) +
            geom_boxplot(aes(x=as.factor(flag_yn), y= hces_surv_asprcfao)) +
            geom_point(aes(x=as.factor(flag_yn), y= hces_surv_asprcfao), size=2, color='red', alpha=0.5) +
            facet_wrap(~flag_type, nrow=1) +
            geom_hline(yintercept = 100, size= 0.5, color='grey80') +
            xlab('FAO statistics flagged') +
            ylab('Percentage difference\nbetween HCES and FAO catch') +
            custom_catch_trend_line_theme


# ~~~ save figure to file ------------------------------------------------------ 
ggsave('../output/figures/fig_boxplot_flag_facet.png', f,
       width=210, height=90, dpi=800, units="mm", type = "cairo-png")
dev.off()







# boxplot ca flag type vs diff --------------------------------------------------  

# ~~~ prep df for flag facet plot ----------------------------------------------
surv_data_forplot <- surv_data %>%
  select(hces_surv_asprcfao, Aquaculture_flag, Catch_flag, Export_flag, Import_flag) %>%
  gather(flag_type, flag_yn, Aquaculture_flag:Import_flag)


# ~~~ ggplot - flag facet plot --------------------------------------------------
f <- ggplot(surv_data) +
  
  geom_hline(yintercept = 100, size= 0.5, color='grey80') +
  geom_boxplot(aes(x=as.factor(Catch_flag), y= hces_surv_asprcfao, fill=as.factor(Catch_flag)), 
               outlier.colour=NA, width=0.5, size=0.2) +
  
  geom_point(aes(x=as.factor(Catch_flag), y= hces_surv_asprcfao, size=sum_catch), 
             color='blue', alpha=0.8, shape=21) +
  
  xlab('FAO statistics flagged') +
  ylab('Percentage difference\nbetween HCES and FAO catch') +
  custom_catch_trend_line_theme +
  scale_fill_brewer(palette='YlOrBr',
                    labels =  c("2" = 'Catch flagged as estimated ',
                                "1" = 'Catch not flagged')) +
  theme(legend.position = 'none')
  


f

# ~~~ save figure to file ------------------------------------------------------ 
ggsave('../output/figures/fig4b_boxplot_ca_flag.png', f,
       width=60, height=90, dpi=800, units="mm", type = "cairo-png")
dev.off()


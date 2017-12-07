
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)

# read input ---------------------------------------------
prod_agg_comb <- read.csv('../output/consumption/prod_coded_n_agg/hh_survey_consump_nat_agg_prod_combined_diff.csv',
                          stringsAsFactors=FALSE)


# Filter data by --------------------------------------------------------------- 
prod_agg_comb <- prod_agg_comb %>%
  # replace country names to shorter format, using part before comma
  mutate(country = unlist(lapply(strsplit(prod_agg_comb$country,','), '[[', 1))) %>%
  # replace Lao manualle because of absence of comma in long name
  mutate(country = ifelse(grepl("Lao People's Democratic Republic", prod_agg_comb$country), "Lao", country)) %>%
  # compound country name and survey year
  mutate(country= paste(country,' (', year_start,')', sep='')) %>%
  # 
  mutate(country= factor(country, levels = unique(country[order(sum_catch_millions)]))) %>%
  # filter to remove negative catch, or NA diffs, and keep only high conf.
  filter(surv_catch > 0, 
         !is.na(tons_dif),
         confidence_lvl== 'high') 

# # Order countries as factors based on all catch
# ordered_countries <- prod_agg_comb %>% 
#   filter(prod_src=='M|F|F/M') %>%
#   select(country, sum_catch_millions) %>%
#   mutate(country= factor(country, 
#                          levels = a))
# a<-unique(prod_agg_comb$country[order(subset(prod_agg_comb, 
#                                           prod_agg_comb$prod_src=='M|F|F/M')['sum_catch_millions'])])


# add all combinations of countries and sources to df
# so that bars are all same width in plot
prod_agg_comb <- rbind.fill(prod_agg_comb, 
                            cbind(expand.grid(country=(unique(prod_agg_comb$country)), 
                                              prod_src=(unique(prod_agg_comb$prod_src)))))


# this filtering must happen after the expansion to include all combinations of countries and product source 
prod_agg_comb <- prod_agg_comb %>%
  arrange(desc(tons_dif)) %>%
  mutate(country = factor(country))


### plot the surv_catch and FAO catch side by side -------------------------------
# **prep data  --------- 
prod_agg_comb_forsidebyside <-  prod_agg_comb %>% 
    
    select(country, surv_catch, sum_catch_millions, prod_src, surv_catch_rng) %>%
    gather(catch_data_type, value, surv_catch:sum_catch_millions) %>%
    mutate(surv_catch_rng= ifelse(catch_data_type=='sum_catch_millions', NA,surv_catch_rng),
           surv_catch_rng= ifelse(surv_catch_rng==0, NA,surv_catch_rng),
           catch_data_type= ifelse(catch_data_type=='sum_catch_millions','FAO catch','Survey catch'),
           prod_src= ifelse(prod_src=='F|F/M','Inland', prod_src),
           prod_src= ifelse(prod_src=='M|F/M','Marine', prod_src),
           prod_src= ifelse(prod_src=='M|F|F/M','All', prod_src))


# ** make bar plot of surv_catch and FAO ------------------------------------------
c <-  ggplot(data=prod_agg_comb_forsidebyside, 
             aes(x=country, y=value, 
                 ymin = value-surv_catch_rng, 
                 ymax= value+surv_catch_rng, 
                 fill=catch_data_type)) +
      
      geom_bar(stat="identity", position=position_dodge(), width=0.8) + 
      geom_errorbar(position=position_dodge(width=0.9), width=0.5, size=0.25, colour='grey10') +    
      
      coord_flip()+ #coord_flip(ylim=c(-2,1.5)) + 
  barplot_catchdiff_theme +
  geom_vline(xintercept=seq(1.5, length(unique(prod_agg_comb$country))-0.5, 1), 
             lwd=0.1, colour="grey10") + 
  geom_hline(yintercept = 0, lwd=0.25, colour= 'black') + 
  scale_fill_grey() +
  
  ggtitle('') + xlab("") + ylab("Catch (million tons)") +
  facet_wrap(~prod_src)#, scales = "free_y")
c


# **save figure to output file -----------------------------------
ggsave("../output/figures/fao_surv_catch_barplot_sep_sources_v2.png",
       width=200, height=130, dpi=800, units="mm", type = "cairo-png")
dev.off()




# ** make bar plot POP PYRAMID STYLE of surv_catch and FAO ------------------------------------------
cp <-  ggplot(data=prod_agg_comb_forsidebyside, 
              aes(x=country, 
                  fill=catch_data_type)) +
  
  geom_bar(data=subset(prod_agg_comb_forsidebyside, prod_src=="Inland"),
           position=position_dodge(),
           stat="identity", 
           aes(y=value),
           width=0.8) +
  
  geom_bar(data=subset(prod_agg_comb_forsidebyside, prod_src=="Marine"),
           position=position_dodge(),
           stat="identity",
           aes(y=value*-1),
           width=0.8) +
  
  geom_errorbar(data=subset(prod_agg_comb_forsidebyside, prod_src=="Inland"),
                aes(y=value,
                    ymin = value-surv_catch_rng,
                    ymax= value+surv_catch_rng),
                position=position_dodge(width=0.9),
                width=0.5, size=0.25, colour='grey10') +
  
  geom_errorbar(data=subset(prod_agg_comb_forsidebyside, prod_src=="Marine"),
                aes(y=value*-1,
                    ymin = (value-surv_catch_rng)*-1,
                    ymax= (value+surv_catch_rng)*-1),
                position=position_dodge(width=0.9), 
                width=0.5, size=0.25, colour='grey10') + 
  
  annotation_custom(grobTree(textGrob("Marine", x=0.12, y=1.01, hjust=0)))+
  annotation_custom(grobTree(textGrob("Inland", x=0.68, y=1.01, hjust=0)))+
  #gp=gpar(col="blue", fontsize=15, fontface="italic"))))+
  
  coord_flip()+
  barplot_catchdiff_theme +
  geom_vline(xintercept=seq(1.5, length(unique(prod_agg_comb$country))-0.5, 1), 
             lwd=0.1, colour="grey10") + 
  geom_hline(yintercept = 0, lwd=0.25, colour= 'black') + 
  scale_fill_grey() +
  
  ggtitle('') + xlab("") + ylab("Catch (million tons)") +
  scale_y_continuous(breaks=seq(-3,2,by=1),labels=abs(seq(-3,2,by=1)))

cp

cpg <- ggplotGrob(cp)
cpg$layout$clip[cpg$layout$name=="panel"] <- "off"
grid.draw(cpg)


# **save figure to output file -----------------------------------
ggsave("../output/figures/fao_surv_catch_barplot_pyr_sep_sources_v3.png", plot=cpg,
       width=200, height=140, dpi=800, units="mm", type = "cairo-png")
dev.off()




# create plot of surv_catch VS FAO difference ----------------------------------
# add bars and errorbars
# Note: errorbars representing the F/M uncertainty are not appropriate for 
# catch comparisons including all products because all products are includuded!
# (uncertainty for the FWEAs should be included however.
# Once all the sources of uncertainty, change bars to geom_boxplot() ???


d <-  ggplot(data=prod_agg_comb, aes(x=country, y=tons_dif, fill=prod_src, 
                                     ymin=tons_dif_lwr, ymax=tons_dif_upr)) +
  
  geom_bar(stat="identity", position=position_dodge(), width=0.7) +
  geom_errorbar(position='dodge', width=0.7, size=0.1, colour='grey10') +
      
      
      geom_vline(xintercept=seq(1.5, length(unique(prod_agg_comb$country))-0.5, 1), 
                 lwd=0.3, colour="grey65") + 
      geom_hline(yintercept = 0, lwd=0.25, colour= 'black') +
  

      coord_flip(ylim=c(-2,1.5)) + barplot_catchdiff_theme +
      ggtitle('') + xlab("") + ylab("Catch difference (million tons)")

d


# save figure to output file -----------------------------------
ggsave("../output/figures/catch_diff_barplot_sep_sources.png",
       width=90, height=200, dpi=800, units="mm", type = "cairo-png")
#dev.off()



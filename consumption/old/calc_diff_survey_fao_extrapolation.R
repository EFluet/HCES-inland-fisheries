


### Import modules -------------------------------------------------------------
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(stringr)
options("scipen"=100, "digits"= 8)


### Read the data consumption table ----------------------------------------------
consump_nat_agg_df <- read.csv('../output/consumption/hh_survey_consump_nat_agg_prod.csv')


### calculate diff between survey & FAO data -----------------------------------
consump_nat_agg_df <- consump_nat_agg_df %>%
      
      # rename colmuns with shorter names
      mutate(surv_catch = consump_million.tons.yr_fwaes_mf50_noimp, 
             surv_catch_rng = consump_million.tons.yr_fwaes_mf50_rng) %>%
      
  
      # calculate percentage difference between the survey and regular  
      mutate(prc_dif = surv_catch/sum_catch_millions,
             prc_dif_lwr = (surv_catch - surv_catch_rng)/sum_catch_millions,
             prc_dif_upr = (surv_catch + surv_catch_rng)/sum_catch_millions) %>%
      

      # delete unneeded columns, and those replaced with shorter colname
      select(-one_of(c('X','country.x','country.y','source',
                       'consump_million.tons.yr',
                       'consump_million.tons.yr_fwaes',
                       'consump_million.tons.yr_fwaes_mf50',
                       'consump_million.tons.yr_fwaes_mf50_rng',
                       'consump_million.tons.yr_fwaes_mf50_noimp')))
  
      # keep only rows with positive catch
      #filter(prc_dif_lwr > 0)


### Join the FAO catch for all countries for 2012 ------------------------------
source('./fishstatj_data/join_nat_catch_pop_wat_2012.R')

# remove duplicate countries (e.g. china & china,mainland)
prod_percap_df <- prod_percap_df %>%
                  distinct(country_code) %>%
                  mutate(sum_catch = sum_catch/1000000)


# join consump to entire country list 
ca_ctry_jnd <-  left_join(prod_percap_df, consump_nat_agg_df, 
                         by=c('country_code'='country_code')) %>%
            
  # calcualte the substituted catch for the same year (2012)
  mutate(subst_catch_mid= ifelse(!is.na(surv_catch), sum_catch*prc_dif, NA),
         subst_catch_lwr= ifelse(!is.na(surv_catch), sum_catch*prc_dif_lwr, NA),
         subst_catch_upr= ifelse(!is.na(surv_catch), sum_catch*prc_dif_upr, NA))

# remove var from env.
#rm(consump_nat_agg_df, prod_percap_df)


### summary table of catch -----------------------------------------------------

# calculate sum for all columns, transpose to maintain columns
sums_df <- as.data.frame(t(colSums(Filter(is.numeric, ca_ctry_jnd), na.rm = TRUE)))

# calculate other indicators from sums
sums_df <- sums_df %>%
  
           # keep only columns that can be summed (not much sense in summing up percentages)
           select(sum_catch, sum_catch_millions, surv_catch, surv_catch_rng, year_start,
                  subst_catch_mid, subst_catch_lwr, subst_catch_upr) %>%
  
           # calculate other indicators from the sums
           mutate(subst_catch_rng = subst_catch_mid - subst_catch_lwr,
                  perc_fao_catch_in_surv_countries = sum_catch_millions/sum_catch,
                  perc_diff_fao_subst_mid = subst_catch_mid/sum_catch_millions,
                  perc_diff_fao_subst_rng = (subst_catch_mid-subst_catch_lwr)/sum_catch_millions)


### Set common to vars for plotting -----------

lim <- c(-800,800)


### prep data for bar plots from the ca_ctry_jnd table  ---------------------

ca_ctry_jnd_p <-  ca_ctry_jnd %>% 
            filter(!is.na(prc_dif) & 
                     prc_dif < 1000) %>%
            arrange(desc(prc_dif)) %>%
            mutate(Country= paste(Country, year_start, sep=',')) %>%
            mutate(Country= factor(Country, levels = Country[order(sum_catch)]))


# Draw plot for 80% ------------------------------------------------------------

ca_ctry_jnd_t80 <-  ca_ctry_jnd_p %>%
              filter(grp == 'First 80%') %>%
              arrange(desc(prc_dif))

t80_width = nrow(ca_ctry_jnd_t80) / 30

t80 <-  ggplot(ca_ctry_jnd_t80, 
             aes(x= factor(Country),
                 y= prc_dif*100,
                 ymin=prc_dif_lwr*100,
                 ymax=prc_dif_upr*100,
                 fill = continent)) + 
  
  # add bars and errorbars
  geom_bar(stat='identity', width=t80_width, position='dodge') + geom_errorbar(position='dodge')+
  
  #
  geom_hline(yintercept = 100, colour= 'red') +
  
    coord_flip() +
  ggtitle('') +
  ylim(lim) + xlab("") + ylab("") + 
  theme_bw() +
  theme(text = element_text(size=8),
        plot.title = element_text(size = rel(0.8)),
        legend.position="none",
        axis.line = element_line(color = 'black'),
        plot.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        axis.ticks  = element_blank(),
        axis.line   = element_line(colour=NA),
        axis.line.x = element_line(colour="grey80"))

t80


# Draw plot for remaining 20% ------------------------------------------------------------

ca_ctry_jnd_p20 <-  ca_ctry_jnd_p %>% 
                    filter(grp == 'Remaining 20%') %>%
                    arrange(desc(prc_dif))

b20_width = nrow(ca_ctry_jnd_p20) / 30



b20 <-  ggplot(ca_ctry_jnd_p20, 
               aes(x= factor(Country),
                   y= prc_dif*100,
                   ymin=prc_dif_lwr*100,
                   ymax=prc_dif_upr*100,
                   fill = continent)) + 
  
  # add bars and errorbars
  geom_bar(stat='identity', width=b20_width, position='dodge') + geom_errorbar()+
  
  #
  geom_hline(yintercept = 100, colour= 'red') +
  
  coord_flip() +
  ggtitle('') +
  ylim(lim) +
  
  xlab("") +ylab("Percentage difference to FAO catch") + 
  theme_bw() +
  theme(legend.position = "bottom", 
        text = element_text(size=8),
        plot.title = element_text(size = rel(0.8)),


      #axis.line = element_line(color = 'black'),
      #axis.text.x=element_blank(),
      plot.background = element_blank(),
      #panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      #axis.text.y=element_blank(),
      panel.border = element_blank())



### Arrange figures into a grid  -------------------------------------------------


# set tight margins so plots are close side-by-side
# Dimensions of each margin: t, r, b, l     (To remember order, think trouble).

# s1_top80 <- s1_top80 + theme(plot.margin=unit(c(-10, -8, -5, 1), "mm"))
# s2_bot20 <- s2_bot20 + theme(plot.margin=unit(c(-10, -2, -5, 15), "mm"))

#grid.arrange(arrangeGrob(t80, b20,ncol=1, nrow=2), heights=c(1,0.04))





t80 <- ggplot_gtable(ggplot_build(t80))
b20 <- ggplot_gtable(ggplot_build(b20))


# set max width
maxWidth = unit.pmax(t80$widths[2:3], b20$widths[2:3])
t80$widths[2:3] <- maxWidth
b20$widths[2:3] <- maxWidth


# # set max width
# maxHeights = unit.pmax(t80$heights[2:3], b20$heights[2:3])
# t80$heights[2:3] <- maxHeights
# b20$heights[2:3] <- maxHeights



#p <- grid.arrange(t80, b20, heights=c(1, 1+(t80_width/p20_width)))#, align = 'v')#, widths=c(1,1.45))
p <- arrangeGrob(t80, b20, heights=c(1, 1+(t80_width/p20_width)))#, align = 'v')#, widths=c(1,1.45))



#p <- plot_grid(t80, b20, ncol = 1, nrow = 2, align = 'v', heights=c(1,1)) 
#p


# save figure to output file ---------------------------------------------------
ggsave("../output/figures/survey_fao_bargraph_sep80_20.png", p, 
       width=190, height=140, dpi=800, units="mm", type = "cairo-png")
dev.off()





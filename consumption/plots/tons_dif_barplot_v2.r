# This script prints a bar graph of the tons different between the 
# hh survey-inferred catch and the 


### Make the plot -------------------------------------------------
barplot_diff <- function(prod_agg_comb, outfile_suffix){
  
  # create plot
  b <-  ggplot(prod_agg_comb, 
               aes(x= factor(country),
                   y= tons_dif*1000,
                   ymin=tons_dif_lwr*1000,
                   ymax=tons_dif_upr*1000,
                   fill = confidence_lvl)) +  # continent 
    
    # add to background
    geom_hline(yintercept = 0, colour= 'grey45') +
    
    # add bars and errorbars
    # Note: errorbars representing the F/M uncertainty are not appropriate for 
    # catch comparisons including all products because all products are includuded!
    # (uncertainty for the FWEAs should be included however.
    geom_bar(stat='identity', width=0.77, position='dodge') + 
    #geom_errorbar(position='dodge', width=0.77, size=0.3)
    
    # set coordinate limits
    #coord_flip(ylim=c(-10000,100)) + 
    coord_flip()+
    ggtitle('') + xlab("") + ylab("catch difference (thousand tons)") + barplot_catchdiff_theme

  # save figure to output file -----------------------------------
  ggsave(paste("../output/figures/fw_ctch_barplot_",outfile_suffix,".png",sep=''), 
         width=210, height=120, dpi=800, units="mm", type = "cairo-png")
  #dev.off()
}


### filter data for pllot -------------------------------------------

filter_forplot <- function(prod_src_filter){
  
  prod_agg_comb_subset <- prod_agg_comb %>%
    filter(surv_catch > 0,
           !is.na(tons_dif),
           prod_src == prod_src_filter) %>%
    arrange(desc(tons_dif)) %>%
    mutate(country= paste(country, year_start, sep=',')) %>%
    mutate(country= factor(country, levels = country[order(sum_catch_millions)]))
  
  return(prod_agg_comb_subset)
}


### read file -------------------------------------------------------------------
prod_agg_comb <- read.csv('../output/consumption/prod_coded_n_agg/hh_survey_consump_nat_agg_prod_combined_diff.csv',
                          stringsAsFactors=FALSE)



# plot separately
prod_agg_comb_subset <- filter_forplot('M|F/M')
barplot_diff(prod_agg_comb_subset,'mar_high')

prod_agg_comb_subset <- filter_forplot('F|F/M')
barplot_diff(prod_agg_comb_subset,'fw_high')

prod_agg_comb_subset <- filter_forplot('M|F|F/M')
barplot_diff(prod_agg_comb_subset,'all_conflvl')




  
  # Add text
  # geom_text(x=nrow(ca_ctry_jnd_t80)/2,
  #           y=lim[2]*1.1, angle=-90, label = 'First 80%') +
  # 
  # set limits and breaks

  #coord_flip(ylim=c(lim[1], lim[2])) +
  ### using the lim inside coord_flip works with setting the breaks
  ### but it does not remove the geometries that exceed the limits
  # coord_flip(ylim=c(lim[1], lim[2])) +
  #scale_y_continuous(breaks = seq(lim[1], lim[2], by = 250)) +
  
  













# 
# ### prep data for bar plots from the ca_ctry_jnd table  ---------------------
# 
# ca_ctry_jnd_p <-  ca_ctry_jnd %>% 
#   
#   filter(!is.na(prc_dif))%>%# & prc_dif < 1000) %>%
#   arrange(desc(prc_dif)) %>%
#   mutate(Country= paste(Country, year_start, sep=',')) %>%
#   mutate(Country= factor(Country, levels = Country[order(sum_catch)])) %>%
# 
#   # Cacluate difference between substituted survey catch and FAO catch for 2012
#   # sum_catch is the FAO catch for 2012
#   mutate(subst2012_dif_mid = subst_catch_mid-sum_catch,
#          subst2012_dif_lwr = subst_catch_lwr-sum_catch,
#          subst2012_dif_upr = subst_catch_upr-sum_catch) %>%
# 
#   # Set to NA the negative and 0 value substituted catch
#   mutate(subst2012_dif_mid = ifelse((surv_catch-surv_catch_rng) < 0,NA,subst2012_dif_mid),
#          subst2012_dif_lwr = ifelse(subst2012_dif_lwr == subst2012_dif_mid,NA,subst2012_dif_lwr),
#          subst2012_dif_upr = ifelse(subst2012_dif_lwr == subst2012_dif_mid,NA,subst2012_dif_upr)) %>%
#   
#   # remove the upper lower bound for the 0 rng errorbar
#   mutate(prc_dif = ifelse(prc_dif_lwr<0,NA,prc_dif),
#          prc_dif_lwr = ifelse(prc_dif_lwr<0 | surv_catch_rng==0,
#                               NA,prc_dif_lwr),
#          prc_dif_upr = ifelse(prc_dif_lwr<0 | surv_catch_rng==0,
#                               NA,prc_dif_upr))

# Draw plot for top 80% catch ------------------------------------------------------------

ca_ctry_jnd_t80 <-  ca_ctry_jnd_p %>%
        filter(grp == 'First 80%') %>%
        arrange(desc(prc_dif))

t80_width = nrow(ca_ctry_jnd_t80) / 30




# Draw plot for remaining 20% catch ------------------------------------------------------------

ca_ctry_jnd_p20 <-  ca_ctry_jnd_p %>% 
  filter(grp == 'Remaining 20%') %>%
  arrange(desc(prc_dif))

b20_width = nrow(ca_ctry_jnd_p20) / 30



b20 <-  ggplot(ca_ctry_jnd_p20, 
               aes(x= factor(Country),
                   y= subst2012_dif_mid*1000,
                   ymin=subst2012_dif_lwr*1000,
                   ymax=subst2012_dif_upr*1000,
                   fill = continent)) + 
  
  # add line to background
  geom_hline(yintercept =0, colour= 'grey45') +
  
  
  # add bars and errorbars
  geom_bar(stat='identity', width=1.1*b20_width, position='dodge') + 
  geom_errorbar(position='dodge', width=1.1*b20_width, size=0.3)+
  
  # Add text
  geom_text(x=nrow(ca_ctry_jnd_p20)/2, 
            y=lim[2]*1.1, angle=-90, label = 'Remaining 20%') + 
  
  # labels
  ggtitle('') + xlab("") + ylab("Difference between survey and FAO catch (thousand tons)") + 
  
  # set limits and breaks
  #coord_flip(ylim(lim[1], lim[2])) +
  ### using the lim inside coord_flip works with setting the breaks
  ### but it does not remove the geometries that exceed the limits
  #coord_flip()+
  coord_flip(ylim=c(lim[1], lim[2])) +
  scale_y_continuous(breaks = c(seq(lim[1], lim[2], by = 250))) +
  
  
  theme_bw() +
  theme(#legend.position = "bottom",
    legend.position = c(.02, -.11),
    legend.key.size = unit(3, "mm"),
    legend.direction= 'horizontal',
    text = element_text(size=8),
    legend.key = element_rect(colour=NULL),
    legend.title = element_blank(),
    plot.title = element_text(size = rel(0.8)),
    plot.background = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank())



### Arrange figures into a grid  -------------------------------------------------


# set tight margins so plots are close side-by-side
# Dimensions of each margin: t, r, b, l     (To remember order, think trouble).
t80 <- t80 + theme(plot.margin=unit(c(-1, 10, -2, -1), "mm"))
b20 <- b20 + theme(plot.margin=unit(c(-2, 10, 10, -1), "mm"))


# convert plots to ggtable objects
t80 <- ggplot_gtable(ggplot_build(t80))
b20 <- ggplot_gtable(ggplot_build(b20))


# set max width of either plot as the width for both
maxWidth = unit.pmax(t80$widths[2:3], b20$widths[2:3])
t80$widths[2:3] <- maxWidth
b20$widths[2:3] <- maxWidth

t80$layout$clip[t80$layout$name == "panel"] <- "off"
b20$layout$clip[b20$layout$name == "panel"] <- "off"


# arrange plots grob into layout 
p <- arrangeGrob(t80, b20, heights=c(0.7, 1.1+(t80_width/b20_width)))


#p <- arrangeGrob(t80, b20)#, heights=c(t80_width, p20_width))#, align = 'v')#, widths=c(1,1.45))
#p <- plot_grid(t80, b20, ncol = 1, nrow = 2, align = 'v', heights=c(1,1)) 




# save figure to output file ---------------------------------------------------
ggsave("../output/figures/survey_fao_tons_dif_bargraph_sep80n20__.png", p, 
       width=190, height=120, dpi=800, units="mm", type = "cairo-png")


dev.off()


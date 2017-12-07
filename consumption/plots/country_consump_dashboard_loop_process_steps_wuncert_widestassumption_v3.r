

### Import modules -------------------------------------------------------------
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(stringr)
options("scipen"=100, "digits"= 8)




### setwd -----------------------------------
setwd("C:/Users/efluet/Dropbox/chap5_global_inland_fish_catch/scripts")




### Read the data consumption table ----------------------------------------------
consump_prods_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods_fwaes_uncert.csv', 
                             stringsAsFactors=F)

consump_nat_agg_df <- read.csv('../output/consumption/hh_survey_consump_nat_agg_prod_fw_uncert_tradecorr_tr_fwae_prepforplotting.csv',
                               stringsAsFactors=F)




### prep product df --------------------------------------------------------------
consump_prods_df <-  consump_prods_df %>% ungroup() %>%  
  
  # select only needed columns before the gather
  select(country, prod_name, prod_src_fm,
         consump_million.tons.yr,
         cons_miltonspyr_fm_min,
         cons_miltonspyr_fm_mean,
         cons_miltonspyr_fm_max,
         cons_miltonspyr_fm_min_fwaes, 
         cons_miltonspyr_fm_mean_fwaes, 
         cons_miltonspyr_fm_max_fwaes) %>%
  
  # replace the codes with text labels for plot
  mutate(prod_src_fm = ifelse(prod_src_fm=='M',  'Marine', prod_src_fm),
         prod_src_fm = ifelse(prod_src_fm=='F',  'Freshwater', prod_src_fm),
         prod_src_fm = ifelse(prod_src_fm=='F/M','Mixed/Unresolved', prod_src_fm)) %>%
  
  # wrap text of labels
  mutate(prod_name= str_wrap(prod_name, width = 24))




# Loop through countries in the column of national consump df ------------------
for (i in consump_nat_agg_df$country) {
  
  
  ### Prepare the products to temporary df ---------------------------------------------------
  # separate edible and fwea consumption, on each their own row per product
  # subset to the single country of the current loop
  temp_consump_prods_df <- consump_prods_df %>% ungroup() %>% filter(country == i) 
  
  col_names <- c('country', 'prod_name', 'prod_src_fm', 'consump_million.tons.yr',
                 'cons_miltonspyr_min',  'cons_miltonspyr_mean', 
                 'cons_miltonspyr_max','step')
  
  
  temp_consump_prods_df_edib <- temp_consump_prods_df %>%
    
    select(country, prod_name, prod_src_fm, consump_million.tons.yr,
           cons_miltonspyr_fm_min_fwaes, 
           cons_miltonspyr_fm_mean_fwaes, 
           cons_miltonspyr_fm_max_fwaes) %>% 
    mutate(step =  'FWAE consumed')
  names(temp_consump_prods_df_edib) <- col_names
  
  
  temp_consump_prods_df_fwae <- temp_consump_prods_df %>%
    select(country, prod_name, prod_src_fm, consump_million.tons.yr,
           cons_miltonspyr_fm_min, cons_miltonspyr_fm_mean, 
           cons_miltonspyr_fm_max) %>% 
    mutate(step =  'Edible quantity consumed')
  names(temp_consump_prods_df_fwae) <- col_names
  
  
  # Remove the values for the whiskers if both are 0 
  # so stacked wiskers dont appear
  temp_consump_prods_df_b2 <- rbind(temp_consump_prods_df_edib, temp_consump_prods_df_fwae) %>%
    
    mutate(cons_miltonspyr_min = ifelse(cons_miltonspyr_max==0, NA, cons_miltonspyr_min),
           cons_miltonspyr_max = ifelse(cons_miltonspyr_max==0, NA, cons_miltonspyr_max)) %>%
    mutate(cons_miltonspyr_min = ifelse(cons_miltonspyr_min==cons_miltonspyr_max, NA, cons_miltonspyr_min),
           cons_miltonspyr_max = ifelse(cons_miltonspyr_min==cons_miltonspyr_max, NA, cons_miltonspyr_max))
  
  
  
  rm(temp_consump_prods_df_edib, temp_consump_prods_df_fwae)
  
  
  
  
  ### Filter nat agg stats df --------------------------------------------------
  # convert df to long format 
  # Select temporary country and rename columns for plot labelling
  temp_consump_nat_agg_df <-  consump_nat_agg_df %>%
    
    # subset data to current country in loop, and select columns
    filter(country == i) %>% # & col_names %in% cols) %>%
    
    # wrap labels over multiple lines
    mutate(cols_wrap = str_wrap(col_names, width = 24)) #%>%
  
  
  
  ### 1 - BAR PLOT -------------------------------------------------------
  # bar plot of products in the country - with added FWEA equivalent
  b1 <- ggplot(temp_consump_prods_df, aes(prod_name, 
                                          consump_million.tons.yr, 
                                          fill= prod_src_fm)) +
    geom_bar(stat='identity') + 
    coord_flip() +
    ggtitle('') + xlab("") + 
    ylab("Per capita average edible quantity consumed yearly (kg/yr)") + 
    theme_minimal(base_size = 9) +
    theme(legend.position=c(0.3, 1.04), 
          legend.title=element_blank(), 
          legend.direction="horizontal",
          legend.key.size = unit(3, "mm"),
          plot.margin = unit(c(5, 5, 5, 5), "mm")) +
    scale_fill_manual(values =  c("Freshwater" = "steelblue2",
                                  "Marine" = "grey65",
                                  'Mixed/Unresolved' = 'aquamarine2'))
  
  b1
  
  ### 2 - BAR PLOT ---------------------------------------------
  # bar graph of country FAO catch , consumption corrected total, import, export
  
  # draw plot object
  b2 <- ggplot(temp_consump_prods_df_b2, 
               aes(x=prod_name, 
                   ymin = cons_miltonspyr_min*1000,
                   y=cons_miltonspyr_mean*1000,
                   ymax = cons_miltonspyr_max*1000, 
                   fill=step)) +
    
    # draw bars
    geom_bar(position=position_dodge(),  stat='identity') +
    
    # draw errorbars
    geom_errorbar(position=position_dodge(width=0.9), colour='black', width=0.4) + 
    
    coord_flip() +
    ggtitle('') + xlab("") + 
    ylab("National quantity consumed yearly (thousand tons/year)") + 
    theme_minimal(base_size = 9) +
    theme(legend.position=c(0.3, 1.04), 
          legend.title=element_blank(), 
          legend.direction="horizontal",
          legend.key.size = unit(3, "mm")) +
    scale_fill_manual(values = c('Edible quantity consumed' = "steelblue2",
                                 'FWAE consumed' = "dodgerblue4"))
  
  
  
  ### BAR PLOT 33333333333333333333333333333333333333333333333333333333333333333
  # bar graph of country FAO catch , consumption corrected total, import, export
  
  
  # declare vector of x axis factors, to set the order in the plot
  positions <- c("Inland Catch (FishStatJ)", "HH Survey consumpion\n(FWEA, trade corr.)",
                 "Freshwater aquaculture\nproduction (FishStatJ)","Freshwater Import\n(FishStatJ)",
                 "Freshwater Export\n(FishStatJ)","HH Survey consumption\n(FWEA)")  
  
  
  
  b3 <- ggplot(data=temp_consump_nat_agg_df, 
               aes(x=cols_wrap,   
                   ymax = max_value*1000, 
                   y=mean_value*1000,
                   ymin = min_value*1000,
                   fill=cols_wrap)) +  
    
    geom_bar(position=position_dodge(),  stat='identity') +
    
    geom_errorbar(position=position_dodge(width=0.9), colour='black', width=0.3) + 
    
    coord_flip() +
    ggtitle('') + xlab("") + 
    ylab("Fish weight (thousand tons/year") + 
    theme_minimal(base_size = 9) +
    theme(legend.position="none") +
    scale_x_discrete(limits = positions) +
    
    scale_fill_manual(values =   
                        c("Inland Catch (FishStatJ)"                        = 'purple2', 
                          "HH Survey consumpion\n(FWEA, trade corr.)"        = 'navyblue',
                          "Freshwater aquaculture\nproduction (FishStatJ)"   = 'coral1',
                          "Freshwater Import\n(FishStatJ)"                   = 'coral1',
                          "Freshwater Export\n(FishStatJ)"                   = 'springgreen4',
                          "HH Survey consumption\n(FWEA)"                    = 'dodgerblue4'))
  
  b3
  
  
  ##############################################################################
  # save figure to output file -------------------------------------------------
  #merge all three plots within one grid (and visualize this)
  
  #grid.arrange(b1, b2, b3, nrow=1, widths=c(5,5,5), top=i)
  
  g <- arrangeGrob(b1, b2, b3, nrow=1, widths=c(5,5,5), 
                   top=textGrob(paste(i,temp_consump_prods_df[1,'year']),
                                gp=gpar(fontsize=20,font=3))) #generates g
  
  
  ### Write figure to file -----------------------------------------------------
  outfile <- paste("../output/figures/country_dashboard/consump_corr_steps_larger_uncert_tr_fwae/",i,"_fw_ctch_consumpsurvey_corr_steps.png",sep="")
  ggsave(file=outfile, g,
         width=260, height=120, dpi=800, units="mm", type = "cairo-png")
  dev.off()
  
}
  

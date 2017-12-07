


### Import modules -------------------------------------------------------------
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(stringr)
options("scipen"=100, "digits"= 8)


### Read the data consumption table ----------------------------------------------
consump_prods_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods.csv', stringsAsFactors = F)
consump_nat_agg_df <- read.csv('../output/consumption/hh_survey_consump_nat_agg_prod.csv')



# Loop through countries --------------------------------------------------------
for (i in consump_nat_agg_df$country) {
  
  
  ### Prepare the products to temporary df ---------------------------------------------------
  temp_consump_prods_df <-  consump_prods_df %>%
    
      ungroup() %>%  # ungroup to prevent problems of filter, after a group_by
      filter(country == i) %>%  # subset to country in the loop
    
      mutate(Freshwater..F..or.Marine..M. = toupper(Freshwater..F..or.Marine..M.)) %>%
    
      mutate(Freshwater..F..or.Marine..M. = ifelse(Freshwater..F..or.Marine..M. == 'M', 
                                                   'Marine', Freshwater..F..or.Marine..M.),
             Freshwater..F..or.Marine..M. = ifelse(Freshwater..F..or.Marine..M. == 'F', 
                                                   'Freshwater', Freshwater..F..or.Marine..M.),
             Freshwater..F..or.Marine..M. = ifelse(Freshwater..F..or.Marine..M. == 'M/F', 
                                                   'Mixed/Unresolved', Freshwater..F..or.Marine..M.),
             Freshwater..F..or.Marine..M. = ifelse(Freshwater..F..or.Marine..M. == 'F/M', 
                                                   'Mixed/Unresolved', Freshwater..F..or.Marine..M.)) %>%
      # wrap text of labels
      mutate(FishType_wrap = str_wrap(FishType, width = 24)) 
  
  
  
  ### Filter nat agg stats df --------------------------------------------------
  # filter to temporary df
  cols <- c('country','sum_export_millions','sum_import_millions',
            'sum_catch_millions','sum_aquacult_millions',
            'consump_million.tons.yr_fwaes','consump_million.tons.yr_fwaes_noimp')
  
  
  temp_consump_nat_agg_df <-  consump_nat_agg_df %>%
                              select(one_of(cols)) %>%
                              gather(country, cols, sum_export_millions:consump_million.tons.yr_fwaes_noimp)
                            
  # rename columns after gather
  names(temp_consump_nat_agg_df) <- c('country', 'col_names', 'million_tons_per_yr')
  
  
  # Select temporary country and rename columns for plot labelling ---------------
  temp_consump_nat_agg_df <- temp_consump_nat_agg_df %>%
    
      filter(country == i & col_names %in% cols) %>%
      
      mutate(col_names = ifelse(col_names=='sum_export_millions','Freshwater Export (FishStatJ)',col_names),
             col_names = ifelse(col_names=='sum_import_millions','Freshwater Import (FishStatJ)',col_names),
             col_names = ifelse(col_names=='sum_catch_millions','Inland Catch (FishStatJ)',col_names),
             col_names = ifelse(col_names=='consump_million.tons.yr_fwaes','HH Survey consumption (FWEA)',col_names),
             col_names = ifelse(col_names=='consump_million.tons.yr_fwaes_noimp','HH Survey consumpion (FWEA, trade corr.)',col_names),
             col_names = ifelse(col_names=='sum_aquacult_millions','Freshwater aquaculture production (FishStatJ)',col_names)) %>%
      
      mutate(cols_wrap = str_wrap(col_names, width = 24)) %>%
      
      mutate(thousand_tons_per_yr = as.numeric(million_tons_per_yr) * 1000)


  ### 11111111111111111111111111111111111111111111111111111111111111111111111111
  ### Bar plot 1 ----------------------------------------------------------------
  # bar plot of products in the country - with added FWEA equivalent
  b1 <- ggplot(temp_consump_prods_df, aes(FishType_wrap, 
                                          Averageediblequantityconsumedgpersonday, 
                                          fill= Freshwater..F..or.Marine..M.)) +
    geom_bar(stat='identity') + 
    coord_flip() +
    ggtitle('') + xlab("") + 
    ylab("Per capita average edible quantity consumed daily (g/day)") + 
    theme_minimal(base_size = 9) +
    theme(legend.position=c(0.3, 1.04), 
          legend.title=element_blank(), 
          legend.direction="horizontal",
          legend.key.size = unit(3, "mm")) +
    scale_fill_manual(values =  c("Freshwater" = "steelblue2",
                                  "Marine" = "grey65",
                                  'Mixed/Unresolved' = 'aquamarine2',
                                  '?' = "grey65"))
  
  
  ### 222222222222222222222222222222222222222222222222222222222222222222222222222
  ### Bar plot 2 ---------------------------------------------------------------
  # bar graph of country FAO catch , consumption corrected total, import, export
  temp_consump_prods_df_b2 <- temp_consump_prods_df %>%
          mutate(consump_million.tons.yr=ifelse(Freshwater..F..or.Marine..M. == 'Freshwater',
                                                consump_million.tons.yr, 0),
                 consump_million.tons.yr_fwaes=ifelse(Freshwater..F..or.Marine..M. == 'Freshwater',
                                                      consump_million.tons.yr_fwaes, 0)) %>%
          select(country, FishType_wrap, consump_million.tons.yr, consump_million.tons.yr_fwaes) %>%
          gather(step, consump, consump_million.tons.yr:consump_million.tons.yr_fwaes) %>%
          mutate(step=ifelse(step=='consump_million.tons.yr','Total edible quantity consumed',step),
                 step=ifelse(step=='consump_million.tons.yr_fwaes','Total FWAE consumed',step))
  

  b2 <- ggplot(temp_consump_prods_df_b2, aes(FishType_wrap, 
                                             consump*1000)) +
    geom_bar(aes(fill = step), position = "dodge", stat='identity') + 
    coord_flip() +
    ggtitle('') + xlab("") + 
    ylab("National quantity consumed yearly (thousand tons/year)") + 
    theme_minimal(base_size = 9) +
    theme(legend.position=c(0.3, 1.04), 
          legend.title=element_blank(), 
          legend.direction="horizontal",
          legend.key.size = unit(3, "mm")) +
    scale_fill_manual(values = c('Total edible quantity consumed' = "steelblue2",
                                 'Total FWAE consumed' = "dodgerblue4"))
    
  

  ### 33333333333333333333333333333333333333333333333333333333333333333333333333
  ### Bar plot 3 ---------------------------------------------------------------
  # bar graph of country FAO catch , consumption corrected total, import, export
  positions <- c("Inland Catch\n(FishStatJ)", "HH Survey\nconsumpion (FWEA,\ntrade corr.)",
                 "Freshwater\naquaculture\nproduction\n(FishStatJ)","Freshwater Import\n(FishStatJ)",
                 "Freshwater Export\n(FishStatJ)","HH Survey\nconsumption (FWEA)")  
  
    
  
  
  b3 <- ggplot(temp_consump_nat_agg_df, aes(cols_wrap, thousand_tons_per_yr)) +
    geom_bar(aes(fill= cols_wrap),  stat='identity') +   #  
    coord_flip() +
    ggtitle('') + xlab("") + 
    ylab("Fish weight (thousand tons/year") + 
    theme_minimal(base_size = 9) +
    theme(legend.position="none") +
    #scale_fill_brewer(palette = "Set1") + 
    scale_x_discrete(limits = positions) +
    scale_fill_manual(values =   c("Inland Catch\n(FishStatJ)"                        = 'purple2', 
                                   "HH Survey\nconsumpion (FWEA,\ntrade corr.)"       = 'navyblue',
                                   "Freshwater\naquaculture\nproduction\n(FishStatJ)" = 'coral1',
                                   "Freshwater Import\n(FishStatJ)"                   = 'coral1',
                                   "Freshwater Export\n(FishStatJ)"                   = 'darkolivegreen1',
                                   "HH Survey\nconsumption (FWEA)"                    = 'dodgerblue4'))

  
  ##############################################################################
  # save figure to output file -------------------------------------------------
  #merge all three plots within one grid (and visualize this)
  
  #grid.arrange(b1, b2, b3, nrow=1, widths=c(5,5,5), top=i)
  
  g <- arrangeGrob(b1, b2, b3, nrow=1, widths=c(5,5,5), 
                   top=textGrob(i,gp=gpar(fontsize=20,font=3))) #generates g

  
  ### Write figure to file -----------------------------------------------------
  outfile <- paste("../output/figures/consump_corr_steps/",i,"_fw_ctch_consumpsurvey_corr_steps.png",sep="")
  ggsave(file=outfile, g,
         width=260, height=120, dpi=800, units="mm", type = "cairo-png")
  dev.off()
  
}
  


#---------------------------
# p <- plot_grid(b1, b2, b3, ncol = 3, nrow = 1, align = 'h') +
#   annotate("text",x=0.5,y=0.98,size=7,label=i)
# p


# gb1 <- ggplotGrob(b1)
# gb2 <- ggplotGrob(b2)
# gb3 <- ggplotGrob(b3)
# grid::grid.newpage()
# p <- grid::grid.draw(cbind(gb1, gb2, gb3))
# #maxWidth = grid::unit.pmax(gb1$widths[2:5], gb2$widths[2:5], gb3$widths[2:5])
# maxHeights = grid::unit.pmax(gb1$heights[2:5], gb2$heights[2:5], gb3$heights[2:5])
# gb1$heights[2:5] <- as.list(maxHeights)
# gb2$heights[2:5] <- as.list(maxHeights)
# gb3$heights[2:5] <- as.list(maxHeights)
# grid.arrange(gb1, gb2, gb3, ncol=3)



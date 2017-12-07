


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



# Loop through countries in the column of national consump df ------------------
for (i in consump_nat_agg_df$country) {
  
  
  ### Prepare the products to temporary df ---------------------------------------------------
  temp_consump_prods_df <-  consump_prods_df %>% ungroup() %>%  
    
      # subset to the single country of the current loop
      filter(country == i) %>%  
  
      # replace the codes with text labels for plot
      mutate( code_fm = ifelse(code_fm == 'M', 'Marine', code_fm),
              code_fm = ifelse(code_fm == 'F',   'Freshwater', code_fm),
              code_fm = ifelse(code_fm == 'F/M', 'Mixed/Unresolved', code_fm)) %>%
        
      # wrap text of labels
      mutate(FishType_wrap = str_wrap(FishType, width = 24)) 
  
  
  
  # -----------------------------------------------------------------------------
  # prepare fish product df for plot b2 
  temp_consump_prods_df_b2 <- temp_consump_prods_df %>%
    
    # convert non-freshwater products consumption to zero 
    mutate(consump_million.tons.yr_mf50=ifelse(code_fm == 'Marine', 0, consump_million.tons.yr_mf50),
           consump_million.tons.yr_fwaes_mf50=ifelse(code_fm == 'Marine', 0, consump_million.tons.yr_fwaes_mf50)) %>%
    
    # select only needed columns before the gather
    select(country, FishType_wrap, 
           consump_million.tons.yr_mf50, consump_million.tons.yr_fwaes_mf50, 
           consump_million.tons.yr_mf50_rng, consump_million.tons.yr_fwaes_mf50_rng) %>%
    
    # gather - turn to long df format
    gather(step, consump, consump_million.tons.yr_mf50:consump_million.tons.yr_fwaes_mf50) %>%
    
    # rename steps for better plot labelling
    mutate(step=ifelse(step=='consump_million.tons.yr_mf50', 'Edible quantity consumed',step),
           step=ifelse(step=='consump_million.tons.yr_fwaes_mf50', 'FWAE consumed',step)) %>%
    
    # combine the two range columns conditionally on the 
    mutate(rng = ifelse(step=='Edible quantity consumed',
                        consump_million.tons.yr_mf50_rng, 
                        consump_million.tons.yr_fwaes_mf50_rng)) %>%
    
    # remove the separate range columns for simplicity
    select(-one_of(c('consump_million.tons.yr_mf50_rng', 
                     'consump_million.tons.yr_fwaes_mf50_rng')))
  
  
  
  
  ### Filter nat agg stats df --------------------------------------------------

  # vector of columns to keep - to filter them before the 'gather'
  cols <- c('country','sum_export_millions','sum_import_millions', 'sum_catch_millions',
            'sum_aquacult_millions','consump_million.tons.yr_fwaes_mf50',
            'consump_million.tons.yr_fwaes_mf50_noimp', 'consump_million.tons.yr_fwaes_mf50_rng')
  
  # convert df to long format 
  # Select temporary country and rename columns for plot labelling ---------------
  temp_consump_nat_agg_df <-  consump_nat_agg_df %>%
                              select(one_of(cols)) %>%
                              gather(col_names, million_tons_per_yr, sum_export_millions:consump_million.tons.yr_fwaes_mf50_noimp) %>%
  
    # subset data to current country in loop, and select columns
    filter(country == i & col_names %in% cols) %>%
    
    # rename columns names for nicer plot labels
    mutate(col_names = ifelse(col_names=='sum_export_millions','Freshwater Export (FishStatJ)',col_names),
           col_names = ifelse(col_names=='sum_import_millions','Freshwater Import (FishStatJ)',col_names),
           col_names = ifelse(col_names=='sum_catch_millions','Inland Catch (FishStatJ)',col_names),
           col_names = ifelse(col_names=='consump_million.tons.yr_fwaes_mf50','HH Survey consumption (FWEA)',col_names),
           col_names = ifelse(col_names=='consump_million.tons.yr_fwaes_mf50_noimp','HH Survey consumpion (FWEA, trade corr.)',col_names),
           col_names = ifelse(col_names=='sum_aquacult_millions','Freshwater aquaculture production (FishStatJ)',col_names)) %>%
    
    # wrap labels over multiple lines
    mutate(cols_wrap = str_wrap(col_names, width = 24)) %>%
  
    # keep the uncertainty range for the HH suvey bars, if not equal to zero
    # THIS SHOULD PROBABLY BE IN THE PREVIOUS DATA FORMATTING SCRIPT
    mutate(consump_million.tons.yr_fwaes_mf50_rng = 
             ifelse(( col_names == 'HH Survey consumption (FWEA)' |
                      col_names == 'HH Survey consumpion (FWEA, trade corr.)') &
                      consump_million.tons.yr_fwaes_mf50_rng != 0,
                      consump_million.tons.yr_fwaes_mf50_rng, NA))
  

  
  ### BAR PLOT 11111111111111111111111111111111111111111111111111111111111111111
  # bar plot of products in the country - with added FWEA equivalent
  b1 <- ggplot(temp_consump_prods_df, aes(FishType_wrap, 
                                          Averageediblequantityconsumedgpersonday/1000*365, 
                                          fill= code_fm)) +
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
  ### BAR PLOT 22222222222222222222222222222222222222222222222222222222222222222
  # bar graph of country FAO catch , consumption corrected total, import, export
  
  # draw plot object
  b2 <- ggplot(temp_consump_prods_df_b2, 
               aes(x=FishType_wrap, 
                   y=consump*1000,
                   ymax = (consump + rng)*1000, 
                   ymin = (consump - rng)*1000,
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
                   y=million_tons_per_yr*1000,
                   ymax = (million_tons_per_yr + consump_million.tons.yr_fwaes_mf50_rng)*1000, 
                   ymin = (million_tons_per_yr - consump_million.tons.yr_fwaes_mf50_rng)*1000,
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

  
  ##############################################################################
  # save figure to output file -------------------------------------------------
  #merge all three plots within one grid (and visualize this)
  
  #grid.arrange(b1, b2, b3, nrow=1, widths=c(5,5,5), top=i)
  
  g <- arrangeGrob(b1, b2, b3, nrow=1, widths=c(5,5,5), 
                   top=textGrob(paste(i,temp_consump_prods_df[1,'year']),
                                gp=gpar(fontsize=20,font=3))) #generates g

  
  ### Write figure to file -----------------------------------------------------
  outfile <- paste("../output/figures/consump_corr_steps_wuncert/",i,"_fw_ctch_consumpsurvey_corr_steps.png",sep="")
  ggsave(file=outfile, g,
         width=260, height=120, dpi=800, units="mm", type = "cairo-png")
  dev.off()
  
}
  

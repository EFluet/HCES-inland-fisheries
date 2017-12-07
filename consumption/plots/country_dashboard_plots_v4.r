source('./consumption/plots/themes/dashboard_plot_theme.r')


### get prod fwae data --------------------------------------------------------
consump_prods_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods_refuse_fwaes.csv',
                             stringsAsFactors=F)


# get fraction inland ---------------------------------------------------------- 
boot_pred <- read.csv('../output/bootpred_inland_consump_fraction_gam_pred.csv')
boot_pred <- boot_pred %>%
  mutate(mean_pred_inland_frac = mean) %>%
  dplyr::select(country_code, mean_pred_inland_frac)



### prep product df --------------------------------------------------------------
consump_prods_df <-  consump_prods_df %>% ungroup() %>%  
  
  filter(!is.na(consump_million.tons.yr)) %>%
  left_join(., boot_pred, by='country_code') %>%
  mutate(country = country.x,
         cons_miltonspyr_fm_min = NA,
         cons_miltonspyr_fm_mean = ifelse(prod_src_fm =='F/M', 
                                          consump_million.tons.yr*mean_pred_inland_frac, consump_million.tons.yr),
         cons_miltonspyr_fm_min_fwaes = consump_million.tons.yr * min_conv_fac,
         cons_miltonspyr_fm_max = NA,
         cons_miltonspyr_fm_mean_fwaes = consump_million.tons.yr * mean_conv_fac,
         cons_miltonspyr_fm_max_fwaes  = consump_million.tons.yr * max_conv_fac) %>%
  
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





### get national consump data --------------------------------------------------
f <- '../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm.csv'
consump_nat_agg_df <- read.csv(f, stringsAsFactors=F)



# declare vector of x axis factors, to set the order in the plot
labels <- c("Inland Catch (FishStatJ)", 
               "HH Survey consumpion\n(FWEA, trade corr.)",
               "Freshwater aquaculture\nproduction (FishStatJ)",
               "Freshwater Import\n(FishStatJ)",
               "Freshwater Export\n(FishStatJ)",
               "HH Survey consumption\n(FWEA)")  


consump_nat_agg_df <- consump_nat_agg_df %>% 
                      mutate(country = country.x) %>%
                      filter(!datatype %in% c("uncert_prodfm_ur", "uncert_fwae_assumed_mc", "uncert_fwae_matched_mc")) %>%

                # rename labels
              mutate(datatype=ifelse(datatype=="sum_catch",       labels[1],datatype),
                     datatype=ifelse(datatype=="tr_corr_consump", labels[2],datatype),
                     datatype=ifelse(datatype=="sum_aquacul",     labels[3],datatype),
                     datatype=ifelse(datatype=="import",          labels[4],datatype),
                     datatype=ifelse(datatype=="export",          labels[5],datatype),
                     datatype=ifelse(datatype=="cons_pretrcorr",  labels[6],datatype)) 


### Create pdf file where all figures are saved
pdf("../output/figures/country_dashboard/dashboards_june2017/country_dashboards_june2017.pdf",
    width = 8.5, height = 4.8)


# Loop thru countries in national consump df ------------------
for (i in unique(sort(consump_nat_agg_df$country))) {
  
  ### Prep products to temporary df ---------------------------------------------------
  # separate edible and fwea consumption, on each their own row per product
  # subset to the single country of the current loop
  temp_consump_prods_df <- consump_prods_df %>% ungroup() %>% filter(country == i) 
  
  col_names <- c('country', 'prod_name', 'prod_src_fm', 'consump_million.tons.yr', 
                 'cons_miltonspyr_min',  'cons_miltonspyr_mean', 'cons_miltonspyr_max', 'step')
  
  
  
  
  temp_consump_prods_df_fwae <- temp_consump_prods_df %>%
                                select(country, prod_name, prod_src_fm, 
                                       consump_million.tons.yr,
                                       cons_miltonspyr_fm_min,
                                       cons_miltonspyr_fm_mean,
                                       cons_miltonspyr_fm_max) %>% 
                                mutate(step =  'Inland processed weight')
                              
  # colnames
  names(temp_consump_prods_df_fwae) <- col_names
  
  
  
  
  
  temp_consump_prods_df_edib <- temp_consump_prods_df %>%
    
                              select(country, prod_name, prod_src_fm, 
                                     consump_million.tons.yr,
                                     cons_miltonspyr_fm_min_fwaes, 
                                     cons_miltonspyr_fm_mean_fwaes, 
                                     cons_miltonspyr_fm_max_fwaes) %>%
                              mutate(step =  'Inland live weight')
  
  # colnames
  names(temp_consump_prods_df_edib) <- col_names
  

  
  # Remove the values for the whiskers if both are 0, so stacked wiskers dont appear
  temp_consump_prods_df_b2 <- rbind(temp_consump_prods_df_edib, temp_consump_prods_df_fwae) %>%
    
    mutate(cons_miltonspyr_min = ifelse(cons_miltonspyr_max==0, NA, cons_miltonspyr_min),
           cons_miltonspyr_max = ifelse(cons_miltonspyr_max==0, NA, cons_miltonspyr_max)) %>%
    mutate(cons_miltonspyr_min = ifelse(cons_miltonspyr_min==cons_miltonspyr_max, NA, cons_miltonspyr_min),
           cons_miltonspyr_max = ifelse(cons_miltonspyr_min==cons_miltonspyr_max, NA, cons_miltonspyr_max))
  
  
  
  rm(temp_consump_prods_df_edib, temp_consump_prods_df_fwae)
  
  
  
  
  ### subset nat consump for b3 plot  --------------------------------------------------
  # convert df to long format & select temporary country and rename columns for plot labelling
  temp_consump_nat_agg_df <-  consump_nat_agg_df %>%
    
    # subset data to current country in loop, and select columns
    filter(country == i) %>% # & col_names %in% cols) %>%
    
    # wrap labels over multiple lines
    mutate(cols_wrap = str_wrap(datatype, width = 24))
  
  
  
  ### BAR PLOT 1 -------------------------------------------------------
  
  # bar plot of products in the country - with added FWEA equivalent
  b1 <- ggplot(temp_consump_prods_df, aes(prod_name, 
                                          consump_million.tons.yr, 
                                          fill= prod_src_fm)) +
    geom_bar(stat='identity') + 
    coord_flip() +
    ggtitle('') + xlab("") + 
    ylab("Per capita average edible quantity \n consumed yearly (kg/yr)") + 
    dashboard_plot_theme + 
    theme(legend.position=c(0.4, 1.04), 
          legend.title=element_blank(), 
          legend.direction="horizontal",
          legend.key.size = unit(3, "mm"),
          plot.margin = unit(c(5, 2, 2, 2), "mm")) +
    scale_fill_manual(values =  c("Freshwater" = "steelblue2",
                                  "Marine" = "grey65",
                                  'Mixed/Unresolved' = 'aquamarine2'))
  
  
  ### BAR PLOT 2 ---------------------------------------------
  
  # bar graph of country FAO catch , consumption corrected total, import, export
  b2 <- ggplot(temp_consump_prods_df_b2, 
               aes(x=prod_name, 
                   ymin = cons_miltonspyr_min*1000,
                   y=cons_miltonspyr_mean*1000,
                   ymax = cons_miltonspyr_max*1000, 
                   fill=step)) +
    
    # draw bars
    geom_bar(position=position_dodge(),  stat='identity') +
    
    # draw errorbars
    geom_errorbar(position=position_dodge(width=0.9), colour='black', width=0.5, size=0.3) + 
    
    coord_flip() +
    ggtitle('') + xlab("") + 
    ylab("National yearly consumption \n (x1000 tonnes/year)") + 
    dashboard_plot_theme +
    theme(legend.position=c(0.4, 1.04), 
          legend.title=element_blank(), 
          legend.direction="horizontal",
          legend.key.size = unit(3, "mm"),
          plot.margin = unit(c(5, 2, 2, 2), "mm")) +
    scale_fill_manual(values = c('Inland processed weight' = "steelblue2",
                                 'Inland live weight' = "dodgerblue4"))
  
  
  ### BAR PLOT 3 ---------------------------------------------------------------
  
  # bar graph of country FAO catch , consumption corrected total, import, export
  b3 <- ggplot(data=temp_consump_nat_agg_df, 
               aes(x=cols_wrap,   
                   ymax = p975 *1000, 
                   y    = mean *1000,
                   ymin = p025 *1000,
                   fill=cols_wrap)) +  
    geom_hline(yintercept = 0, size = 0.3, color='grey45') +
      
    geom_bar(position=position_dodge(),  stat='identity') +
  
    geom_errorbar(position=position_dodge(width=0.9), colour='black', width=0.3) + 
    
    coord_flip() +
    ggtitle('') + xlab("") + 
    ylab("Inland fish production/trade \n (x1000 tonnes/year)") + 
    dashboard_plot_theme +
    theme(legend.position="none",
          plot.margin = unit(c(5, 2, 2, 2), "mm")) +
    scale_x_discrete(limits = as.factor(labels)) +
    
    scale_fill_manual(values =   
                        c("Inland Catch (FishStatJ)"                        = 'purple2', 
                          "HH Survey consumpion\n(FWEA, trade corr.)"        = 'navyblue',
                          "Freshwater aquaculture\nproduction (FishStatJ)"   = 'coral1',
                          "Freshwater Import\n(FishStatJ)"                   = 'coral1',
                          "Freshwater Export\n(FishStatJ)"                   = 'springgreen4',
                          "HH Survey consumption\n(FWEA)"                    = 'dodgerblue4'))
  
  
  # arrange plot grid  --------------------------------------------------------
  # merge all three plots within one grid

  g <- arrangeGrob(b1, b2, b3, nrow=1, widths=c(5,5,5), 
                   
                  # add country label
                  top = textGrob(temp_consump_nat_agg_df[1,'country_label'], 
                                x=0.02, hjust=0, #vjust=0.95,
                                gp=gpar(fontsize=16, font=3)))
  #x=1.05, 
  ### Write figure to file -----------------------------------------------------

  
  # # create filename
  # outfile_name <- paste("../output/figures/country_dashboard/dashboards_june2017/", i,
  #                  "_dashboard_june2017.png", sep="")
  # 
  # # save under filename
  # ggsave(file=outfile_name, g,
  #        width=260, height=120, dpi=800, units="mm", type = "cairo-png")
  # dev.off()
  # 
  
  # draw single plot
  grid.draw(g)
  # make newpage
  grid.newpage()


}
dev.off()



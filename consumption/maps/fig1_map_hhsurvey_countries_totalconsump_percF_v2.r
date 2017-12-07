
# Useful source for reprojecting geo spatial data
#http://rpsychologist.com/working-with-shapefiles-projections-and-world-maps-in-ggplot


### Import libraries ----------------------------------------------------------- 
source('./consumption/maps/map_libraries.r')

### get themes for ggplot ---------------------------------------------------- 
source('./consumption/maps/map_theme.r')
source('./consumption/plots/themes/map_inset_theme.r')


# get&prep data --------------------------------------------------------------------


#~~ get country shpfiles --------------------------------------
# read and reproject countries  -  and ticks to  Robinson 
# ogrInfo(".", "ne_110m_admin_0_countries")
countries <- readOGR("../data/gis/nat_earth", "ne_110m_admin_0_countries")
countries_robin <- spTransform(countries, CRS("+proj=robin"))

# read and reproject outside box
bbox <- readOGR("../data/gis/nat_earth", "ne_110m_wgs84_bounding_box") 
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)

rm(countries, bbox, bbox_robin)


#~~ get list of countries w/ surveys  ----------------------------------------
filename<-'../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm.csv'
surv_countries_final <- read.csv(filename, stringsAsFactors=F)

# keep only diff rows from survey df
surv_data_diff <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv')
surv_data_diff <- surv_data_diff %>%
  dplyr::select(country_code, mean_dif) %>%
  distinct()


#~~ get fwae product data  ------------------------------------------------
f <- '../output/consumption/hh_survey_consump_sep_prods_refuse_fwaes_manual_prod_match.csv'
consump_prod_fwaes <- read.csv(f, stringsAsFactors = F)

consump_prod_fwaes<- consump_prod_fwaes %>%
  
                     # 
                     filter(prod_src_fm %in% c('F', 'F/M')) %>%
                     dplyr::select(country_code, prod_name, product, consump_million.tons.yr) %>%
                     mutate(product = ifelse(is.na(product), 'Assumed\nfresh', product)) %>%
  
                     # sum total weight per country
                     group_by(country_code) %>%
                     mutate(sum_weight = sum(consump_million.tons.yr)) %>%
                     ungroup %>%
  
                     # sum weight of each product
                     group_by(country_code, product, sum_weight) %>%
                     dplyr::summarize(prod_weight = sum(consump_million.tons.yr)) %>%
                     ungroup %>%
                     
                     # calculate percentage
                     mutate(proc_perc_weight = prod_weight / sum_weight) %>%
                    
                     # select processing with highest fraction
                     group_by(country_code) %>%
                     filter(proc_perc_weight == max(proc_perc_weight)) %>%
                      
                     mutate(product = ifelse(product %in% c('fresh','fresco','frais'), 'Fresh', product),
                            product = ifelse(product %in% c('smoked','humado','fumé', 
                                                            'dried', 'seco','séché'), 'Dried\nsmoked', product),
                            product = ifelse(product %in% c('frozen'), 'Frozen', product),
                            largest_f_fwae_method = product) %>%
                     ungroup %>%
                     dplyr::select(country_code, largest_f_fwae_method)


#~~ get final estimate of inland production ------------------------------------
consump_nat_agg_df_mcperc_diff<- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv')
consump_nat_agg_df_mcperc_diff <- consump_nat_agg_df_mcperc_diff %>%
                                  dplyr::select(country_code, tr_corr_consump)



#~~ get predicted f:m ratio ----------------------------------------------------
surv_data <- read.csv('../output/consumption/hh_survey_consump_sep_prods_refuse.csv', stringsAsFactors = FALSE)

# run the gam model for inland fraction
boot_pred <- read.csv('../output/bootpred_inland_consump_fraction_gam_pred.csv')
boot_pred <- boot_pred %>%
             mutate(mean_pred_inland_frac = mean) %>%
             dplyr::select(country_code, mean_pred_inland_frac)



#~~ Join it together  ----------------------------------------------------------

surv_data <- surv_data %>%
  
  filter(country_code.x %in% surv_countries_final$country_code) %>%
  mutate(nb_products_f_or_fm = ifelse(prod_src_fm %in% c('F','F/M'),1,0)) %>%
  
  group_by(country_code.x) %>%
  dplyr::summarize(total_consump_kgyr = sum(Averageediblequantityconsumedgpersonday_wrefuse / 1000 * 365),
            nb_product = n(),
            nb_products_f_or_fm = sum(nb_products_f_or_fm)) %>%
  
  left_join(., surv_data_diff, by=c('country_code.x'='country_code')) %>%
  left_join(., boot_pred, by=c('country_code.x'='country_code')) %>%
  left_join(., consump_prod_fwaes, by=c('country_code.x'='country_code')) %>%
  left_join(., consump_nat_agg_df_mcperc_diff, by=c('country_code.x'='country_code')) %>%
  
  filter(tr_corr_consump >0)
  

rm(surv_countries_final, filename)




# cut plotted variables into ranges --------------------------------------------

#~~ Cut survey consumption into bins  ------------------------------------------
my_breaks = c(0, 5, 10, 15, 20, 50)
surv_data$total_consump_kgyr_cut <- cut(surv_data$total_consump_kgyr, breaks = my_breaks,
                                        dig.lab=10)

# replace the categories stings to make them nicer in the legend
surv_data$total_consump_kgyr_cut <- gsub("\\(|\\]", "", surv_data$total_consump_kgyr_cut)
surv_data$total_consump_kgyr_cut <- gsub("\\,", " to ", surv_data$total_consump_kgyr_cut)

surv_data$total_consump_kgyr_cut <- factor(surv_data$total_consump_kgyr_cut, 
                                              levels=c("0 to 5","5 to 10","10 to 15","15 to 20","20 to 50"))
                                                       
# order factor of products
surv_data$largest_f_fwae_method <- factor(surv_data$largest_f_fwae_method, 
                                           levels=c("Fresh","Assumed\nfresh","Dried\nsmoked","Frozen"))




#~~ Cut percentage F into bins -------------------------------------------------
my_breaks = seq(0, 75, 15)
surv_data$mean_pred_inland_frac_cut <- cut(round(surv_data$mean_pred_inland_frac*100-0.5, 0), breaks = my_breaks,
                                        dig.lab=10)

# replace the categories stings to make them nicer in the legend
surv_data$mean_pred_inland_frac_cut <- gsub("\\(|\\]", "", surv_data$mean_pred_inland_frac_cut)
surv_data$mean_pred_inland_frac_cut <- gsub("\\,", " to ", surv_data$mean_pred_inland_frac_cut)

# relabel countries where the estimated F:M is applied to entire consumption


#~~ Cut estimated production into bins  ----------------------------------------

my_breaks = c(0, 100, 200, 300, 500, 2000)
surv_data$tr_corr_consump_cut <- cut(surv_data$tr_corr_consump * 1000, breaks = my_breaks,
                                           dig.lab=10)

# replace the categories stings to make them nicer in the legend
surv_data$tr_corr_consump_cut <- gsub("\\(|\\]", "", surv_data$tr_corr_consump_cut)
surv_data$tr_corr_consump_cut <- gsub("\\,", " to ", surv_data$tr_corr_consump_cut)

# relabel negative catch countries that are excluded
# for symbol as outline



# Join survey data to gis layer ------------------------------------------------

# convert shp to spatial df and add id column
countries_robin_df <- fortify(countries_robin)
countries_robin@data$id <- rownames(countries_robin@data)

# join survey data to attribute table
countries_robin_data <- merge(countries_robin@data, surv_data, 
                        by.x="adm0_a3", by.y='country_code.x',all.x=T, all.y=F)

# merge the attribute table back to spatial df
countries_robin_df <- merge(countries_robin_df, countries_robin_data, by="id")

# select only polygons with survey data
# sort by id, to preven polygon tearing
countries_robin_df_wsurvey <- countries_robin_df %>%
  filter(!is.na(total_consump_kgyr_cut)) %>%
  arrange(id)




# A map -----------------------------------------------------------------
# Map country production per capita 
source('./consumption/maps/indiv_maps_and_insets/consump_map_and_inset.r')

# Combine consump map & inset
g1 = ggplotGrob(inset_barplot_consump)
consump_map_w_inset = map_plot_consump + annotation_custom(grob = g1, 
                                                           xmin= -15 *10^6,
                                                           xmax= -7.3 *10^6,
                                                           ymin= -7.1 *10^6, 
                                                           ymax=  1.2 *10^6)


# B map -----------------------------------------------------------
### Map fraction of inland in consumption 
source('./consumption/maps/indiv_maps_and_insets/inland_fraction_map_and_inset.r')


# Combine nb.prod map & inset 
g2 = ggplotGrob(inset_barplot_inlandf)
inland_map_w_inset = map_plot_inlandf + annotation_custom(grob = g2, 
                                                          xmin= -15 *10^6, 
                                                          xmax= -8 *10^6, 
                                                          ymin= -7.1 *10^6, 
                                                          ymax=  1.2 *10^6)

### C map --------------------------------------------------------------------

# Map processing method
source('./consumption/maps/indiv_maps_and_insets/most_fwae_map_and_inset.r')

# Combine nb.prod map & inset 
g3 = ggplotGrob(inset_barplot_mostfwae)
mostfwae_map_w_inset = map_plot_mostfwae + annotation_custom(grob = g3, 
                                                             xmin= -15 *10^6, 
                                                             xmax= -7.3 *10^6, 
                                                             ymin= -7.1 *10^6, 
                                                             ymax=  1.2 *10^6)

###  D plot ---------------------

# Map processing method   -------------------------------------------------------
source('./consumption/maps/indiv_maps_and_insets/hces_prod_map_and_inset.r')

# Combine nb.prod map & inset 
g4 = ggplotGrob(inset_barplot_hces_prod)
hces_prod_map_w_inset = map_plot_hces_prod + annotation_custom(grob = g4, 
                                                               xmin= -15 *10^6, 
                                                               xmax= -7.3 *10^6, 
                                                               ymin= -7.1 *10^6, 
                                                               ymax=  1.2 *10^6)



### Combine maps and plots -------------------------------------
require("gridExtra")
require("grid")

g1 <- ggplotGrob(consump_map_w_inset)
g2 <- ggplotGrob(inland_map_w_inset)
g3 <- ggplotGrob(mostfwae_map_w_inset)
g4 <- ggplotGrob(hces_prod_map_w_inset)


t1 = arrangeGrob(g1,ncol=1, left = textGrob("A", y = 0.9, x= 1.1, vjust=1, gp=gpar(fontsize=12)))
t2 = arrangeGrob(g2,ncol=1, left = textGrob("B", y = 0.9, x= 1.1, vjust=1, gp=gpar(fontsize=12)))
t3 = arrangeGrob(g3,ncol=1, left = textGrob("C", y = 0.9, x= 1.1, vjust=1, gp=gpar(fontsize=12)))
t4 = arrangeGrob(g4,ncol=1, left = textGrob("D", y = 0.9, x= 1.1, vjust=1, gp=gpar(fontsize=12)))


final = arrangeGrob(t1,t2,t3,t4, ncol=2)
final_plot <- grid.arrange(final)



### Save figure to file --------------------------------------------------------
ggsave('../output/figures/maps/map_hhsurvey_consump_winset_v2_wrefuse_4plots_v6_2cols.pdf',  final_plot,
       width=210, height=110, dpi=800, units="mm")#, type = "cairo-png")
dev.off()

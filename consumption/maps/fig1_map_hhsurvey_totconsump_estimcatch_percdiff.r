
# Useful source for reprojecting geo spatial data
#http://rpsychologist.com/working-with-shapefiles-projections-and-world-maps-in-ggplot


### Import libraries ----------------------------------------------------------- 
source('./consumption/maps/map_libraries.r')

### get country shpfiles --------------------------------------
# read and reproject countries  -  and ticks to  Robinson 
# ogrInfo(".", "ne_110m_admin_0_countries")
countries <- readOGR("../data/gis/nat_earth", "ne_110m_admin_0_countries")
countries_robin <- spTransform(countries, CRS("+proj=robin"))

# read and reproject outside box
bbox <- readOGR("../data/gis/nat_earth", "ne_110m_wgs84_bounding_box") 
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)

rm(countries, bbox, bbox_robin)



# ~~~ get diff survey df -----
surv_data_diff <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv')
surv_data_diff <- surv_data_diff %>%
  mutate(hces_surv_asprcfao = tr_corr_consump/sum_catch *100) %>%
  dplyr::select(country_code, tr_corr_consump, hces_surv_asprcfao) %>%
  distinct()


# ~~~ get nat survey estim of inland production -------------------------------------
# this file includes the consumption
#f <- '../output/consumption/hh_survey_consump_sep_prods_refuse_fwaes_manual_prod_match.csv'
f<-'../output/consumption/hh_survey_consump_sep_prods_refuse.csv'
prod_surv_data <- read.csv(f, stringsAsFactors = FALSE)

sum_prod_surv_data <- prod_surv_data %>%
            filter(country_code %in% surv_data_diff$country_code) %>%
              mutate(nb_products_f_or_fm = ifelse(prod_src_fm %in% c('F','F/M'),1,0)) %>%
              group_by(country_code) %>%
              dplyr::summarize(total_consump_kgyr = sum(Averageediblequantityconsumedgpersonday_wrefuse / 1000 * 365),
                        nb_product = n(),
                        nb_products_f_or_fm = sum(nb_products_f_or_fm))


### Join surv data together ---------------------------------------------------
surv_data<- sum_prod_surv_data %>%
              left_join(., surv_data_diff, 
                        by=c('country_code'='country_code')) 
  #left_join(., consump_nat_agg_df_mcperc_diff, by=c('country_code'='country_code'))
  #filter(tr_corr_consump >0)

rm(surv_countries_final, filename, f, sum_prod_surv_data, surv_data_diff)




### Cut into bins and set factor order ---------------------------------------

# ~~~ cut survey consumption into bins  ------------------
my_breaks = c(0, 5, 10, 15, 20, 50)
surv_data$total_consump_kgyr_cut <- cut(surv_data$total_consump_kgyr, breaks = my_breaks,
                                        dig.lab=10)

# replace the categories stings to make them nicer in the legend
surv_data$total_consump_kgyr_cut <- gsub("\\(|\\]", "", surv_data$total_consump_kgyr_cut)
surv_data$total_consump_kgyr_cut <- gsub("\\,", " to ", surv_data$total_consump_kgyr_cut)

surv_data$total_consump_kgyr_cut <- factor(surv_data$total_consump_kgyr_cut, 
                                              levels=rev(c("0 to 5","5 to 10","10 to 15",
                                                           "15 to 20","20 to 50")))
                                                       
 

# ~~~ Cut hces catch into bins  -------------------------------------------------
my_breaks = c(0, 100, 200, 300, 500, 2000)
surv_data$tr_corr_consump_cut <- cut(surv_data$tr_corr_consump * 1000, breaks = my_breaks,
                                           dig.lab=10)

# replace the categories stings to make them nicer in the legend
surv_data$tr_corr_consump_cut <- gsub("\\(|\\]", "", surv_data$tr_corr_consump_cut)
surv_data$tr_corr_consump_cut <- gsub("\\,", " to ", surv_data$tr_corr_consump_cut)
surv_data <- surv_data %>% mutate(tr_corr_consump_cut = ifelse(is.na(tr_corr_consump_cut), 
                                                               "Negative", tr_corr_consump_cut))


surv_data$tr_corr_consump_cut <- factor(surv_data$tr_corr_consump_cut, 
                                           levels=rev(c("Negative", "0 to 100", "100 to 200", 
                                                        "200 to 300", "300 to 500", "500 to 2000")))




# ~~~ Cut ca diff into bins  --------------------------------------------------
#my_breaks = c(-10000, 0, 50, 75, 125, 150, 200, 400, 500000)
my_breaks = c(-10000, 0, 50, 100, 150, 200, 400, 500000)
surv_data$hces_surv_asprcfao_cut <- cut(surv_data$hces_surv_asprcfao, breaks = my_breaks,
                                   dig.lab=10)

# replace the categories stings to make them nicer in the legend
surv_data$hces_surv_asprcfao_cut <- gsub("\\(|\\]", "", surv_data$hces_surv_asprcfao_cut)
surv_data$hces_surv_asprcfao_cut <- gsub("\\,", " to ", surv_data$hces_surv_asprcfao_cut)
surv_data <- surv_data %>% mutate(hces_surv_asprcfao_cut=ifelse(hces_surv_asprcfao_cut=="400 to 500000",
                                                           "over 400",hces_surv_asprcfao_cut))
surv_data <- surv_data %>% mutate(hces_surv_asprcfao_cut=ifelse(hces_surv_asprcfao_cut=="-10000 to 0",
                                                                "Negative",hces_surv_asprcfao_cut))

# set order
lengend_order <- rev(c("Negative", "0 to 50", "50 to 100", "100 to 150", 
                       "150 to 200","200 to 400", "over 400"))      

surv_data$hces_surv_asprcfao_cut <- factor(surv_data$hces_surv_asprcfao_cut, levels = lengend_order)
levels(surv_data$hces_surv_asprcfao_cut)


### Join survey data to gis layer ----------------------------------------------

# convert shp to spatial df and add id column
countries_robin_df <- fortify(countries_robin)
countries_robin@data$id <- rownames(countries_robin@data)

# join survey data to attribute table
countries_robin_data <- merge(countries_robin@data, surv_data, 
                        by.x="adm0_a3", by.y='country_code',all.x=T, all.y=F)

# merge the attribute table back to spatial df
countries_robin_df <- merge(countries_robin_df, countries_robin_data, by="id")

# select only polygons with survey data
# sort by id, to preven polygon tearing
countries_robin_df_wsurvey <- countries_robin_df %>%
                              filter(!is.na(total_consump_kgyr_cut)) %>%
                              arrange(id)




# Draw maps & insets -----------------------------------------------------------

# ~~~ get plotting themes for ggplot ------------------------------------------- 
source('./consumption/maps/map_theme.r')
source('./consumption/plots/themes/map_inset_theme.r')




# ~~~ A map -----------------------------------------------------------------
# Map country production per capita 
source('./consumption/maps/indiv_maps_and_insets/consump_map_and_inset.r')

# Combine consump map & inset
g1 = ggplotGrob(inset_barplot_consump)
consump_map_w_inset = map_plot_consump + annotation_custom(grob = g1, 
                                                           xmin= -14.7 *10^6,
                                                           xmax= -6.3 *10^6,
                                                           ymin= -7.5 *10^6, 
                                                           ymax=  0.4 *10^6)

# ~~~ D map ---------------------

# Map processing method
source('./consumption/maps/indiv_maps_and_insets/hces_prod_map_and_inset.r')

# Combine nb.prod map & inset 
g4 = ggplotGrob(inset_barplot_hces_prod)
hces_prod_map_w_inset = map_plot_hces_prod + annotation_custom(grob = g4, 
                                                               xmin= -14.7 *10^6,
                                                               xmax= -6.3 *10^6,
                                                               ymin= -7.5 *10^6, 
                                                               ymax=  0.4 *10^6)

# ~~~ E map ---------------------

# Map processing method
source('./consumption/maps/indiv_maps_and_insets/hces_diff_map_and_inset.r')

# Combine nb.prod map & inset 
g5 = ggplotGrob(inset_barplot_hces_diff)
hces_diff_map_w_inset = map_plot_hces_diff + annotation_custom(grob = g5, 
                                                               xmin= -14.7 *10^6,
                                                               xmax= -6.3 *10^6,
                                                               ymin= -7.5 *10^6, 
                                                               ymax=  0.4 *10^6)



### Combine maps and plots -------------------------------------
require("gridExtra")
require("grid")

g1 <- ggplotGrob(consump_map_w_inset)
g4 <- ggplotGrob(hces_prod_map_w_inset)
g5 <- ggplotGrob(hces_diff_map_w_inset)


t1 = arrangeGrob(g1,ncol=1, left = textGrob("A", y = 0.9, x= 1.1, vjust=1, gp=gpar(fontsize=12)))
t4 = arrangeGrob(g4,ncol=1, left = textGrob("B", y = 0.9, x= 1.1, vjust=1, gp=gpar(fontsize=12)))
t5 = arrangeGrob(g5,ncol=1, left = textGrob("C", y = 0.9, x= 1.1, vjust=1, gp=gpar(fontsize=12)))


final = arrangeGrob(t1,t4,t5, ncol=1)
final_plot <- grid.arrange(final)



### Save figure to file --------------------------------------------------------
ggsave('../output/figures/fig1_map_consump_prod_diff_1col_v2.pdf',  final_plot,
       width=87, height=140, dpi=800, units="mm")  #, type = "cairo-png")
dev.off()

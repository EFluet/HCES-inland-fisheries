
# Useful source for reprojecting
#http://rpsychologist.com/working-with-shapefiles-projections-and-world-maps-in-ggplot


### Import libraries ----------------------------------------------------------- 
source('./consumption/maps/map_libraries.r')

### Create map ggplot theme_opts ---------------------------------------------------- 
source('./consumption/maps/map_theme.r')


source('./consumption/plots/themes/map_inset_theme.r')



### Read and reproject countries and ticks to  Robinson ------------------------
# read and reproject countries
# ogrInfo(".", "ne_110m_admin_0_countries")
countries <- readOGR("../data/gis/nat_earth", "ne_110m_admin_0_countries")
countries_robin <- spTransform(countries, CRS("+proj=robin"))

# read and reproject outside box
bbox <- readOGR("../data/gis/nat_earth", "ne_110m_wgs84_bounding_box") 
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)

rm(countries, bbox, bbox_robin)


### get list of countries finally analyzed ----------------------------------------
filename<-'../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc.csv'
surv_countries_final <- read.csv(filename, stringsAsFactors=F)

# keep only diff rows from survey df
surv_data_diff <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_diff.csv')
surv_data_diff <- surv_data_diff %>%
                  dplyr::select(country_code, mean_dif) %>%
                  distinct()


### Read survey product-level data --------------------------------------------
surv_data <- read.csv('../output/consumption/hh_survey_consump_sep_prods_refuse.csv', stringsAsFactors = FALSE)


surv_data <- surv_data %>%
              filter(country_code.x %in% surv_countries_final$country_code) %>%
              mutate(nb_products_f_or_fm = ifelse(prod_src_fm %in% c('F','F/M'),1,0)) %>%
              group_by(country_code.x) %>%
              summarize(total_consump_kgyr = sum(Averageediblequantityconsumedgpersonday_wrefuse / 1000 * 365),
                       nb_product = n(),
                       nb_products_f_or_fm = sum(nb_products_f_or_fm)) %>%
              left_join(., surv_data_diff, by=c('country_code.x'='country_code'))






rm(surv_countries_final, filename)




### Cut sum_catch into bins  --------------------------------------------------
my_breaks = c(0, 5, 10, 15, 20, 50)
surv_data$total_consump_kgyr_cut <- cut(surv_data$total_consump_kgyr, breaks = my_breaks,
                                                     dig.lab=10)

# replace the categories stings to make them nicer in the legend
surv_data$total_consump_kgyr_cut <- gsub("\\(|\\]", "", surv_data$total_consump_kgyr_cut)
surv_data$total_consump_kgyr_cut <- gsub("\\,", " to ", surv_data$total_consump_kgyr_cut)


### Cut nb_product into bins  -------------------------------------------------
my_breaks = c(0, 5, 10, 15, 20, 80)
surv_data$nb_product_cut <- cut(surv_data$nb_product, breaks = my_breaks,
                                        dig.lab=10)

# replace the categories stings to make them nicer in the legend
surv_data$nb_product_cut <- gsub("\\(|\\]", "", surv_data$nb_product_cut)
surv_data$nb_product_cut <- gsub("\\,", " to ", surv_data$nb_product_cut)



### Join survey data to gis layer ----------------------------------------------

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





### Map country production per capita -------------------------------------------------------
source('./consumption/maps/indiv_maps_and_insets/consump_map_and_inset.r')

# Combine consump map & inset
g2 = ggplotGrob(inset_barplot_consump)
consump_map_w_inset = map_plot_consump + annotation_custom(grob = g2, 
                                                           xmin= -16 *10^6,
                                                           xmax= -7 *10^6,
                                                           ymin= -7.5 *10^6, 
                                                           ymax=  1.5 *10^6)


### Map nb of products per survey -------------------------------------------------------
source('./consumption/maps/indiv_maps_and_insets/nb_products_map_and_inset.r')


# Combine nb.prod map & inset 
g3 = ggplotGrob(inset_barplot_nbprod)
nbprod_map_w_inset = map_plot_nbprod + annotation_custom(grob = g3, xmin= -16 *10^6, 
                                     xmax= -7 *10^6, 
                                     ymin= -7.5 *10^6, 
                                     ymax=  1.5 *10^6)






### Combine maps and plots -------------------------------------
require("gridExtra")
require("grid")

g1 <- ggplotGrob(consump_map_w_inset)
g2 <- ggplotGrob(nbprod_map_w_inset)


t1 = arrangeGrob(g1,ncol=1, left = textGrob("A", y = 0.95, vjust=1, gp=gpar(fontsize=16)))
t2 = arrangeGrob(g2,ncol=1, left = textGrob("B", y = 0.95, vjust=1, gp=gpar(fontsize=16)))

final = arrangeGrob(t1,t2, ncol=1)
final_plot <- grid.arrange(final)



### Save figure to file --------------------------------------------------------
ggsave('../output/figures/maps/map_hhsurvey_consump_winset_v2_wrefuse.png',  final_plot,
       width=90, height=100, dpi=800, units="mm", type = "cairo-png")
dev.off()

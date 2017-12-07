

# Useful source for reprojecting
#http://rpsychologist.com/working-with-shapefiles-projections-and-world-maps-in-ggplot


### Import libraries ----------------------------------------------------------- 
source('./consumption/maps/map_libraries.r')

### Create map ggplot theme_opts ---------------------------------------------------- 
source('./consumption/maps/map_theme.r')



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



### Read survey data table --------------------------------------------
surv_data <- read.csv('../output/consumption/hh_survey_consump_nat_agg_prod_fw_mc.csv', stringsAsFactors = FALSE)

surv_data <- read.csv('../output/consumption/hh_survey_consump_sep_prods.csv', stringsAsFactors = FALSE)


surv_means <- surv_data %>% 
             dplyr::select(country_code, datatype, mean) %>%
             filter(datatype %in% c('sum_catch', 'tr_corr_consump')) %>%
             #unite(comb, p025, mean, p975, datatype) %>%
             spread(datatype, mean) 

surv_data <-  surv_data %>% 
              dplyr::select(country_code, datatype, p025, p975) %>%
              filter(datatype %in% c('tr_corr_consump')) %>%
              left_join(., surv_means, by='country_code') %>%
              mutate(mean_dif_perc = (tr_corr_consump-sum_catch)/sum_catch *100,
                     p025_dif_perc = (tr_corr_consump-sum_catch)/sum_catch *100,
                     p975_dif_perc = (tr_corr_consump-sum_catch)/sum_catch *100) %>%
              mutate(prc_categ = ifelse(sum_catch < p025, 
                                        'Survey > FAO', NA),
                     
                     prc_categ = ifelse(p975 < sum_catch , 
                                        'Survey < FAO', prc_categ),
                     
                     prc_categ = ifelse(p025 < sum_catch & sum_catch < p975 & sum_catch < tr_corr_consump, 
                                        'Survey > FAO - within U.I.', prc_categ),
                     
                     prc_categ = ifelse(p025 < sum_catch & sum_catch < p975 & sum_catch > tr_corr_consump, 
                                        'Survey < FAO - within U.I.', prc_categ))


surv_data_sums <- surv_data %>% 
                  dplyr::select(-one_of('country_code', 'datatype')) %>%
                  dplyr::summarise_each(funs(sum))


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
                              filter(!is.na(mean_dif_perc)) %>%
                              arrange(id)




###  Cut sum_catch into bins for representation log scale ----------------------
my_breaks = c(-100, -50, 0, 50, 100, 200, 500, 1000, 100000)
countries_robin_df_wsurvey$mean_dif_perc <- cut(countries_robin_df_wsurvey$mean_dif_perc, breaks = my_breaks,
                                              dig.lab=10)

# replace the categories stings to make them nicer in the legend
countries_robin_df_wsurvey$mean_dif_perc <- gsub("\\(|\\]", "", countries_robin_df_wsurvey$mean_dif_perc)
countries_robin_df_wsurvey$mean_dif_perc <- gsub("\\,", " to ", countries_robin_df_wsurvey$mean_dif_perc)




### Map country production per capita -------------------------------------------------------

ggplot(bbox_robin_df, aes(long, lat)) + 

  # add background country polygons
  geom_polygon(data=countries_robin_df,
               aes(long, lat, group=group),
               fill='grey85') +
  
  # add survey countries
  geom_polygon(data=countries_robin_df_wsurvey, aes(long, lat, group=group, fill= prc_categ), alpha=1) +

  # add country outline
  geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
  geom_path(data=countries_robin_df_wsurvey, aes(long, lat, group=group), color='black', size=0.12) +
  

  coord_equal() +  theme_fig() + 

  #scale_fill_manual(values=c('firebrick1', 'dodgerblue','darkolivegreen2')) +
          #'steelblue1', 'dodgerblue','dodgerblue3','dodgerblue4', 'dodgerblue4','dodgerblue4')) +
  
  scale_fill_manual(values =   
                      c("Survey < FAO"                 = '#3399ff', 
                        "Survey < FAO - within U.I."   = '#b3d9ff',
                        "Survey > FAO"                 = '#ff3333',
                        "Survey > FAO - within U.I."   = '#ffb3b3',
                        "above_rice"     = 'darkseagreen2')) +
  
  guides(fill = guide_legend(nrow = 2)) + 
  theme(legend.position = c(0.5, -0.05),
        legend.background = element_blank())

 

### Save figure to file --------------------------------------------------------
ggsave('../output/figures/maps/map_hhsurvey_perc_dif.png', 
       width=90, height=70, dpi=1000, units="mm", type = "cairo-png")
dev.off()

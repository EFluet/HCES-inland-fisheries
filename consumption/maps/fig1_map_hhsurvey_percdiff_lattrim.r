
# Useful source for reprojecting geo spatial data
#http://rpsychologist.com/working-with-shapefiles-projections-and-world-maps-in-ggplot


### Import libraries ----------------------------------------------------------- 
source('./consumption/maps/map_libraries.r')

# ~~~ get plotting themes for ggplot ------------------------------------------- 
source('./consumption/maps/map_theme.r')
source('./consumption/plots/themes/map_inset_theme.r')



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
sum_prod_surv_data <- read.csv(f, stringsAsFactors = FALSE)


### Join surv data together ---------------------------------------------------
surv_data<- sum_prod_surv_data %>%
              left_join(., surv_data_diff, 
                        by=c('country_code'='country_code')) 

rm(surv_countries_final, filename, f, sum_prod_surv_data, surv_data_diff)





# ~~~ Cut ca diff into bins  --------------------------------------------------
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

surv_data<- surv_data %>% filter(hces_surv_asprcfao_cut != "Negative")


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
                              filter(!is.na(hces_surv_asprcfao_cut)) %>%
                              arrange(id)





# Useful source for reprojecting geo spatial data
#http://rpsychologist.com/working-with-shapefiles-projections-and-world-maps-in-ggplot


### Import libraries ----------------------------------------------------------- 
source('./consumption/maps/map_libraries.r')

### get country shpfiles --------------------------------------
# read and reproject countries  -  and ticks to  Robinson 
# ogrInfo(".", "ne_110m_admin_0_countries")
# countries <- readOGR("../data/gis/nat_earth", "ne_110m_admin_0_countries")
# countries_robin <- spTransform(countries, CRS("+proj=robin"))


# read and reproject 30deg graticules 
grat <- readOGR("../data/gis/nat_earth", "ne_110m_graticules_30") 
grat_robin <- spTransform(grat, CRS("+proj=robin"))  # reproject graticule
grat_robin_df <- fortify(grat_robin)

grat_robin_df <- grat_robin_df %>%
  filter(lat <= 6500000) %>%
  filter(lat >= -6500000) %>%
  arrange(id)



# # read and reproject outside box
bbox <- readOGR("../data/gis/nat_earth", "ne_110m_wgs84_bounding_box")
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)

bbox_robin_df_top <- bbox_robin_df %>%
  filter(lat <= 6500000) %>%
  filter(lat >= -5400000) %>%
  arrange(id)


bbox_robin_df_bot <- bbox_robin_df %>%
                    #filter(lat <= 6500000) %>%
                    filter(lat <= -5000000) %>%
                    arrange(id)


# 
rm(countries, bbox, bbox_robin)



countries_robin_df <- countries_robin_df %>% 
  filter(lat >= -5100000) %>%
  arrange(id)




### Map nb of products per survey -------------------------------------------------------

map_plot_hces_diff <- ggplot(bbox_robin_df, aes(long, lat)) + 
  
  # add background country polygons
  geom_polygon(data=countries_robin_df,
               aes(long, lat, group=group),
               fill='grey90') +
  
  # add survey countries
  geom_polygon(data=countries_robin_df_wsurvey, aes(long, lat, group=group, fill= hces_surv_asprcfao_cut)) +
  
  # add country outline
  geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df_bot, aes(long, lat, group=group), color="black", size=0.2) +
  geom_path(data=bbox_robin_df_top, aes(long, lat, group=group), color="black", size=0.2) +
  
  
  
  geom_path(data=countries_robin_df_wsurvey, aes(long, lat, group=group), color='black', size=0.1) +
  
  #geom_path(data=grat_robin_df, aes(long, lat, group=group), color='black', size=0.11) +
  #geom_path(data=grat_robin_df_bot, aes(long, lat, group=group), color='black', size=0.11) +
  
  theme_fig() +
  #coord_equal() +  
  scale_y_continuous(limits = c(-5100000,6250000)) +
  coord_fixed(ratio = 1 )+ #, xlim =  c(-18*10^6,18*10^6), ylim = c(-6482147,6482147)) +
  
  
  theme(legend.position="none") +
  
  
  scale_fill_manual(values =
                      c("Negative"    = '#999999',
                        "0 to 50"     = '#6666ff',
                        "50 to 100"    = '#ccccff',
                        "100 to 150"  = '#ffcccc',
                        "150 to 200"  = '#ff6666',
                        "200 to 400"  = '#ff0000',
                        "over 400"  = '#990000')) +
  
  theme(plot.margin = unit(c(-10,-3,-12,-3), "mm"))


g5 = ggplotGrob(map_plot_hces_diff)

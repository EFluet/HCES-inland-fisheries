


### Import libraries -----------------------------------------------------------
library(dplyr)
library(tidyr)
# Module that matches and converts country names to codes 
# alternatively, use library(ISOcodes)
library(countrycode)
options("scipen"=100, "digits"=4)
library(rgdal)
library(rgeos)
library(ggplot2)
library(fuzzyjoin)
library(RColorBrewer)
library(scales)



### Read hh survey consumption data --------------------------------------------
consump_subdiv_df <- read.csv('../output/consumption/hh_survey_consump_w_subdiv.csv', 
                              stringsAsFactors = F)




### Read and reproject countries and ticks to  Robinson ------------------------

# read and reproject countries
# ogrInfo(".", "ne_110m_admin_0_countries")
countries <- readOGR(".", "ne_110m_admin_0_countries")
countries_robin <- spTransform(countries, CRS("+proj=robin"))
countries_robin_df <- fortify(countries_robin)


# read and reproject outside box
bbox <- readOGR(".", "ne_110m_wgs84_bounding_box") 
#bbox_df <- fortify(bbox)
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)

rm(countries, bbox, bbox_robin)




### Prep data for the map ------------------------------------------------------

gadm1 <- readOGR("../data/gadm", "gadm28_adm1") 
gadm1 <- spTransform(gadm1, CRS("+proj=robin"))  # reproject bounding box
gadm1_df <- fortify(gadm1)

gadm1@data$id <- rownames(gadm1@data)

gadm1_data <- gadm1@data

consump_subdiv_df <- consump_subdiv_df %>%
  filter(!is.na(country_code))

# Fuzzy join on two attributes
gadm1_data <- stringdist_left_join(gadm1_data, consump_subdiv_df, 
                                   by = c(ISO = "country_code", NAME_1='SubDiv'),
                                   max_dist=8, distance_col  = "distance")



#### Make table of missing subregions, to update the table --------------------
# 
# # Fuzzy join on two attributes
# gadm1_data <- stringdist_left_join(gadm1_data, consump_subdiv_df, 
#                                    by = c(ISO = "country_code", NAME_1='SubDiv'),
#                                    max_dist=8, distance_col  = "distance")
# 
# gadm1_data <- gadm1_data %>%
#               filter(ISO %in% consump_df$country_code) %>%
#               group_by(id, NAME_0, NAME_1) %>%
#               #filter(ISO.distance == 0) %>%
#               top_n(1, desc(NAME_1.distance)) %>%
#               select(id, NAME_0, ISO, country_code, NAME_1,  SubDiv, Region,
#                      ISO.distance, NAME_1.distance) %>%
#               distinct(id)  #somehow a duplicated id slipped in
# 
# # If the wrong country is matched, remove all other attributed from the join.
# gadm1_data[gadm1_data[, 8] != 0, c(4,6,7,8,9)] <- NA
# 
# write.csv(gadm1_data,'../data/gadm/gadm1_data.csv')  
# 




### Clean up the joined data table, removing duplicates ------------------------
gadm1_data <- gadm1_data %>%
              filter(ISO %in% consump_subdiv_df$country_code) %>%
              group_by(id, NAME_0, NAME_1) %>%
              #filter(ISO.distance == 0) %>%
              top_n(1, desc(NAME_1.distance)) %>%
              select(id, NAME_0, ISO, country_code, NAME_1, SubDiv, consump_gppersperday_fwaes,
                     ISO.distance, NAME_1.distance) %>%
              distinct(id) %>%  #somehow a duplicated id slipped in
              mutate(consump_kgpersperyr_fwaes = consump_gppersperday_fwaes/1000*365)

# If the wrong country is matched, remove all other attributed from the join.
gadm1_data[gadm1_data[, 8] != 0, c(4,6,7)] <- NA

# Remvoe duplicated rows, that or some reason are there
gadm1_data <- gadm1_data %>%
              distinct(id, NAME_0, ISO, country_code, NAME_1, SubDiv,
                       consump_gppersperday_fwaes, .keep_all = TRUE) %>%
              filter(!is.na(consump_gppersperday_fwaes))



###  Cut data into bins for representation -------------------------------------
my_breaks = c(0, 0.5, 1, 5, 10, 20, 40, 70, 80)
gadm1_data$consump_kgpersperyr_fwaes_cut <- cut(gadm1_data$consump_kgpersperyr_fwaes, 
                                              breaks = my_breaks,
                                              dig.lab=10)

# replace the categories stings to make them nicer in the legend
gadm1_data$consump_kgpersperyr_fwaes_cut <- gsub("\\(|\\]", "", gadm1_data$consump_kgpersperyr_fwaes_cut)
gadm1_data$consump_kgpersperyr_fwaes_cut <- gsub("\\,", "-", gadm1_data$consump_kgpersperyr_fwaes_cut)



# Join data table with the polygon df ------------------------------------------
gadm1_df <- plyr::join(gadm1_df, gadm1_data, by="id")#, all.x=T, sort = TRUE)

# Filter to keep only countries with data
gadm1_df <- gadm1_df %>%
            filter(id %in% gadm1_data$id,
                   consump_gppersperday_fwaes > 0) %>%
            arrange(id)  # IMPORTANT; SORT THE IDs TO PREVENT POLYGON TEARING 






### Create map ggplot theme_opts ---------------------------------------------------- 
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_rect(fill='white'),
                         plot.background = element_rect(fill='white'),
                         legend.text=element_text(size=6), 
                         legend.title=element_text(size=6),
                         legend.margin = unit(0, "mm"),
                         legend.key.size = unit(3, "mm"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=8)))




### Plot the country map -------------------------------------------------------
library(scales)

ggplot(countries_robin_df, aes(long, lat, group=group)) + 
  
  geom_polygon(data=countries_robin_df, 
               aes(long, lat, group=group), 
               fill='grey80') +
  
  geom_polygon(data=gadm1_df, 
               aes(long, lat, group=group, fill=consump_kgpersperyr_fwaes_cut)) +
  
  geom_path(data=countries_robin_df, 
            aes(long, lat, group=group), 
            color='white', size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group, fill=NULL), 
            color="black", size=0.2) +
  
  coord_equal() + theme_opts + 
  
  scale_fill_brewer(name=expression(italic(tons~~year^{-1})),
                    palette = "Greens",  direction=1) +
  # scale_fill_distiller(palette='Greens',# breaks=pretty_breaks(n = 3),# trans = "log",
  #                      trans = 'reverse', 
  #                      name=expression(italic(log~kg~person^{-1}~year^{-1}))) +
  # 
  # guides(fill = guide_legend(reverse = TRUE)) +
  # 
  #theme(legend.position="bottom", legend.box="horizontal") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=6)) +
  labs(title = "Per capita inland fish consumption  \n (source: Sub-National household surveys, 1992-2014)")



### Save figure to file --------------------------------------------------------
ggsave('../output/figures/maps/global_consump_subnat_categ.png', width=140, height=90, dpi=1000, units="mm")
dev.off()



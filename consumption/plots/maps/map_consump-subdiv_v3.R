
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
library(stringr)

### Read hh survey consumption data --------------------------------------------
consump_subdiv_df <- read.csv('../output/consumption/hh_survey_consump_w_subdiv.csv', 
                              stringsAsFactors = F)

### get country shpfiles --------------------------------------
# read and reproject countries  -  and ticks to  Robinson 
# ogrInfo(".", "ne_110m_admin_0_countries")
countries <- readOGR("../data/gis/nat_earth", "ne_110m_admin_0_countries")
countries_robin <- spTransform(countries, CRS("+proj=robin"))

# read and reproject outside box
bbox <- readOGR("../data/gis/nat_earth", "ne_110m_wgs84_bounding_box") 
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)



# ### Read and reproject countries and ticks to  Robinson ------------------------

# read and reproject countries
# ogrInfo(".", "ne_110m_admin_0_countries")
countries <- readOGR("../data/gis/nat_earth", "ne_110m_admin_0_countries")
countries_robin <- spTransform(countries, CRS("+proj=robin"))
countries_robin_df <- fortify(countries_robin)


# read and reproject outside box
bbox <- readOGR("../data/gis/nat_earth", "ne_110m_wgs84_bounding_box")
#bbox_df <- fortify(bbox)
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)

rm(countries, bbox, bbox_robin)




### Prep data for the map ------------------------------------------------------
gadm1 <- readOGR("../data/gis/gadm", "gadm28_adm1") 
gadm1 <- spTransform(gadm1, CRS("+proj=robin"))  # reproject bounding box
gadm1_df <- fortify(gadm1)

gadm1@data$id <- rownames(gadm1@data)
gadm1_data <- gadm1@data
consump_subdiv_df <- consump_subdiv_df %>% filter(!is.na(country_code))


# Fuzzy join on two attributes
gadm1_data <- stringdist_left_join(gadm1_data, consump_subdiv_df, 
                                   by = c(ISO = "country_code", NAME_1='SubDiv'),
                                   max_dist=8, distance_col  = "distance")





### Clean up the joined data table, removing duplicates ------------------------
gadm1_data <- gadm1_data %>%
              filter(ISO %in% consump_subdiv_df$country_code) %>%
              group_by(id, NAME_0, NAME_1) %>%
              #filter(ISO.distance == 0) %>%
              top_n(1, desc(NAME_1.distance)) %>%
              dplyr::select(id, NAME_0, ISO, country_code, NAME_1, SubDiv, consump_gppersperday_fwaes,
                     ISO.distance, NAME_1.distance) %>%
              ungroup()

gadm1_data <- gadm1_data %>%
              #dplyr::distinct(id) %>%  #somehow a duplicated id slipped in
              dplyr::mutate(consump_kgpersperyr_fwaes = consump_gppersperday_fwaes/1000*365)

# If the wrong country is matched, remove all other attributed from the join.
gadm1_data[gadm1_data[, 8] != 0, c(4,6,7)] <- NA

# Remove duplicated rows, that or some reason are there
gadm1_data <- gadm1_data %>%
              distinct(id, NAME_0, ISO, country_code, NAME_1, SubDiv,
                       consump_gppersperday_fwaes, .keep_all = TRUE) %>%
              filter(!is.na(consump_gppersperday_fwaes))



###  Cut data into bins for representation -------------------------------------
my_breaks = c(0, 1, 5, 10, 20, 40, 80)
gadm1_data$consump_kgpersperyr_fwaes_cut <- cut(gadm1_data$consump_kgpersperyr_fwaes, 
                                              breaks = my_breaks,
                                              dig.lab=10)

# replace the categories stings to make them nicer in the legend
gadm1_data$consump_kgpersperyr_fwaes_cut <- gsub("\\(|\\]", "", gadm1_data$consump_kgpersperyr_fwaes_cut)
gadm1_data$consump_kgpersperyr_fwaes_cut <- gsub("\\,", "-", gadm1_data$consump_kgpersperyr_fwaes_cut)


# Join consump data to  polygon df ------------------------------------------
gadm1_df <- plyr::join(gadm1_df, gadm1_data, by="id")#, all.x=T, sort = TRUE)

# Filter to keep only countries with data
gadm1_df <- gadm1_df %>%
            filter(id %in% gadm1_data$id,
                   consump_gppersperday_fwaes > 0) %>%
            arrange(id)  # IMPORTANT; SORT THE IDs TO PREVENT POLYGON TEARING 


### slect the countries w/ surveys ---------------------------------------------
#countries_robin_df <- fortify(countries_robin)
countries_robin@data$id <- rownames(countries_robin@data)
countries_robin_wsurv <- subset(countries_robin, adm0_a3 %in% gadm1_data$country_code)
countries_robin_wsurv_df <- fortify(countries_robin_wsurv)


### read in theme
source('./maps/old/subnat_map_theme.r')


### Plot the country map -------------------------------------------------------
library(scales)

#ggplot(countries_robin_df, aes(long, lat, group=group)) + 
m <- ggplot() +  
  
  # background grey countries
  geom_polygon(data=countries_robin_df,
               aes(long, lat, group=group), fill='grey80') +
  
  # lines between countries
  geom_path(data=countries_robin_df, 
            aes(long, lat, group=group), color='white', size=0.3) +
  

  # subnational units
  geom_polygon(data=gadm1_df,
               aes(long, lat, group=group, fill=consump_kgpersperyr_fwaes_cut)) +

  # lines between countries w/ surveys
  geom_path(data=countries_robin_wsurv_df, 
            aes(long, lat, group=group), color='black', size=0.3) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group, fill=NULL), 
            color="black", size=0.3) +
  
  coord_equal() + theme_opts + 
  
  scale_fill_brewer(name=expression(italic(kg~~year^{-1})),
                    palette = "Greens",  direction=1) +
  
  theme(legend.text=element_text(size=8), 
        legend.title=element_text(size=8),
        legend.direction = 'horizontal',
        legend.position = 'top',
        plot.margin = unit(c(2,-25,-4,-25), "mm")) +
  
  guides(fill = guide_legend(nrow = 1))
  # labs(title = "Per capita annual fish consumption (kg/year)  \n 
  #      (source: Sub-National household consumption surveys, 1992-2014)")



### Save figure to file --------------------------------------------------------
ggsave('../output/figures/maps/global_consump_subnat_categ_v3.pdf', m,
       width=187, height=100, dpi=600, units="mm")

ggsave('../output/figures/maps/global_consump_subnat_categ_v3.png', m,
       width=187, height=105, dpi=600, units="mm")

dev.off()

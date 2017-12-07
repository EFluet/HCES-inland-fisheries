
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




# Draw maps & insets -----------------------------------------------------------

# ~~~ get plotting themes for ggplot ------------------------------------------- 
source('./consumption/maps/map_theme.r')
source('./consumption/plots/themes/map_inset_theme.r')

# ~~~ E map ---------------------
# Map processing method
source('./consumption/maps/indiv_maps_and_insets/hces_diff_map_and_inset.r')


map_plot_hces_diff <- map_plot_hces_diff + 
  theme(plot.margin = unit(c(2,-2,-2,-2), "mm"))

g5 = ggplotGrob(map_plot_hces_diff)


### Combine maps and plots -------------------------------------
# require("gridExtra")
# require("grid")
#g5 <- ggplotGrob(hces_diff_map_w_inset)

#t5 = arrangeGrob(g5,ncol=1, left = textGrob("C", y = 0.9, x= 1.1, vjust=1, gp=gpar(fontsize=12)))


#final = arrangeGrob(t1,t4,t5, ncol=1)
#final_plot <- grid.arrange(final)

# 
# 
# ### Save figure to file --------------------------------------------------------
# ggsave('../output/figures/fig1_map_consump_prod_diff_1col_v2.pdf',  final_plot,
#        width=87, height=140, dpi=800, units="mm")  #, type = "cairo-png")
# dev.off()

# Description: This script format the consumption table from ADepT
#              Also, joins in population and exports from FAO.



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


### Read HH survey consumption data  -------------------------------------------
consump_df <- read.csv('../output/consumption/Consumption_statistics_of_fish.csv', 
                       stringsAsFactors=FALSE)
# '../output/consumption/Consumption_statistics_of_fish.csv'
# '../output/consumption/Food_item_protein_consumption_by_region.csv'


# remove "." from column headers
colnames(consump_df) <- gsub("[.]", "", colnames(consump_df))

# Reduce number of columns, keep only the useful ones
consump_df <- consump_df %>% 
  dplyr::select(1:5)



### rearrange consumption data from header to columns --------------------------

# relies on the title/header rows to have NA at the 

# get the index of the rows matching the headers and a logical vector of the match.
# get a logic vector of length of rows matching the section title in first column.
na_logic_vec <- is.na(consump_df[,5])
na_idx <- which(na_logic_vec)


for (i in 1:length(na_logic_vec)) {
  
  if (na_logic_vec[i] == TRUE) {
    curr_region <- consump_df[i,"FishType"]
    consump_df[i,"Region"] <- curr_region}
  
  else {consump_df[i,"Region"] <- curr_region}
}


# remove the column rows earlier identified, as the region is now a column
consump_df <- consump_df[-na_idx,]

rm(curr_region, na_idx, na_logic_vec, i)



### Read the surveys from Asia Pacific and add them to the table ---------------
source('./consumption/format_asia_pacific_survey_data.R', print.eval = TRUE)

# Exclude records where fish type is total, to avoid duplication of 
# counts when calculating totals
c <- c %>% filter(Fish.Type != 'Total|total')

names(c) <- names(consump_df)
c$section <- 'Asia Pacific Surveys'

consump_df <- rbind(consump_df, c)

rm(c)



### Make a sheet for FishType encoding by experts  -----------------------------
# 
# consump_df$FishType <- tolower(consump_df$FishType)
# 
# consump_df_forcoding <- consump_df %>%
#   group_by(country,FishType) %>%
#   summarise() %>%
#   dplyr::select(unique.country=country, unique.FishType=FishType)
# 
# 
# write.csv(consump_df_forcoding, '../output/consumption/Consumption_statistics_of_fish_unique_fishtypes_for_coding.csv')
# rm(consump_df_forcoding)



### Convert consumed weights into FWAEs w/ conv.factors ------------------------

library(stringdist)


# read conversion factors
conv_fwae <- read.csv('../data/consumption/FWAE_conversion_factors.csv', 
                      stringsAsFactors=FALSE)


# only keep factors from Hortle report-until a full table is compiled  
conv_fwae <- conv_fwae %>% filter(original_source == 'Hortle et al. 2007')


# Remove word fish from the string to improve match
conv_fwae$product <- gsub('fish|Fish|   ',"", conv_fwae$product)
consump_df$FishType <- gsub('fish|Fish|   ',"", consump_df$FishType)


# Fuzzy closest match betwen fish products 
# currently using 'longest common string' method. More testing needed.
consump_df$conv_fwae_m <- amatch(consump_df$FishType, 
                                 conv_fwae$product, method = c("lcs"), 
                                 maxDist=20)


# pre-assign empty data frame of right size for the matches
fact_df_rpt <- data.frame(matrix(ncol = length(names(conv_fwae)), 
                                 nrow = length(consump_df[,1])))
colnames(fact_df_rpt) <- names(conv_fwae)


for (i in 1:length(consump_df[,1])){
  idx_row_to_add <- consump_df[i, 'conv_fwae_m']
  fact_df_rpt[i,]  <- conv_fwae[idx_row_to_add,]
}


# add the factors for each row to the consump dataframe
consump_df <- cbind(consump_df, fact_df_rpt)
rm(fact_df_rpt, idx_row_to_add, i)


# unamtched produts, assumed to be fresh fish
na_fac_idx <- is.na(consump_df$factor_cons_prod_to_fwae)
consump_df[na_fac_idx, c('factor_cons_prod_to_fwae')] <- 1.25
consump_df[na_fac_idx, c('product')] <- 'no match - assumed fresh fish'


# multiply the consumed quantity
consump_df <- consump_df %>%
  mutate(consump_gppersperday_fwaes =
           Averageediblequantityconsumedgpersonday * factor_cons_prod_to_fwae)


rm(na_fac_idx, conv_fwae)



### Join the fish product coding -----------------------------------------------

prod_codes <- read.csv('../output/consumption/Consumption_statistics_of_fish_unique_fishtypes_with_codes.csv', 
                       stringsAsFactors=FALSE)



# Make assumption that all uncoded fish product is freshwater
prod_codes[is.na('Freshwater..F..or.Marine..M.') , 'Freshwater..F..or.Marine..M.'] <- 'F'

prod_codes <- prod_codes %>%
  mutate(Freshwater..F..or.Marine..M. = ifelse(is.na(Freshwater..F..or.Marine..M.) |
                                                 Freshwater..F..or.Marine..M. == "", 
                                               'F', 
                                               Freshwater..F..or.Marine..M.))


# execute left join (only keep the consump countries)
consump_df <- consump_df %>%
  left_join(prod_codes, by = c("country" = "unique.country",
                               "FishType" = "unique.FishType"))

# remove the code df from environment
rm(prod_codes)



### Subset freshwater capture consumption and aggregate consumption to national -------------------------------
consump_df <- consump_df %>%
  filter(!grepl('National', Region),
         !grepl('Urban', Region),
         !grepl('Rural', Region)) %>%
  group_by(country, Region, year) %>%
  summarise(consump_gppersperday_fwaes = 
              sum(consump_gppersperday_fwaes, na.rm=TRUE))

# export table to move the 
consump_df_foredit <- consump_df %>%
                      select(country, Region) %>%
                      distinct(country, Region)
# write.csv(consump_df_foredit, '../output/consumption/consumption_regions_foredit.csv')



### Split year start-end and convert format to YYYY ----------------------------

# split year along hyphen 
yr_splt <- strsplit(consump_df$year, "-")

# write start and end year to columns
consump_df$year_start <- lapply(yr_splt, function(l) l[[1]])
consump_df$year_end <- lapply(yr_splt, function(l) if (length(l[l]) > 1 ) {l[[2]]} else {NA})



### Add digits to end year to make it full year format YYYY -------------------
# Add digit to end year column depending of if years 1900 or 2000
consump_df$year_end <- lapply(consump_df$year_end, function(l) 
  sub("0.+?|1.+?", paste("20", l, sep=''), l))
consump_df$year_end <- lapply(consump_df$year_end, function(l) 
  sub("9.+?", paste("19", l, sep=''), l))

# convert yrear columns to numeric
consump_df$year_start <- as.numeric(consump_df$year_start)
consump_df$year_end <- as.numeric(consump_df$year_end)


# remove unneeded env. variable
rm(yr_splt)



### keep only the latest year of consumption survey ----------------------------
consump_df <- consump_df %>% 
              group_by(country) %>%
              filter(year == max(year))


# add country code to table
consump_df$country_code <- countrycode(consump_df$country, 
                                       'country.name', 'iso3c', warn = TRUE)



### Remove freshwater fish imports and add Exports -----------------------------
# 
# source('./fishstatj_data/read_nat_fw_trade_data.R', print.eval = TRUE)
# 
# 
# # execute left join (only keep the consump countries)
# consump_df <- consump_df %>%
#   left_join(tr_sum_bycountry, 
#             by = c("country_code" = "country_code",
#                    "year_start" = "year"))
# 
# consump_df <- as.data.frame(consump_df)
# 
# 
# # change the empty rows of import to 0
# consump_df <- consump_df %>%
#   mutate(Import = ifelse(is.na(Import), 0, Import),
#          Export = ifelse(is.na(Export), 0, Export))
# 
# # reread table because the group_by created an error with country.x column
# consump_df <- as.data.frame(consump_df)  
# consump_df <- consump_df %>%
#   mutate(sum_import_millions = Import / 1000000,
#          sum_export_millions = Export / 1000000,
#          consump_fwae_million.tons.yr_noimp = 
#            consump_fwae_million.tons.yr - sum_import_millions + sum_export_millions)
# 
# # delete the trade table from the environment 
# rm(tr_sum_bycountry)



### Join the renamed regions list ------------------------------------------ 

reg <- read.csv('../output/consumption/consumption_regions_foredit_filled.csv', 
                stringsAsFactors=FALSE)

reg <-  reg %>%
        filter(country != '') %>%
        mutate(SubDiv = ifelse(SubDiv=='', Region, SubDiv))
  
consump_subdiv_df <- right_join(consump_df, reg, by=c('country','Region'))

# remove the edit table
rm(reg)



### Read and reproject countries and ticks to  Robinson ------------------------

# read and reproject countries
# ogrInfo(".", "ne_110m_admin_0_countries")
countries <- readOGR(".", "ne_110m_admin_0_countries")
countries_robin <- spTransform(countries, CRS("+proj=robin"))
countries_robin_df <- fortify(countries_robin)

# read and reproject 30deg graticules 
grat <- readOGR(".", "ne_110m_graticules_30") 
#grat_df <- fortify(grat)
grat_robin <- spTransform(grat, CRS("+proj=robin"))  # reproject graticule
grat_robin_df <- fortify(grat_robin)


# read and reproject outside box
bbox <- readOGR(".", "ne_110m_wgs84_bounding_box") 
#bbox_df <- fortify(bbox)
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)

rm(countries, grat, bbox)



### Prep data for the map ------------------------------------------------------

gadm1 <- readOGR("../data/gadm", "gadm28_adm1") 
gadm1 <- spTransform(gadm1, CRS("+proj=robin"))  # reproject bounding box
gadm1_df <- fortify(gadm1)

gadm1@data$id <- rownames(gadm1@data)

gadm1_data <- gadm1@data

# gadm1_data <- merge(gadm1_data, consump_subdiv_df, 
#                     by.x=c('ISO','NAME_1'), by.y=c('country_code','SubDiv'), 
#                     all.x=T, all.y=T)

consump_subdiv_df <- consump_subdiv_df %>%
                      filter(!is.na(country_code))

gadm1_data <- stringdist_left_join(gadm1_data, consump_subdiv_df, 
                                   by = c(ISO = "country_code", NAME_1='SubDiv'),
                                   max_dist=3, distance_col  = "distance")

gadm1_data2 <-   gadm1_data %>%
                group_by(id, NAME_0, NAME_1) %>%
                #filter(ISO.distance == 0) %>%
                top_n(1, desc(NAME_1.distance)) %>%
                select(id, NAME_0, ISO, country_code, NAME_1, SubDiv, 
                       consump_gppersperday_fwaes, ISO.distance, NAME_1.distance) %>%
                distinct(id)  #somehow a duplicated id slipped in

# If the wrong country is matched, remove all other attributed from the join.
gadm1_data[gadm1_data[, 8] != 0, c(4,6,7)] <- NA

# , .keep_all = TRUE

gadm1_data <- gadm1_data %>%
              distinct(id, NAME_0, ISO, country_code, NAME_1, SubDiv,
                       consump_gppersperday_fwaes, .keep_all = TRUE) %>%
              filter(!is.na(consump_gppersperday_fwaes))

#gadm1_data <- subset(gadm1_data, select=-c(ISO.distance, NAME_1.distance))

gadm1_df1 <- plyr::join(gadm1_df, gadm1_data, by="id")#, all.x=T, sort = TRUE)


gadm1_df1 <- subset(gadm1_df1, consump_gppersperday_fwaes > 0) 
gadm1_df1 <-  gadm1_df1 %>%
              arrange(id)

rm(consump_df, consump_df_foredit)




#pooooooooooooooooooooooooooooooooooooooooooooooo
gadm1ooo <- merge(gadm1, gadm1_data2, by="id", all.x=F, all.y=F)
gadm1ooo <- subset(gadm1ooo, consump_gppersperday_fwaes > 0)
gadm1ooo_df <- fortify(gadm1ooo)

gadm1_df@data <- data.frame(gadm1_df@data, gadm1_data[match(gadm1@data[,'id'], gadm1_data[,'id']),])

sp@data =         data.frame(sp@data,      df        [match(sp@data    [,by],           df[,by]),])



### Create map ggplot theme_opts ---------------------------------------------------- 
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_rect(fill='white'),
                         plot.background = element_rect(fill='white'),
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
  
  geom_polygon(data=gadm1_df1, 
               aes(long, lat, group=group, fill=log(consump_gppersperday_fwaes/1000*365),2)) +
  
  geom_path(data=countries_robin_df, 
            aes(long, lat, group=group), 
            color='white', size=0.1) +
  
  # Add graticules
  # geom_path(data=grat_robin, aes(long, lat, group=group, fill=NULL),
  #           linetype="dashed", color="grey50", size=0.1) +
  
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group, fill=NULL), 
            color="black", size=0.2) +
  
  coord_equal() + theme_opts + 
  
  scale_fill_distiller(palette='Greens',# breaks=pretty_breaks(n = 3),# trans = "log",
                       trans = 'reverse', 
                      name=expression(italic(log~kg~person^{-1}~year^{-1}))) +

  guides(fill = guide_legend(reverse = TRUE)) +

  theme(legend.position="bottom", legend.box="horizontal") +
  theme(legend.text=element_text(size=6), legend.title=element_text(size=6)) +
  labs(title = "Per capita inland fish consumption  \n (source: Sub-National household surveys, 1992-2014)")
  


### Save figure to file --------------------------------------------------------
ggsave('../output/figures/global_consump_subnat.png', width=90, height=90, dpi=1000, units="mm")
dev.off()

write.csv()  




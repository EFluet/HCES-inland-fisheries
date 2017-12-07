# Plot timelines the FAOSTAT inland catch from 1950 to today
# Separated by countries, facets of continents.


### Read and format ca table #------------------------------------------------
path <- "../data/catch_yield/FishStatJ/exported_from_fishstatj/global_catch_production_all.csv"
ca <- read.csv(path)
rm(path)



### Clean up table characters and #---------------------------------------------
# remove all '.' and 'X' in the column names 
# Then Remove non numeric flags from cells in the cabase.
colnames(ca) <- gsub("[.]|X","",colnames(ca))
ca <- as.data.frame(apply(ca, 2, function(y) gsub("0 0", "0", y)))
ca <- as.data.frame(apply(ca, 2, function(y) gsub("...", "", y, fixed = TRUE)))
ca <- as.data.frame(apply(ca, 2, function(y) gsub(" F", "", y)))
ca <- as.data.frame(apply(ca, 2, function(y) gsub("-", "", y)))



### Reshape table and complete clean up #---------------------------------------

# reshape table from wide to long format
ca <- ca %>%
  filter(MeasureMeasure == "Quantity (tonnes)") %>% 
  gather(year, catch, 5:ncol(ca)) %>%
  # Filter for empty columns
  filter(FishingareaFAOmajorfishingarea!="") %>%
  mutate(catch = ifelse(catch=='',0,catch))



### Change data type of columns ------------------------------------------------

# convert year and catch column to numeric
ca$year <- as.numeric(as.character(ca$year))
ca$catch <- as.numeric(as.character(ca$catch))

# convert column to character
ca$FishingareaFAOmajorfishingarea <- 
  as.character(ca$FishingareaFAOmajorfishingarea)



### sum production by area, country and year -----------------------------------

# Sum by country and change the 
ca_sum_bycountry <- ca %>%
  mutate(source = ifelse(grepl("Inland waters", FishingareaFAOmajorfishingarea),
                         "Inland", "Marine")) %>%
  
  group_by(CountryCountry, source, year) %>%
  summarise(sum_catch = sum(catch)) %>%
  mutate(type = "Catch") 


# Remove column
ca_sum_bycountry$FishingareaFAOmajorfishingarea <- NULL


# Match country name with code, for later joining
ca_sum_bycountry$country_code <- countrycode(ca_sum_bycountry$CountryCountry, 
                                             'country.name', 'iso3c', warn = TRUE)
ca_sum_bycountry$continent <- countrycode(ca_sum_bycountry$country_code, 
                                          'iso3c', 'continent', warn = TRUE)


ca_sum_bycountry <- ca_sum_bycountry %>%
                    arrange(year) %>%
                    filter(source =='Inland' & 
                           !is.na(sum_catch)) %>%
  
                    mutate(continent=ifelse(is.na(continent),'Europe',continent))









# make plot -----------------------------------------------------

# colourCount = length(unique(ca_sum_bycountry$CountryCountry))
# getPalette = colorRampPalette(brewer.pal(9, "RdYlGn"))



a <- ggplot(ca_sum_bycountry, aes(x=year,
                             y=sum_catch/100000,
                             fill=CountryCountry)) +
  
            geom_area(colour='grey15', size=0.1, stat = "identity", position = 'stack', na.rm=TRUE) + 
            geom_area(colour = 'grey15',  size=0.1, data = transform(ca_sum_bycountry, continent = "World")) +
            
            # scale_fill_manual(values = getPalette(colourCount)) +
  
            coord_cartesian(expand=0) +
  
            ylab('Inland catch (thousand tons)') + xlab('Year') +
          
            facet_wrap(~continent, nrow=2, drop=TRUE, scales = "free_y") +
            theme_bw()+
            theme(legend.position = "none",
                  axis.text.x=element_text(angle=-60, hjust=0, size = 6),
                  axis.text.y=element_text(size = 6),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_rect(colour = "black"),
                  strip.background = element_rect(colour = NA, fill = NA))
a


# save figure to output file ---------------------------------------------------
ggsave("../output/figures/sum_catch_trend_contfacet.png", 
       width=190, height=130, dpi=800, units="mm", type = "cairo-png")
dev.off()





### Import libraries -----------------------------------------------------------
library(dplyr)
library(tidyr)
library(countrycode)
options("scipen"=100, "digits"=4)

# get ggplot2 theme
source('./consumption/plots/themes/custom_catch_trend_line_theme.r')




### read FAO STATJ catch table -------------------------------------------------
source('./fishstatj_data/read_nat_catch_data.R', print.eval=TRUE)
source('./fishstatj_data/read_nat_aquaculture_data.R', print.eval=TRUE)


# remove entire inland catch table
#rm(ca, aq, aq_inl)

# 
# ### sum production by area, country and year -----------------------------------
# aq_sum_bycountry <- aq %>%
#   filter(SpeciesASFISspecies != "Aquatic plants nei") %>%
#   mutate(source = ifelse(EnvironmentEnvironment == "Freshwater" | EnvironmentEnvironment == "Brackishwater" ,
#                          "Inland","Marine")) %>%
#   group_by(source, year) %>%
#   summarize(sumbysource = sum(production)) %>%
#   ungroup %>%
#   mutate(type = "Aquaculture") %>%
#   mutate(type='Cultured') #%>% select(-EnvironmentEnvironment)
# 



aq_sum_bycountry_nsource <- aq_sum_bycountry %>%
                            group_by(source, year) %>%
                            summarize(sumbysource = sum(sum_aquacul)) %>%
                            mutate(type='Cultured')




# sum by country, and FW/MARINE
ca_sum_bycountry_nsource <-  ca_sum_bycountry %>%
                              group_by(source, year) %>%
                              summarize(sumbysource = sum(sum_catch)) %>%
                              mutate(type='Capture')


ca_aq <- rbind(ca_sum_bycountry_nsource, aq_sum_bycountry_nsource) %>%
         mutate(sourcetype = paste(source, type, sep='-'))




ggplot() +
  geom_line(data= ca_aq, 
            aes(x=year, y=sumbysource/10^6, color=sourcetype), size=0.9) +
  ylab("Fish production (million tonnes)") + xlab("Year") +
  custom_catch_trend_line_theme +
  
  geom_text_repel(data = subset(ca_aq, year == 2013),
                  aes(x= year, y= sumbysource/10^6, label = sourcetype, color = sourcetype),
                  size = 3.5,
                  nudge_x = 50,
                  segment.color = NA,
                  box.padding = unit(0.5, 'mm')) +
  
  coord_cartesian(expand=0, xlim= c(1950, 2037), ylim= c(0, 100)) +
  scale_x_continuous(breaks=c(seq(1950, 2010, 10))) +
  
  scale_colour_brewer(palette = "Set2") +

  theme(legend.position = 'none',
        text = element_text(size=10, colour='black'),
        axis.text = element_text(size=10, colour='black'),
        legend.text = element_text(size = 10),
        plot.margin=unit(c(2, 6, 2, 2), "mm"))


### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/global_fish_production.png",
       dpi=800, width=120, height=80, units='mm', type = "cairo-png")

dev.off()







ggplot() +
  geom_line(data= subset(ca_aq, sourcetype == 'Inland-Capture'),
            aes(x=year, y=sumbysource/10^6), size=0.9) +
  ylab("Fish production (million tonnes)") + xlab("Year") +
  custom_catch_trend_line_theme +
  
  # geom_text_repel(data = subset(ca_aq, year == 2013),
  #                 aes(x= year, y= sumbysource/10^6, label = sourcetype, color = sourcetype),
  #                 size = 3.5,
  #                 nudge_x = 50,
  #                 segment.color = NA,
  #                 box.padding = unit(0.5, 'mm')) +
  
  #coord_cartesian(expand=0, xlim= c(1950, 2037), ylim= c(0, 100)) +
  scale_x_continuous(breaks=c(seq(1950, 2010, 10))) +
  
  scale_colour_brewer(palette = "Set2") +
  
  theme(legend.position = 'none',
        text = element_text(size=10, colour='black'),
        axis.text = element_text(size=10, colour='black'),
        legend.text = element_text(size = 10),
        plot.margin=unit(c(2, 6, 2, 2), "mm"))


### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/global_inland_ca_trend.png",
       dpi=800, width=90, height=70, units='mm', type = "cairo-png")

dev.off()












## plot line by continent ------------------------------------------------------
ca_sum_bycountry$continent <- countrycode(ca_sum_bycountry$country_code, 
                                          'iso3c', 'continent', warn = TRUE)


ca_sum_bycountry <- ca_sum_bycountry %>%
  arrange(year) %>%
  filter(source =='Inland' & 
           !is.na(sum_catch)) %>%
  
  mutate(continent=ifelse(is.na(continent),'Europe',continent))



ca_sum_bycontinent <- ca_sum_bycountry %>%
                    group_by(continent, year) %>%
                    summarize(sum_catch = sum(sum_catch)) %>%
                    ungroup


a <- ggplot(ca_sum_bycontinent, 
            aes(x=year, y=sum_catch/10^6, fill=continent)) +
  
  geom_area(colour='grey15', size=0.1, stat = "identity", position = 'stack', na.rm=TRUE) + 
#  geom_area(colour = 'grey15',  size=0.1, data = transform(ca_sum_bycountry, continent = "World")) +
  
  # scale_fill_manual(values = getPalette(colourCount)) +
  
  coord_cartesian(expand=0) +
  
  ylab('Inland catch (million tonnes)') + xlab('Year') +
  
  #facet_wrap(~continent, nrow=2, drop=TRUE, scales = "free_y") +
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


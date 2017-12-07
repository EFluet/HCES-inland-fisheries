
### Import modules -------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggrepel)


### Read the data consumption table ----------------------------------------------

consump_pop_df <- read.csv('../output/consumption/consumption_survey_inland_corrected.csv')


### plot br graph of consump prod and national prod ----------------------------


tiff("../Output/Figures/fw_ctch_consumpsurvey_vs_natcatch.png",
     res=300,width=240,height=280,units='mm')


# Format the data to long format and sort it for plotting
consump_pop_df_forplot <- consump_pop_df %>%
  
  mutate(country = paste(country, year_start, sep=', ')) %>%
  
  dplyr::select(country, 
         consump_fwae_million.tons.yr_noimp, 
         sum_catch_millions) %>%
  
  gather(datasource, 
         consumpt_tonsperyr, 
         consump_fwae_million.tons.yr_noimp:sum_catch_millions) %>%
  
  arrange( desc(datasource), -consumpt_tonsperyr) %>%
  
  mutate(datasource = 
           ifelse(datasource == 'sum_catch_millions', 
                  'National reported inland catch', 
                  'Domestic inland capture fish consumption'))


consump_pop_df_forplot$country <- 
  factor(consump_pop_df_forplot$country,
         levels = consump_pop_df_forplot$country[order(consump_pop_df_forplot$consumpt_tonsperyr)])


myfile %>% mutate(V5 = ifelse(V1 == 1 & V2 != 4, 1, ifelse(V2 == 4 & V3 != 1, 2, 0)))

# Draw the bargraph
c <-  ggplot(consump_pop_df_forplot, 
             aes(x= factor(country), y= consumpt_tonsperyr, fill = datasource)) + 
  geom_bar(stat="identity", position = "dodge") +
  
  coord_flip() +
  ggtitle('Inland catch from official statistics and from \n  
          household consumption surveys (without aquaculture and imports') +
  
  xlab("") + 
  ylab("Catch (million tons)") + 
  theme_minimal() +
  
  theme(legend.position = "bottom", text = element_text(size=8),
        plot.title = element_text(size = rel(0.8)))

c
dev.off()



### Scatterplot of consump VS national catch  ----------------------------------


# create a field of  country and year
consump_pop_df <- consump_pop_df %>%
  mutate(cntry_yr = paste(country, year_start, sep= ','))

# create output file
png("../Output/Figures/fw_ctch_consumpsurvey_vs_natcatch_scatterplot.png",
    res=500,width=90,height=90,units='mm')


# declare plot object
s <- ggplot(consump_pop_df, aes(x = sum_catch_millions, 
                                y = consump_fwae_million.tons.yr_noimp,
                                colour = continent)) +
  
  # add a 1:1 line
  geom_abline(intercept = 0, slope = 1, colour='red') + 
  
  
  geom_point(size = 1.25, shape = 1, stroke = 0.75) +
  # scale_color_manual(name = "",
  #                    values = c("#000099",
  #                               "#FF9933")) +
  # 
  geom_text_repel(aes(label=cntry_yr), 
                  size = 1.3, 
                  colour='gray35', 
                  segment.size = 0.15, 
                  segment.color='gray85',
                  box.padding = unit(0.3, 'lines'),
                  point.padding = unit(0.2, 'lines')) +
  
  geom_abline(intercept = 0, slope = 1, colour='red') + 
  
  xlab("National reported inland catch to FAO (million tons)") +
  ylab("HH survey -derived national consumption of inland fish catch \n (million tons)") +
  
  xlim(0,0.6) + ylim(0,0.6) +
  
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        legend.position = c(.85, .25), 
        text = element_text(size=6),
        legend.key = element_rect(colour = "white"),
        legend.background = element_rect(colour = "gray55", size=0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank())

s
dev.off()

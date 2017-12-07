
### Import modules -------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggrepel)
library(Cairo)
library(cowplot)



### Read the data consumption table ----------------------------------------------
consump_nat_agg_df <- read.csv('../output/consumption/hh_survey_consump_nat_agg_prod.csv')



# Format the data to long format and sort it for plotting ----------------------
consump_nat_agg_df_forplot <- consump_nat_agg_df %>%
  
  mutate(country = paste(country, year_start, sep=', ')) %>%
  
  dplyr::select(country, 
                consump_million.tons.yr_fwaes_mf50_noimp, 
                sum_catch_millions) %>%
  
  gather(datasource, 
         consumpt_tonsperyr, 
         sum_catch_millions:consump_million.tons.yr_fwaes_mf50_noimp) %>%
  
  arrange( desc(datasource), consumpt_tonsperyr) %>%
  
  filter(!is.na(consumpt_tonsperyr)) %>%
  
  # rename catch label
  mutate(datasource = 
           ifelse(datasource == 'sum_catch_millions', 
                  'National reported inland catch', 
                  'Domestic inland capture fish consumption'))



### plot br graph of consump prod and national prod ----------------------------



# Create factor list for ordered bar plot 
consump_nat_agg_df_forplot$country <- factor(consump_nat_agg_df_forplot$country, 
                                             levels=unique(consump_nat_agg_df_forplot$country))



# Draw the bargraph 
b <-  ggplot(consump_nat_agg_df_forplot, 
             aes(x= country, y= consumpt_tonsperyr, fill = datasource)) + 
  geom_bar(stat="identity", position = "dodge") +
  
  coord_flip() +
  ggtitle('Inland catch from official statistics and from \n  
          household consumption surveys (without aquaculture and imports') +
  
  xlab("") + 
  ylab("Catch (million tons)") + 
  theme_minimal() +
  
  theme(legend.position = "bottom", text = element_text(size=8),
        plot.title = element_text(size = rel(0.8)))

b

### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/barplot_survey_vs_natcatch.png",
     dpi=300, width=240, height=280, units='mm', type = "cairo-png")

dev.off()


### Import modules -------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggrepel)
library(Cairo)
library(cowplot)



### Read the data consumption table ----------------------------------------------
consump_nat_agg_df <- read.csv('../output/consumption/hh_survey_consump_nat_agg_prod_fw_uncert_tradecorr_tr_fwae_prepforplotting.csv', stringsAsFactors=F)


# Format the data to long format and sort it for plotting ----------------------
consump_nat_agg_df_forplot <- consump_nat_agg_df %>%
  
  mutate(country = country.x) %>%
  #mutate(country = paste(country, year_start, sep=', ')) %>%
  
  filter(col_names == 'Inland Catch (FishStatJ)' |
         col_names == 'HH Survey consumpion (FWEA, trade corr.)') %>%
  
  mutate(col_names= ifelse(col_names=='HH Survey consumpion (FWEA, trade corr.)','Survey Catch',col_names)) %>%
  
  arrange( col_names, mean_value) #%>%
  



### plot br graph of consump prod and national prod ----------------------------



# Create factor list for ordered bar plot 
consump_nat_agg_df_forplot$country <- factor(consump_nat_agg_df_forplot$country, 
                                             levels=unique(consump_nat_agg_df_forplot$country))


source('./consumption/plots/themes/barplot_catchdiff_theme.r')

# Draw the bargraph 
b <-  ggplot(consump_nat_agg_df_forplot, 
             aes(x= country, 
                 y= mean_value,
                 ymin=min_value,
                 ymax=max_value,
                 fill = col_names)) + 
  

  geom_bar(stat="identity", position=position_dodge(), width=0.7) +
  geom_errorbar(position='dodge', width=0.7, size=0.3, colour='grey10') +
  
  coord_flip() +
  # ggtitle('Inland catch from official statistics and from \n  
  #         household consumption surveys (without aquaculture and imports') +
  
  xlab("") + 
  ylab("Catch (million tons)") + 
  #theme_minimal() +
  barplot_catchdiff_theme +
  scale_fill_grey() +
  # scale_fill_manual(values =   
  #                     c("Inland Catch (FishStatJ)"                        = 'purple2', 
  #                       "HH Survey consumpion\n(FWEA, trade corr.)"        = 'navyblue')
  
  theme(legend.position = "bottom", text = element_text(size=8),
        plot.title = element_text(size = rel(0.8)))

b

### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/barplot_surveycatch_vs_natcatch_fw.png",
       dpi=300, width=210, height=180, units='mm', type = "cairo-png")

#dev.off()

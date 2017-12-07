
setwd("C:/Users/efluet/Dropbox/chap5_global_inland_fish_catch/scripts/")

### Import modules -------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(Cairo)
library(cowplot)



### Read the data consumption table ----------------------------------------------
consump_nat_agg_df <- read.csv('../output/consumption/hh_survey_consump_nat_agg_prod.csv',
                               stringsAsFactors = FALSE)



# Format the data to long format and sort it for plotting ----------------------
consump_nat_agg_df_forplot <- consump_nat_agg_df %>%
  
  # paste the country name and year in single field, for labelling
  mutate(country = paste(country, year_start, sep=', ')) %>%
  
  
  dplyr::select(country, 
                sum_catch_millions,
                consump_million.tons.yr_fwaes_mf50_noimp,
                consump_million.tons.yr_fwaes_mf50_rng) %>%
  
  gather(datasource, 
         consumpt_tonsperyr, 
         sum_catch_millions:consump_million.tons.yr_fwaes_mf50_noimp) %>%
  
  # sort
  arrange( desc(datasource), consumpt_tonsperyr) %>%
  
  # remove NA
  filter(!is.na(consumpt_tonsperyr)) %>%
  
  # rename catch label
  mutate(datasource = 
           ifelse(datasource == 'sum_catch_millions', 
                  'National reported inland catch', 
                  'Domestic inland capture fish consumption'))



### Scatterplot of consump VS national catch  ----------------------------------


consump_nat_agg_df <- consump_nat_agg_df %>%
                      
                      # get 1st element of split into the list
                      mutate(country = unlist(lapply((strsplit(consump_nat_agg_df$country.x,',')), '[[', 1)),
                             
                             # create a field combining country and year for labelling
                             cntry_yr = paste(country, year_start, sep= ',')) %>%
  
                      # remove rows not in a group
                      filter(!is.na(grp))



### Top 80% scatter  plot --------------------------------------------------------
consump_nat_agg_df_f80 <- consump_nat_agg_df %>% filter(grp=='First 80%')

s1_top80 <- ggplot(consump_nat_agg_df_f80, 
                   aes(x = sum_catch_millions * 1000, #convert values to thousand tons
                       y = consump_million.tons.yr_fwaes_mf50_noimp * 1000,
                       ymax = (consump_million.tons.yr_fwaes_mf50_noimp + consump_million.tons.yr_fwaes_mf50_rng)*1000, 
                       ymin = (consump_million.tons.yr_fwaes_mf50_noimp - consump_million.tons.yr_fwaes_mf50_rng)*1000,
                       colour = continent)) +
  
  # add some slope lines 1:1, 1:2, 2:1
  geom_abline(intercept = 0, slope = 2, size=0.15, colour='grey85') + 
  geom_abline(intercept = 0, slope = 1, size=0.65, colour='grey85') + 
  geom_abline(intercept = 0, slope = 0.5, size=0.15, colour='grey85') + 

  # add labels for the ablines
  geom_text(x=550, y=1200, label="2:1", angle=67.5, fontface='italic', size=4, colour='grey85') +
  geom_text(x=1150, y=1200, label="1:1", angle=45, fontface='italic', size=4, colour='grey85') +
  geom_text(x=1150, y=560, label="1:2", angle=22.5, fontface='italic', size=4, colour='grey85') +
  
  geom_point(size = 0.8)+# shape = 1, stroke = 1.25, alpha= 0.8) +
  # scale_color_manual(name = "",values = c("#000099","#FF9933")) +
  
  # draw errorbars
  geom_errorbar(width=0.3, size=0.2) + 
  
  # add labels
  geom_text_repel(aes(label=cntry_yr), 
                  size = 2, 
                  colour='gray15', 
                  segment.size = 0.25, 
                  segment.color='gray55',
                  force = 8,
                  box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines')) +
  
  # set labels and title
  xlab("National reported inland catch to FAO (thousand tons)") +
  ylab("HH survey -derived national consumption \n of inland fish catch  (million tons)") +
  ggtitle("Among top 16 producers accounting \n for 80% of global inland catch") +
  
  # set the plot ratio and limits to 1:2
  #xlim(0, 1200) + ylim(0, 2400) + coord_fixed(ratio = 0.5) +
  xlim(0, 1250) + ylim(0, 1250) + #+ coord_fixed(ratio = 0.5) +
  
  
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        legend.position = c(0.115, .83), 
        text = element_text(size=6),
        legend.key = element_rect(colour = "white"),
        legend.background = element_rect(colour = "black", size=0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank())
  



### Remaining 20% scatterplot --------------------------------------------------
consump_nat_agg_df_b20 <- consump_nat_agg_df %>% filter(grp=='Remaining 20%')


# declare plot object
s2_bot20 <- ggplot(consump_nat_agg_df_b20, 
                   aes(x = sum_catch_millions*1000,  # convert values to thousand tons
                       y = consump_million.tons.yr_fwaes_mf50_noimp *1000,
                       ymax = (consump_million.tons.yr_fwaes_mf50_noimp + consump_million.tons.yr_fwaes_mf50_rng)*1000, 
                       ymin = (consump_million.tons.yr_fwaes_mf50_noimp - consump_million.tons.yr_fwaes_mf50_rng)*1000,
                       colour = continent)) +
  
  # add 1:1, 1:2, 1:3 lines
  geom_abline(intercept = 0, slope = 2, size=0.15, colour='grey85') + 
  geom_abline(intercept = 0, slope = 1, size=0.65, colour='grey85') + 
  geom_abline(intercept = 0, slope = 0.5, size=0.15, colour='grey85') +
  
  # add labels for the lines
  geom_text(x=145, y=290, label="2:1", angle=45, fontface='italic', size=4, colour='grey85') +
  geom_text(x=145, y=155, label="1:1", angle=22.5, fontface='italic', size=4, colour='grey85') +
  geom_text(x=145, y=70, label="1:2", angle=11.25, fontface='italic', size=4, colour='grey85') +
  
  geom_point(size = 0.8)+#, shape = 1, stroke = 1.25, alpha= 0.8) +
  # scale_color_manual(name = "", values = c("#000099", "#FF9933")) +
 
  # draw errorbars
  geom_errorbar(width=0.3, size=0.2) + 
  
  
  geom_text_repel(aes(label=cntry_yr),
                  
                  # subset to only label points bigger than a threshold
                  # to limit clutter on plot - as many points are stacked in bottom left.
                  data = subset(consump_nat_agg_df_b20, 
                                consump_million.tons.yr_fwaes_mf50_noimp > 0.025),
                  size = 2, 
                  colour='gray15',
                  force = 8,
                  segment.size = 0.25, 
                  segment.color='gray55',
                  box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines')) +
  
  # set labels and title
  xlab("National reported inland catch to FAO (thousand tons)") +ylab("") +
  ggtitle("Among smaller producing countries \n within remaining 20%") +
  
  # set axis limits
  #xlim(0, 500) + ylim(0, 1000) + coord_fixed(ratio = 0.5) +
  xlim(0, 150) + ylim(0, 300) + #+ coord_fixed(ratio = 0.5) +
  
  # set theme
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=6),
        #legend.key = element_rect(colour = "white"),
        #legend.background = element_rect(colour = "gray55", size=0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank(),
        legend.position="none")



### save figure to output file -------------------------------------------------

# set tight margins so plots are close side-by-side
# Dimensions of each margin: t, r, b, l     (To remember order, think trouble).

# s1_top80 <- s1_top80 + theme(plot.margin=unit(c(-10, -8, -5, 1), "mm"))
# s2_bot20 <- s2_bot20 + theme(plot.margin=unit(c(-10, -2, -5, 15), "mm"))


p <- plot_grid(s1_top80, s2_bot20, ncol = 2, nrow = 1, align = 'h') 
p


# save figure to output file
ggsave("../output/figures/fw_ctch_consumpsurvey_vs_natcatch_scatterplot_sep8020_werrbars_limited.png", 
       width=190, height=90, dpi=800, units="mm", type = "cairo-png")
dev.off()


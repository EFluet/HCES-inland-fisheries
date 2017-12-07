


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
                consump_million.tons.yr_fwaes_noimp, 
                sum_catch_millions) %>%
  
  gather(datasource, 
         consumpt_tonsperyr, 
         consump_million.tons.yr_fwaes_noimp:sum_catch_millions) %>%
  
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
dev.off()
# tiff("../Output/Figures/fw_ctch_consumpsurvey_vs_natcatch.png",
#      res=300,width=240,height=280,units='mm')



### Scatterplot of consump VS national catch  ----------------------------------

# create a field of  country and year
consump_nat_agg_df <- consump_nat_agg_df %>%
                      mutate(cntry_yr = paste(country, year_start, sep= ',')) %>%
                      filter(!is.na(grp))



### Top 80% scatter  plot --------------------------------------------------------
consump_nat_agg_df_f80 <- consump_nat_agg_df %>% filter(grp=='First 80%')

s1_top80 <- ggplot(consump_nat_agg_df_f80, aes(x = sum_catch_millions * 1000, #convert values to thousand tons 
                                y = consump_million.tons.yr_fwaes_noimp * 1000,
                                colour = continent)) +
  
  # add some slope lines 1:1, 1:2, 2:1
  geom_abline(intercept = 0, slope = 1, size=0.15, colour='grey85') + 
  geom_abline(intercept = 0, slope = 2, size=0.15, colour='grey85') + 
  geom_abline(intercept = 0, slope = 0.5, size=0.15, colour='grey85') + 

  
  geom_text(x=1110, y=2410, label="2:1", angle=67.5, fontface='italic', size=4, colour='grey85') +
  geom_text(x=1110, y=1230, label="1:1", angle=45, fontface='italic', size=4, colour='grey85') +
  geom_text(x=1110, y=630, label="1:2", angle=22.5, fontface='italic', size=4, colour='grey85') +
  
  geom_point(size = 1.25, shape = 1, stroke = 1.25, alpha= 0.8) +
  # scale_color_manual(name = "",values = c("#000099","#FF9933")) +

  geom_text_repel(aes(label=cntry_yr), 
                  size = 2, 
                  colour='gray15', 
                  segment.size = 0.25, 
                  segment.color='gray55',
                  force = 2,
                  box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines')) +
  
  xlab("National reported inland catch to FAO (thousand tons)") +
  ylab("HH survey -derived national consumption of inland fish catch \n (million tons)") +
  ggtitle("Top 16 producers accounting \n for 60% of global inland catch") +
  
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        legend.position = c(0.25, .85), 
        text = element_text(size=6),
        legend.key = element_rect(colour = "white"),
        legend.background = element_rect(colour = "gray55", size=0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank()) +
  
  # set the plot ratio and limits to 1:2
  xlim(0, 1200) + ylim(0, 2400) +
  coord_fixed(ratio = 1)
  #facet_grid(.~grp, scales = "free_x") + coord_equal()
  #facet_wrap(~ grp, ncol=1, scales = "free_x") 



### Remaining 20% scatterplot --------------------------------------------------
consump_nat_agg_df_b20 <- consump_nat_agg_df %>% filter(grp=='Remaining 20%')


# declare plot object
s2_bot20 <- ggplot(consump_nat_agg_df_b20, aes(x = sum_catch_millions*1000,  # convert values to thousand tons 
                                               y = consump_million.tons.yr_fwaes_noimp *1000,
                                               colour = continent)) +
  # add a 1:1 line
  geom_abline(intercept = 0, slope = 1, size=0.15, colour='grey85') + 
  geom_abline(intercept = 0, slope = 2, size=0.15, colour='grey85') + 
  geom_abline(intercept = 0, slope = 0.5, size=0.15, colour='grey85') +
  
  geom_text(x=420, y=240, label="1:2", angle=22.5, fontface='italic', size=4, colour='grey85') +
  geom_text(x=420, y=450, label="1:1", angle=45, fontface='italic', size=4, colour='grey85') +
  geom_text(x=420, y=870, label="2:1", angle=67.5, fontface='italic', size=4, colour='grey85') +
  
  geom_point(size = 1.25, shape = 1, stroke = 1.25, alpha= 0.8) +
  # scale_color_manual(name = "", values = c("#000099", "#FF9933")) +
 
  geom_text_repel(aes(label=cntry_yr),
                  data = subset(consump_nat_agg_df_b20, 
                                consump_million.tons.yr_fwaes_noimp > 0.05),
                  size = 2, 
                  colour='gray15',
                  force = 2,
                  segment.size = 0.25, 
                  segment.color='gray55',
                  box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.2, 'lines')) +
  
  xlab("National reported inland catch to FAO (thousand tons)") +
  ylab("") +
  ggtitle("From remaining 20%") +
  
  xlim(0, 450) + ylim(0, 900) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=6),
        #legend.key = element_rect(colour = "white"),
        #legend.background = element_rect(colour = "gray55", size=0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank(),
        legend.position="none") + 
  coord_fixed(ratio = 1)
  #coord_equal() 
        #facet_grid(.~grp, scales = "free_x")
        #facet_wrap(~ grp, ncol=1, scales = "free_x") 



### save figure to output file -------------------------------------------------
s1_top80 <- s1_top80 + theme(plot.margin=unit(c(0.6, -8, 0.6, 0.6), "mm")) 
s2_bot20 <- s2_bot20 + theme(plot.margin=unit(c(0.6, 0.6, 0.6, -8), "mm")) 


p <- plot_grid(s1_top80, s2_bot20, ncol = 2, nrow = 1, align = 'h') 
p


# save figure to output file
ggsave("../output/figures/fw_ctch_consumpsurvey_vs_natcatch_scatterplot_sep8020.png", 
       width=125, height=110, dpi=800, units="mm", type = "cairo-png")
dev.off()



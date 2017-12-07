
### Map nb of products per survey -------------------------------------------------------

map_plot_hces_diff <- ggplot(bbox_robin_df, aes(long, lat)) + 
  
  # add background country polygons
  geom_polygon(data=countries_robin_df,
               aes(long, lat, group=group),
               fill='grey90') +
  
  # add survey countries
  geom_polygon(data=countries_robin_df_wsurvey, aes(long, lat, group=group, fill= hces_surv_asprcfao_cut)) +
  
  # add country outline
  geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.1) +
  geom_path(data=countries_robin_df_wsurvey, aes(long, lat, group=group), color='black', size=0.11) +
  
  
  theme_fig() +
  coord_equal() +  
  #coord_fixed(ratio = 1, xlim =  c(-18*10^6,18*10^6), ylim = c(-5000000,5000000)) +
              
  theme(legend.position="none") +
  
  
  scale_fill_manual(values =
                      c("Negative"    = '#999999',
                        "0 to 50"     = '#6666ff',
                        "50 to 100"    = '#ccccff',
                        "100 to 150"  = '#ffcccc',
                        "150 to 200"  = '#ff6666',
                        "200 to 400"  = '#ff0000',
                        "over 400"  = '#990000')) +
  
  theme(plot.margin = unit(c(-5,-4,-5,-10), "mm"))




###  Nb prod inset histogram ------------------------------------------------------------
source('./consumption/plots/themes/map_inset_theme.r')

inset_barplot_hces_diff <-    
  ggplot(subset(surv_data)) + 
  geom_histogram(aes(x=hces_surv_asprcfao, fill=hces_surv_asprcfao_cut), breaks= seq(0, 850, 50),  
                 color='black', size=0.1) +
  
  
  # scale_fill_manual(values =
  #                     c("Negative"    = '#999999',
  #                       "0 to 50"     = '#6666ff',
  #                       "50 to 75"    = '#ccccff',
  #                       "75 to 125"   = '#ffff99',
  #                       "125 to 150"  = '#ffcccc',
  #                       "150 to 200"  = '#ff6666',
  #                       "200 to 400"  = '#ff0000',
  #                       "over 400"  = '#990000')) +
  
  scale_fill_manual(values =
                      c("Negative"    = '#999999',
                        "0 to 50"     = '#6666ff',
                        "50 to 100"    = '#ccccff',
                        #"75 to 125"   = '#ffff99',
                        "100 to 150"  = '#ffcccc',
                        "150 to 200"  = '#ff6666',
                        "200 to 400"  = '#ff0000',
                        "over 400"  = '#990000')) +
  
  ylab('Nb. countries') +
  xlab('Survey to reported\npercentage (%)') +
  scale_y_continuous(breaks=pretty_breaks(n=4))+
  coord_flip() +
  barplot_mapinset_theme +
  theme(panel.border = element_blank(),
        legend.position=c(-0.3, 1.4),
        #legend.position="none",
        panel.background = element_rect(fill = "transparent",colour = NA), 
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size=5, colour='black'))


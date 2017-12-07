
### Map nb of products per survey -------------------------------------------------------

map_plot_hces_prod <- ggplot(bbox_robin_df, aes(long, lat)) + 
  
  # add background country polygons
  geom_polygon(data=countries_robin_df,
               aes(long, lat, group=group),
               fill='grey90') +
  
  # add survey countries
  geom_polygon(data=countries_robin_df_wsurvey, aes(long, lat, group=group, fill= tr_corr_consump_cut)) +
  
  # add country outline
  geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.1) +
  geom_path(data=countries_robin_df_wsurvey, aes(long, lat, group=group), color='black', size=0.11) +
  
  
  coord_equal() +  theme_fig() +
  theme(legend.position="none") +
  
  scale_fill_manual(values =
                      c("Negative"    = '#999999',
                        "0 to 100"    = '#e6f5ff',
                        "100 to 200"  = '#99d6ff',
                        "200 to 300"  = '#4db8ff',
                        "300 to 500"  = '#0099ff',
                        "500 to 2000" = '#006bb3')) +
 
  theme(plot.margin = unit(c(-5,-4,-5,-10), "mm"))

  

###  Nb prod inset histogram ------------------------------------------------------------

source('./consumption/plots/themes/map_inset_theme.r')

inset_barplot_hces_prod <-    
  ggplot(surv_data) + 
  geom_histogram(aes(x=tr_corr_consump*1000, fill=tr_corr_consump_cut), breaks= seq(0, 2000, 100),  
                 color='black', size=0.1) +
  
  scale_fill_manual(values =
                      c("Negative"    = '#999999',
                        "0 to 100"    = '#e6f5ff',
                        "100 to 200"  = '#99d6ff',
                        "200 to 300"  = '#4db8ff',
                        "300 to 500"  = '#0099ff',
                        "500 to 2000" = '#006bb3')) +
  
  
  
  ylab('Nb. countries') +
  xlab('Survey production\n(x1000 tonnes/yr)') +
  scale_y_continuous(breaks=pretty_breaks(n=4))+
  coord_flip() +
  barplot_mapinset_theme +
  theme(panel.border = element_blank(),
        legend.position=c(-0.3, 1.4),
        #legend.position="none",
        panel.background = element_rect(fill = "transparent",colour = NA), 
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size=5, colour='black'))


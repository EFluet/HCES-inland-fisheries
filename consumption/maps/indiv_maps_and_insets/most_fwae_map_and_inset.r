
### Map nb of products per survey -------------------------------------------------------

map_plot_mostfwae <- ggplot(bbox_robin_df, aes(long, lat)) + 
  
  # add background country polygons
  geom_polygon(data=countries_robin_df,
               aes(long, lat, group=group),
               fill='grey85') +
  
  # add survey countries
  geom_polygon(data=countries_robin_df_wsurvey, aes(long, lat, group=group, fill= largest_f_fwae_method)) +
  
  # add country outline
  geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.1) +
  geom_path(data=countries_robin_df_wsurvey, aes(long, lat, group=group), color='black', size=0.11) +
  
  
  coord_equal() +  theme_fig() +
  theme(legend.position="none") +
  
  scale_fill_manual(values =
                      c("Frozen"        = '#9933ff',
                        "Dried\nsmoked"  = '#ffcc00',
                        "Fresh"         =  '#00e600',
                        "Assumed\nfresh" =  '#00ff99')) +
  
  theme(plot.margin = unit(c(-5,-3,-5,-7.5), "mm"))

  

###  Nb prod inset histogram ------------------------------------------------------------

source('./consumption/plots/themes/map_inset_theme.r')

inset_barplot_mostfwae <-    
  ggplot(surv_data) + 
  geom_bar(aes(x=largest_f_fwae_method, y=..count.., fill=largest_f_fwae_method),
                 color='black', size=0.1, width=0.4) +
  
  scale_fill_manual(values =
                      c("Frozen"        = '#9933ff',
                        "Dried\nsmoked"  = '#ffcc00',
                        "Fresh"         =  '#00e600',
                        "Assumed\nfresh" =  '#00ff99'),
                    labels=c("Frozen"        = 'Frozen',
                             "Dried\nsmoked"  = 'Dried/Smoked',
                             "Fresh"         =  'Fresh',
                             "Assumed\nfresh" =  'Assumed Fresh')) +
  
  ylab('Nb. countries') +
  xlab('Most important\nprocessing methods') +
  coord_flip() +
  barplot_mapinset_theme +
  scale_y_continuous(breaks=pretty_breaks(n=3))+
  theme(panel.border = element_blank(),
        legend.position=c(-0.7, 1.4),
        #legend.position="none",
        panel.background = element_rect(fill = "transparent",colour = NA), 
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size=5, colour='black'),
        legend.key.size = unit(1.25, "mm")) +
  
  guides(fill = guide_legend(keywidth = unit(1.25, "mm"), 
                             keyheight = unit(1.25, "mm")))



### Map nb of products per survey -------------------------------------------------------

map_plot_inlandf <- ggplot(bbox_robin_df, aes(long, lat)) + 
  
  # add background country polygons
  geom_polygon(data=countries_robin_df,
               aes(long, lat, group=group),
               fill='grey85') +
  
  # add survey countries
  geom_polygon(data=countries_robin_df_wsurvey, aes(long, lat, group=group, fill= mean_pred_inland_frac_cut)) +
  
  # add country outline
  geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.1) +
  geom_path(data=countries_robin_df_wsurvey, aes(long, lat, group=group), color='black', size=0.11) +
  
  
  coord_equal() +  theme_fig() +
  theme(legend.position="none") +
  
  scale_fill_manual(values =
                      c("0 to 15"     = '#ffe6e6',
                        "15 to 30"    = '#ff9999',
                        "30 to 45"   =  '#ff4d4d',
                        "45 to 60"   =  '#ff0000',
                        "60 to 75"   =  '#b30000')) +
  theme(plot.margin = unit(c(-5,-3,-5,-7.5), "mm"))

  

###  Nb prod inset histogram ------------------------------------------------------------

source('./consumption/plots/themes/map_inset_theme.r')

inset_barplot_inlandf <-    ggplot(surv_data) + 
  geom_histogram(aes(x=round(mean_pred_inland_frac*100-0.5, 0), fill=mean_pred_inland_frac_cut),  
                 breaks=seq(0, 75, 7.5), color='black', size=0.1) +
  
  scale_fill_manual(values =
                      c("0 to 15"     = '#ffe6e6',
                        "15 to 30"    = '#ff9999',
                        "30 to 45"   =  '#ff4d4d',
                        "45 to 60"   =  '#ff0000',
                        "60 to 75"   =  '#b30000')) +
  
  ylab('Nb. countries') +
  xlab('Percentage\ninland consumption') +
  coord_flip() +
  barplot_mapinset_theme +
  theme(panel.border = element_blank(),
        legend.position=c(-0.2, 1.4),
        #legend.position="none",
        panel.background = element_rect(fill = "transparent",colour = NA), 
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size=5, colour='black'))


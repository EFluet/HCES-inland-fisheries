

### Map country production per capita -------------------------------------------------------

map_plot_consump <- ggplot(bbox_robin_df, aes(long, lat)) + 
  
  # add background country polygons
  geom_polygon(data=countries_robin_df,
               aes(long, lat, group=group),
               fill='grey90') +
  
  # add survey countries
  geom_polygon(data=countries_robin_df_wsurvey, aes(long, lat, group=group, fill= total_consump_kgyr_cut), alpha=1) +
  
  # add country outline
  geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.1) +
  geom_path(data=countries_robin_df_wsurvey, aes(long, lat, group=group), color='black', size=0.11) +
  
  
  coord_equal() +  theme_fig() +
  theme(legend.position="none") +
  
  scale_fill_manual(values =   
                      c("0 to 5"    = '#d6f5d6',
                        "5 to 10"   = '#85e085',
                        "10 to 15"   = '#33cc33',
                        "15 to 20"   = '#1f7a1f',
                        "20 to 50"   = '#0a290a')) +
  
  theme(plot.margin = unit(c(-5,-4,-5,-10), "mm"))




### Consumption kg/yr  inset histogram ------------------------------------------------------------

inset_barplot_consump <- ggplot(surv_data) + 
  geom_histogram(aes(x=total_consump_kgyr, fill=total_consump_kgyr_cut),  
                 breaks= seq(0, 50, 5), color='black', size=0.1) +
  
  scale_fill_manual(values =   
                      c("0 to 5"    = '#d6f5d6',
                        "5 to 10"   = '#85e085',
                        "10 to 15"   = '#33cc33',
                        "15 to 20"   = '#1f7a1f',
                        "20 to 50"   = '#0a290a'))  + 
  
  ylab('Nb. countries') +
  xlab('Fish consumption\nall sources (kg/pers/yr)') +
  coord_flip() +
  #scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 50)) +
  barplot_mapinset_theme +
  theme(panel.border = element_blank(),
        legend.position=c(-0.3, 1.4),
        #legend.position="none",
        panel.background = element_rect(fill = "transparent",colour = NA), 
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size=5, colour='black'))


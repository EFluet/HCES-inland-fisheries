





# count the number of F/M products per country  to filter countries ------------
surv_data2 <-  surv_data %>%
  group_by(country.x, prod_src_fm, tot_pop_both_sexes) %>%
  summarise(prod_src_fm_sum = sum(Averageediblequantityconsumedgpersonday)) %>%
  ungroup() %>%
  mutate(prod_src_fm=ifelse(prod_src_fm=='F/M','FM',prod_src_fm)) %>% 
  spread(prod_src_fm, prod_src_fm_sum) %>%
  mutate_each(funs(replace(., is.na(.), 0))) %>%
  mutate(tot= rowSums(.[2:4])) %>%
  filter(F != 0 | FM != 0) %>%
  mutate(country_code = countrycode(country.x,'country.name','iso3c'))


# calculate ratios
df_src_fm_sum_wratio <- df_src_fm_sum %>%                        
  mutate(FtoFandM_ratio = F / (F+M),
         FnM_aspercoftot = (F+M)/(F+FM+M)) %>%
  filter(!is.na(FtoFandM_ratio)) %>%
  #filter(FtoFandM_ratio > 0) %>%
  #filter(clpa > 0)%>%
  #filter(clpa < 0.02) %>%
  filter(FnM_aspercoftot > 0.5) %>%
  mutate(geo_pos = ifelse(clpa == 0, 'Landlocked', NA),
         geo_pos = ifelse(clpa > 0.01, 'Islands', geo_pos),
         geo_pos = ifelse(is.na(geo_pos), 'Coastal', geo_pos))
#filter(FtoFandM_ratio>0)













### Map nb of products per survey -------------------------------------------------------
map_plot_perc_inland_consump <- ggplot(bbox_robin_df, aes(long, lat)) + 
  
  # add background country polygons
  geom_polygon(data=countries_robin_df,
               aes(long, lat, group=group),
               fill='grey85') +
  
  # add survey countries
  geom_polygon(data=countries_robin_df_wsurvey, aes(long, lat, group=group, fill= nb_product_cut), alpha=1) +
  
  # add country outline
  geom_path(data=countries_robin_df, aes(long, lat, group=group), color='white', size=0.1) +
  # Add outline bounding box
  geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
  geom_path(data=countries_robin_df_wsurvey, aes(long, lat, group=group), color='black', size=0.12) +
  
  
  coord_equal() +  theme_fig() +
  theme(legend.position="none") +
  
  scale_fill_manual(values =   
                      c("0 to 5"    = '#ffe6e6',
                        "5 to 10"   = '#ff9999',
                        "10 to 15"   = '#ff4d4d',
                        "15 to 20"   = '#ff0000',
                        "20 to 80"   = '#b30000')) +
  theme(plot.margin = unit(c(-2,-3,-2,-10), "mm"))



###  Nb prod inset histogram ------------------------------------------------------------

source('./consumption/plots/themes/map_inset_theme.r')

inset_barplot_nbprod <-    ggplot(surv_data) + 
  geom_histogram(aes(x=nb_product, fill=nb_product_cut),  
                 breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70), color='black', size=0.1) +
  
  scale_fill_manual(values =   
                      c("0 to 5"    = '#ffe6e6',
                        "5 to 10"   = '#ff9999',
                        "10 to 15"   = '#ff4d4d',
                        "15 to 20"   = '#ff0000',
                        "20 to 80"   = '#b30000')) +
  
  ylab('Number of \ncountries') +
  xlab('Number of fish \nproducts') +
  coord_flip() +
  barplot_mapinset_theme +
  theme(panel.border = element_blank(),
        legend.position=c(-0.2, 1.4),
        panel.background = element_rect(fill = "transparent",colour = NA), 
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size=5, colour='black'))


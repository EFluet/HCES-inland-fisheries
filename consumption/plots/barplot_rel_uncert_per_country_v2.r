
# calc relative uncert per country  ----------------------------------------------
sel_datatype <- c('uncert_prodfm_ur', 'uncert_fwae_assumed_mc', 'uncert_fwae_matched_mc')



df_forplot_uncert <- df_forplot %>%
                      group_by(country.x) %>%
                      filter(!is.na(p975),
                             datatype %in% sel_datatype) %>%
                      mutate(rng95 = p975 - p025) %>%
                      mutate(rng95rel = rng95/sum(rng95)) %>% 
                      mutate(rng95rel = ifelse(rng95==sum(rng95), 1, rng95rel)) %>%
                      mutate(rng95rel = rng95rel * 100) %>%
                      ungroup %>%
  
                      mutate(datatype= ifelse(datatype=='uncert_prodfm_ur', 'Unresolved source', datatype),
                             datatype= ifelse(datatype=='uncert_fwae_assumed_mc', 'Unidentified processing', datatype),
                             datatype= ifelse(datatype=='uncert_fwae_matched_mc', 'Identified processing', datatype))



# create bar plot or rel uncertainty source ------------------------------------
df_lrg_uncert <- df_forplot_uncert %>% filter(cnty_grp_panel == "Large prod")


lrg_uncert<- ggplot(df_lrg_uncert,  
                     aes(x = country_label, y = rng95rel, fill = datatype)) +
  
  geom_bar(stat="identity", width=0.4, colour='black', size=0.3) +
  xlab("") +  ylab("") + 
              coord_flip() +
  barplot_catchdiff_theme +
  scale_y_continuous(breaks= pretty_breaks(4)) +
  theme(legend.position = c(0.5, 1.14),#"none", 
        text = element_text(size=10),
        axis.text.y=element_blank(),
        legend.title=element_blank())  + 
  guides(fill = guide_legend(ncol = 2)) +
  scale_fill_grey()


#--------------------------------------------------------

df_sml_uncert <- df_forplot_uncert %>% filter(cnty_grp_panel == "Small prod")

sml_uncert<- ggplot(df_sml_uncert, 
                    aes(x = country_label, y = rng95rel, fill = datatype)) +
  
  geom_bar(stat="identity", width=0.4, colour='black', size=0.3) +
              xlab("") +  ylab("") + 
              coord_flip() +
              barplot_catchdiff_theme +
              scale_y_continuous(breaks= pretty_breaks(4)) +
              theme(legend.position = "none", 
                    text = element_text(size=10),
                    axis.text.y=element_blank()) + 
      scale_fill_grey() # scale_fill_brewer(palette = 'Blues')




#-------------------------------------------------------
df_smlst_uncert <- df_forplot_uncert %>% filter(cnty_grp_panel == "Smallest prod")


smlst_uncert<- ggplot(df_smlst_uncert, 
                    aes(x = country_label, y = rng95rel, fill = datatype)) +
  
                geom_bar(stat="identity", width=0.4, colour='black', size=0.3) +
                xlab("") +  ylab("Contribution to survey catch uncertainty (%)") + 
                coord_flip() +
                barplot_catchdiff_theme +
                scale_y_continuous(breaks= pretty_breaks(4)) +
                theme(legend.position = "none", 
                      text = element_text(size=10),
                      axis.text.y=element_blank()) +
        scale_fill_grey()


#---------------------------------------------------
lrg_uncert   <- lrg_uncert +   theme(plot.margin=unit(c(10, 2, -1, -1), "mm"))
sml_uncert   <- sml_uncert +   theme(plot.margin=unit(c(-1, 2, -1, -1), "mm"))
smlst_uncert <- smlst_uncert + theme(plot.margin=unit(c(-1, 2,  1, -1), "mm"))



# convert plots to ggtable objects
lrg_uncert <- ggplot_gtable(ggplot_build(lrg_uncert))
sml_uncert <- ggplot_gtable(ggplot_build(sml_uncert))
smlst_uncert <- ggplot_gtable(ggplot_build(smlst_uncert))



### save plot ------------------------------------------------------------------
# ggsave("../Output/Figures/test.png",
#        dpi=600, width=90, height=140, units='mm', type = "cairo-png")
# 
# dev.off()

# import package
source('./consumption/import_libraries.r')
# source the plot theme
source('./consumption/plots/themes/barplot_catchdiff_theme.r')

# read the datafile
df <- read.csv('../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc.csv',  stringsAsFactors = FALSE)


#make list of negative countries:
neg_countries <- df[which(df$mean < 0), "country_code"]


# make list of large/small producer countries for selection --------------------
large_prod_cntry_list<- df %>% 
      filter(datatype == 'sum_catch') %>%
      filter(!country_code %in% neg_countries) %>%
      mutate(cnty_grp_panel= ifelse(mean >= 0.15, 
                                    "Large prod", "Small prod")) %>%
      mutate(cnty_grp_panel= ifelse(mean <= 0.033, 
                                    "Smallest prod", cnty_grp_panel)) %>%
      select(country.x, cnty_grp_panel) %>%
      distinct()


# Format the data to long format and sort it for plotting ----------------------
df_forplot <- df %>%
      group_by(country.x) %>%
      #mutate(min_mean = min(mean)) %>%
      #filter(min(mean) >= 0) %>%
      ungroup %>%
      select(country.x, country_label, p025, mean, p975, datatype) %>%
      left_join(., large_prod_cntry_list, by='country.x') %>%
      arrange(datatype, mean) %>% 
      mutate(datatype= ifelse(datatype=='sum_catch','FAO Catch (FishStatJ)', datatype),
             datatype= ifelse(datatype=='tr_corr_consump', 'HCES estimated catch',datatype))


# Create factor list for ordered bar ----------------------------
df_fororder <-  df_forplot %>%  filter(datatype %in% c('FAO Catch (FishStatJ)')) %>% arrange(mean)
df_forplot$country_label <- factor(df_forplot$country_label, 
                                   levels = factor(df_fororder$country_label))

df_forplot_catch <- df_forplot %>% filter(datatype %in% c('FAO Catch (FishStatJ)', 'HCES estimated catch')) 


#--------------------------------------------------------------------
df_lrg<- df_forplot_catch %>% filter(cnty_grp_panel == "Large prod")


t80 <- ggplot(df_lrg, 
      aes(x = country_label, 
                  y = mean, ymin = p025, ymax = p975, 
                  fill = datatype)) +
      
      geom_bar(stat="identity", position=position_dodge(), width = 0.75) +
      geom_errorbar(position='dodge', width=0.75, size=0.3, colour='grey10') +
      coord_flip() +
      
      xlab("") +  ylab("") + 
      barplot_catchdiff_theme +
      scale_fill_grey() +
      scale_y_continuous(breaks = pretty_breaks(6)) +
      theme(legend.position = c(0.4, 1.16),#"none", 
            text = element_text(size=10),
            plot.title = element_text(size = rel(0.8)),
            legend.title=element_blank())
  


##----------------------------------------------------------------
df_sml<- df_forplot_catch %>% filter(cnty_grp_panel == "Small prod")

b20 <- ggplot(df_sml, 
        aes(x = country_label, 
                    y = mean, ymin = p025, ymax = p975,
                    fill = datatype)) +
        
        geom_bar(stat="identity", position=position_dodge(), width = 0.75) +
        geom_errorbar(position='dodge', width=0.75, size=0.3, colour='grey10') +
        
        coord_flip() +
        xlab("") +  ylab("") + 
        barplot_catchdiff_theme +
        scale_fill_grey() +
        scale_y_continuous(breaks = pretty_breaks(6)) +
        theme(legend.position = "none", 
              text = element_text(size=10),
              plot.title = element_text(size = rel(0.8)))


#---------------------------------------------------------------------
df_smlst<- df_forplot_catch %>% filter(cnty_grp_panel == "Smallest prod")


smlst <- ggplot(df_smlst, 
              aes(x = country_label,
                  y = mean,
                  ymin = p025,
                  ymax = p975,
                  fill = datatype)) +
  
  geom_bar(stat="identity", position=position_dodge(), width = 0.75) +
  geom_errorbar(position='dodge', width=0.75, size=0.3, colour='grey10') +
  
  coord_flip() +
  xlab("") +  ylab("Catch (million tons)") + 
  barplot_catchdiff_theme +
  scale_fill_grey() +
  scale_y_continuous(breaks = pretty_breaks(6)) +
  theme(legend.position = "none", 
        text = element_text(size=10), #legend.position = "bottom", 
        plot.title = element_text(size = rel(0.8)))



# arrange in grid --------------------------------------------------------------
# set tight margins so plots are close side-by-side
# Dimensions of each margin: t, r, b, l     (To remember order, think trouble).
t80   <- t80 +   theme(plot.margin=unit(c(10, -2, -1, -1), "mm"))
b20   <- b20 +   theme(plot.margin=unit(c(-1, -2, -1, -1), "mm"))
smlst <- smlst + theme(plot.margin=unit(c(-1, -2,  1, -1), "mm"))


# convert plots to ggtable objects
t80 <- ggplot_gtable(ggplot_build(t80))
b20 <- ggplot_gtable(ggplot_build(b20))
smlst <- ggplot_gtable(ggplot_build(smlst))

# set max width of either plot as the width for both
library(grid)
maxWidth = unit.pmax(t80$widths[2:3], b20$widths[2:3], smlst$widths[2:3])
t80$widths[2:3] <- maxWidth
b20$widths[2:3] <- maxWidth
smlst$widths[2:3] <- maxWidth


# t80$layout$clip[t80$layout$name == "panel"] <- "off"
# b20$layout$clip[b20$layout$name == "panel"] <- "off"
# smlst$layout$clip[smlst$layout$name == "panel"] <- "off"

nb_cntry <- nrow(df_forplot)/2


t80_height = (nrow(df_lrg)) / nb_cntry
b20_height = (nrow(df_sml)-4)  / nb_cntry
smlst_height = (nrow(df_smlst)-4) / nb_cntry



# Make uncertainty plots ------------------------------------------------------- 
source('./consumption/plots/barplot_rel_uncert_per_country_v2.r')


# arrange plots grob into layout  ------------------------------------------------
#p <- arrangeGrob(t80, b20)#, heights=c(0.7, 1.1+(t80_width/b20_width)))
#p <- plot_grid(t80, b20, ncol = 1, nrow = 2, align = 'v', heights=c(1,1))
#p <- grid.arrange(t80, b20, smlst, heights=c(b20_width, t80_width, smlst_width))
p <- grid.arrange(t80, lrg_uncert, b20, sml_uncert, smlst, smlst_uncert, 
                  heights=c(t80_height, b20_height, smlst_height),
                  widths=c(1.37, 1.0))

### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/barplot_2bars_uncert_dec2017.png", p, 
       dpi=600, width=210, height=195, units='mm', type = "cairo-png")

dev.off()

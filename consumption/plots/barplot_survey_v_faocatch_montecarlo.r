# source the plot theme
source('./consumption/plots/themes/barplot_catchdiff_theme.r')

# read the datafile
df <- read.csv('../output/consumption/hh_survey_consump_nat_agg_prod_fw_mc.csv',
                                      stringsAsFactors = FALSE)


# make list of large/small producer countries for selection
large_prod_cntry_list <- df %>% filter(datatype == 'sum_catch', mean > 0.2) 



# Format the data to long format and sort it for plotting ----------------------
# modify data forp
df_forplot <- df %>%
              filter(datatype %in% c('sum_catch','tr_corr_consump')) %>%
              select(country.x, p025, mean, p975, datatype) %>%
              mutate(cnty_grp_panel = ifelse(country.x %in% large_prod_cntry_list$country.x,
                                             "Large prod", "Small prod")) %>%
              arrange(datatype, mean) %>% #cnty_grp_panel, 
              mutate(datatype= ifelse(datatype=='sum_catch','FAO Catch (FishStatJ)', 'HCES estimated catch'))



# plot br graph of consump prod and national prod ----------------------------
# Create factor list for ordered bar plot 
df_forplot$country.x <- factor(df_forplot$country.x,
                               levels=unique(df_forplot$country.x))

df_forplot$country.x <- as.factor(df_forplot$country.x)
df_forplot_order <- df_forplot[order(df_forplot$mean, df_forplot$datatype) , ]


#--------------------------------------------------------------------

df_lrg<- df_forplot %>%
         filter(cnty_grp_panel == "Large prod")


t80 <- ggplot(df_lrg, 
                   aes(x = country.x,
                       y = mean,
                       ymin = p025,
                       ymax = p975,
                       fill = datatype)) +
              
              geom_bar(stat="identity", position=position_dodge(), width = 0.7) +
              geom_errorbar(position='dodge', width=0.7, size=0.3, colour='grey10') +
            
              coord_flip() +
              #facet_wrap(~cnty_grp_panel, ncol=1 , scales="free")+#, space='free_x') +
              
              xlab("") +  ylab("") + 
              barplot_catchdiff_theme +
              scale_fill_grey() +
              scale_y_continuous(breaks = pretty_breaks(3)) +
              theme(text = element_text(size=10), #legend.position = "bottom", 
                    plot.title = element_text(size = rel(0.8)))


################################################################################
df_sml<- df_forplot %>%
  filter(cnty_grp_panel == "Small prod")

b20 <- ggplot(df_sml, 
                   aes(x = country.x,
                       y = mean,
                       ymin = p025,
                       ymax = p975,
                       fill = datatype)) +
              
              geom_bar(stat="identity", position=position_dodge(), width = 0.7) +
              geom_errorbar(position='dodge', width=0.7, size=0.3, colour='grey10') +
              
              coord_flip() +
              #facet_wrap(~cnty_grp_panel, ncol=1 , scales="free")+#, space='free_x') +
              
              xlab("") +  ylab("Catch (million tons)") + 
              barplot_catchdiff_theme +
              scale_fill_grey() +
              scale_y_continuous(breaks = pretty_breaks(3)) +
              theme(legend.position = "none", 
                    text = element_text(size=10),
                    plot.title = element_text(size = rel(0.8)))




# arrange in grid --------------------------------------------------------------
# set tight margins so plots are close side-by-side
# Dimensions of each margin: t, r, b, l     (To remember order, think trouble).
t80 <- t80 + theme(plot.margin=unit(c(-1, 10, -1, -1), "mm"))
b20 <- b20 + theme(plot.margin=unit(c(-1, 10, 10, -1), "mm"))

# convert plots to ggtable objects
t80 <- ggplot_gtable(ggplot_build(t80))
b20 <- ggplot_gtable(ggplot_build(b20))

# set max width of either plot as the width for both
library(grid)
maxWidth = unit.pmax(t80$widths[2:3], b20$widths[2:3])
t80$widths[2:3] <- maxWidth
b20$widths[2:3] <- maxWidth

t80$layout$clip[t80$layout$name == "panel"] <- "off"
b20$layout$clip[b20$layout$name == "panel"] <- "off"



t80_width = nrow(df_sml) / 26
b20_width = nrow(df_lrg) / 26


# arrange plots grob into layout 
#p <- arrangeGrob(t80, b20)#, heights=c(0.7, 1.1+(t80_width/b20_width)))
p <- grid.arrange(t80, b20,  heights=c(b20_width, t80_width))
#p <- plot_grid(t80, b20, ncol = 1, nrow = 2, align = 'v', heights=c(1,1)) 


### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/barplot_surveycatch_vs_natcatch_fw_montecarlo_v5.png", p,
       dpi=300, width=90, height=180, units='mm', type = "cairo-png")

dev.off()

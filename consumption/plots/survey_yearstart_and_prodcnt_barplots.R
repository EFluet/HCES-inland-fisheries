
library(ggplot2)



# Plot a histogram of the distribution of surbeys per year
# the input dataframe is an output of the scripts calculating the diff with fao data.


h <-  ggplot(ca_ctry_jnd_p, aes(year_start)) +
      geom_histogram(binwidth = 1, fill='grey60', colour='white', size=0.05) +
      geom_vline(xintercept = 2012, colour= 'red') +
      ggtitle('') + xlab("Year") + ylab("Survey count") + 
      scale_x_continuous(breaks = c(seq(1996, 2012, by = 2))) +
  
  
  theme_bw() +
  theme(#legend.position = "bottom",
    legend.position = c(.02, -.11),
    legend.key.size = unit(3, "mm"),
    legend.direction= 'horizontal',
    text = element_text(size=8),
    legend.key = element_rect(colour=NULL),
    legend.title = element_blank(),
    plot.title = element_text(size = rel(0.8)),
    plot.background = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank())

# save figure to output file ---------------------------------------------------
ggsave("../output/figures/survey_yearstart_histogram.png", h, 
       width=70, height=35, dpi=800, units="mm", type = "cairo-png")


dev.off()



# Plot histogram of number of products per -------------------------------------


# Read input files 
# read the code-assigned product list
consump_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods_fwaes_uncert.csv', stringsAsFactors=FALSE)

# count the number of F/M products per country  to filter countries 
prod_src_fm_cnt_perctry <-  consump_df %>%
          group_by(country.x, prod_src_fm) %>%
          summarise(src_fm_count = n()) %>%
          ungroup() %>%
          mutate(prod_src_fm=ifelse(prod_src_fm=='F/M','FM',prod_src_fm)) %>%
          spread(prod_src_fm, src_fm_count) %>%
          mutate(F=ifelse(is.na(F),0, F),
                 FM=ifelse(is.na(FM),0, FM),
                 M=ifelse(is.na(M),0, M)) %>%
          # selection criteria for countries, that F products constitute 50% of product pool
          filter((F/(F+FM)) >= 0.5)


# plot histogram -------------------------------
p <-  ggplot(consump_df_prodcount, aes(prod_count)) +
  geom_histogram(binwidth = 1, fill='grey60', colour='white', size=0.05) +
  #geom_vline(xintercept = 2012, colour= 'red') +
  ggtitle('') + xlab("Number of distinct products") + ylab("Survey count") + 
  #scale_x_continuous(breaks = c(seq(1996, 2012, by = 2))) +
  
  
  theme_bw() +
  theme(#legend.position = "bottom",
    legend.position = c(.02, -.11),
    legend.key.size = unit(3, "mm"),
    legend.direction= 'horizontal',
    text = element_text(size=8),
    legend.key = element_rect(colour=NULL),
    legend.title = element_blank(),
    plot.title = element_text(size = rel(0.8)),
    plot.background = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank())

p

# histogram of product count per type  ------------------------------------------------

p <-  ggplot(consump_df_prodcount, aes(x=country, y=prod_count, fill=Freshwater..F..or.Marine..M.)) +
      geom_bar(stat = "identity", position = 'stack') +
      ggtitle('') + xlab("") + ylab("Number of products") + coord_flip() +
  
  theme_bw() +
  theme(legend.position = c(0.8, 0.5),
    #legend.key.size = unit(3, "mm"),
    #legend.direction= 'horizontal',
    text = element_text(size=8),
    legend.background = element_rect(colour = "black"),
    legend.key = element_rect(colour=NULL),
    legend.title = element_blank(),
    plot.title = element_text(size = rel(0.8)),
    plot.background = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank())

p



# save figure to output file ---------------------------------------------------
ggsave("../output/figures/survey_prodcount_pertype_histogram.png", p, 
       width=210, height=140, dpi=800, units="mm", type = "cairo-png")

dev.off()



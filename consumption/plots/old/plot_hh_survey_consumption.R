


### Plot bar graph of the  --------------------------------------------------
# for each row with NA in column: Average edible quantity consumed (g/person/day) 
# sum the below values 
# library(ggplot2)
# library(plotly)
# 
# 
# tiff("../../Output/Figures/HHsurvey_fishconsump_tonsperyr_bycountry_.tif",
#      res=300,width=5.5,height=4,units='in', compression = c("none"))
# 
# 
# consump_pop_df$concat <- apply( consump_pop_df[ , c('country','year_start')] , 1 , paste , collapse = "," )
# 
# 
# p <- ggplot(data=consump_pop_df, 
#        aes(x=reorder(concat, consump_million.tons.yr),
#            y=(consump_million.tons.yr/1000000))) +
#   geom_bar(stat="identity") + 
#   coord_flip() +
#   ylab('National fish consumption (million tonnes per year)') + 
#   xlab('Household expendure surveys') + 
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))
#  
# p
# #(gg <- ggplotly(p))
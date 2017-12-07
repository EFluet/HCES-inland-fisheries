### Import modules
library(ggplot2)
library(dplyr)
library(ggrepel)


### Read in data ---------------------------------------------------------------
consump_pop_nat_joined <- read.csv("../output/consump_pop_nat_joined.csv")

# paste country and year
consump_pop_nat_joined$cntry_yr <- paste(consump_pop_nat_joined$country, 
                                         consump_pop_nat_joined$year, 
                                         sep=",")



### Subset table to only big fish producers ------------------------------------


# Subset to 10 countries with highest inland catch
top_inl_ca_countries <- consump_pop_nat_joined %>%
                        filter(source == "Inland" & type == "Catch") %>%
                        top_n(n = 10, wt = value)

# Retrieve all rows for the top countries found above 
top_inl_ca_countries <- consump_pop_nat_joined %>%
                        filter(country %in% top_inl_ca_countries$country)
  



### Plot histogram of all countries joined data -------------------


# create output table
tiff("../../output/figures/HHsurvey_all_bycountryfacet1.png",
     res=200,width=16,height=9,units='in', compression = c("none"))


# Plotting
p <- ggplot(consump_pop_nat_joined, aes(x = type, y = value, fill = source)) +
  geom_bar(stat="identity") +
  #facet_grid(cntry_yr ~. , scales = "free") +
  facet_wrap(~cntry_yr, ncol = 12, scales = "free") +
  #coord_flip() + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=6),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.text.x = element_text(size = 6, face = "bold"))

p
dev.off()

### join national statistics to consumption

### Import modules -------------------------------------------------------------
library(countrycode)
library(data.table)


setwd('C:/Users/efluet/Dropbox/chap5_global_inland_fish_catch/scripts/consumption')



### run scripts that read in data ----------------------------------------------

# read data for consumption and catch
source('./format_nat_fish_consumption_data.R', print.eval=TRUE)

# read in consumption survey and national fishstat 
source('../fishstatj_data/read_nat_aquaculture_data.R', print.eval=TRUE)
source('../fishstatj_data/read_nat_catch_data.R', print.eval=TRUE)
source('../fishstatj_data/read_nat_trade_data.R', print.eval=TRUE)

# delete unneeded objects from the sourced scripts
rm(tr, ca, aq, aq_inl)

###

colnames(aq_sum_bycountry)[3] <- "value"
colnames(ca_sum_bycountry)[3] <- "value"
colnames(tr_sum_bycountry)[3] <- "type"
colnames(tr_sum_bycountry)[4] <- "value"


nat_joined <- do.call("rbind", 
                      list(aq_sum_bycountry, 
                           ca_sum_bycountry, 
                           tr_sum_bycountry))


###  Join the dataframes of national FishStatJ data ----------------------------

nat_joined <- ca_sum_bycountry %>%
              full_join(aq_sum_bycountry, by = c('CountryCountry','year')) %>%
              full_join(tr_sum_bycountry, by = c('CountryCountry','year'))


# remove the joined tables
rm(ca_sum_bycountry, aq_sum_bycountry, tr_sum_bycountry)



### Convert the country names of national catch data to ISO3 codes -------------
nat_joined$country_code <- countrycode(nat_joined$CountryCountry, 
                                       'country.name', 'iso3c', warn = TRUE)



### Join the consumption survey and national FishStatJ data --------------------

consum_pop_nat_joined <- consump_pop_df %>%
  left_join(nat_joined, 
            by = c('country_code'='country_code',
                   'year_start'='year'))

# remove df from before join
rm(consump_pop_df, nat_joined)







### Plot the joined data -------------------

drops <- c("year",
           "year_end",
           "Averageediblequantityconsumedgpersonday",
           "tot_pop_both_sexes",
           "country.y",
           "CountryCountry",
           "country_code")
consum_pop_nat_joined <- consum_pop_nat_joined[ , !(names(consum_pop_nat_joined) %in% drops)]


# first modify format to long format 
a <- consum_pop_nat_joined %>%
  gather(country)



df %>%
  gather(key, value, -id, -time) %>%
  extract(key, c("question", "loop_number"), "(Q.\\..)\\.(.)") %>%
  spread(question, value)



# Plotting
p <- ggplot(consum_pop_nat_joined, aes(x = country, y = sum_catch, fill = FishingareaFAOmajorfishingarea)) +
  geom_bar(stat="identity") +
  #facet_grid(.~Country) +
  coord_flip() + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

p


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




# # Data  
# Hex <- sample(c("H1","H2","H3","H4","H5","H6"),size = 100,replace = TRUE)
# Period <- sample(c("period 0",   "period 1","period 2", "period 3","period 3.5", "period 4","period 5",   "period 7"),size = 100, replace = TRUE)
# Teacher <- sample(c("13","14","15","16","17","18","19","20","21","22","23","24","25"), size = 100, replace = TRUE)
# Qty <- sample(c(1:9), size = 100, replace = TRUE)
# 
# ref_by_period_df <- data.frame(Hex=Hex,Period=Period,Teacher=Teacher,Qty=Qty)
# ref_by_period_df <- ddply(ref_by_period_df, .(Period), transform, pos = cumsum(Qty) - (0.5 * Qty)) #Now let's calculate midpoint



# remove column
#consump_pop_df <- subset(consump_pop_df, -c(country.y, year, tot_pop_both_sexes, Averageediblequantityconsumedgpersonday))
# 
# # h
# consump_pop_catch_df <- consump_pop_df %>%
#   left_join(nat_rep_catch_allyr, by = c("country_code" = "country_code", "year_start" = "year"))
# 
# # remove columns
# consump_pop_df <- subset(consump_pop_df, -c(country.y, 
#                                             measure, 
#                                             SpeciesASFISspecies, 
#                                             FishingareaFAOmajorfishingarea, 
#                                             X))
# # remove pre-join tables
# rm(nat_rep_catch_allyr, consump_pop_df)




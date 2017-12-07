# this script goes after the country list is filtered, but before the marine products get removed
library('kedd')

# read file
#prod_codes_df <- read.csv('../output/consumption/prod_codes/Consumption_statistics_of_fish_unique_fishtypes_with_codes.csv', stringsAsFactors=FALSE)



# clean data table prior to join
consump_df2 <- consump_df %>%
  select(unique.country, unique.prod_name, prod_src_fm) %>%
  mutate(
    # Convert columns to upper/lower case to match case between tables to join
    country_code = countrycode(unique.country,
                               'country.name', 'iso3c', warn = F),
    prod_src_fm = toupper(prod_src_fm),
    # Make assumption by filling blank codes with F/M, 
    #for products without an expert code
    # filter rows to remove producs and country_codes with NA 
    prod_src_fm = ifelse(is.na(prod_src_fm) | prod_src_fm == "", '
                         F/M', prod_src_fm)) %>%
  filter(!is.na(unique.prod_name), !is.na(country_code)) %>% 
  select(country_code, unique.country, prod_src_fm)
  

# Read file with geographic position -------------------------------------------
geo_pos <- read.csv('../data/consumption/country_geo_pos.csv', 
                    stringsAsFactors=FALSE)
geo_pos <- geo_pos %>%
            select(country_code, geo_pos)


# calc the fraction of F products, 
consump_df2 <- consump_df2 %>%
    filter(prod_src_fm != 'F/M') %>%
    mutate(prod_src_fm_num = ifelse(prod_src_fm=='F',1,0)) %>%
    group_by(country_code) %>%
    summarize(frac = sum(prod_src_fm_num)/n()) %>%
    mutate(continent = countrycode(country_code,'iso3c', 
                                   'continent', warn = F)) %>%
    mutate(country = countrycode(country_code,'iso3c', 
                                 'country.name', warn = F)) %>%
    left_join(., geo_pos, by='country_code') 



# make individual lists for two types of geo_pos
landlocked <- unname(c(unlist(subset(consump_df2, geo_pos=='Landlocked')['frac'])))
coastal <- unname(c(unlist(subset(consump_df2, geo_pos=='Coastal')['frac'])))  
  

# landlocked- estiate density kernel -------------------------------------------
t <- density(landlocked, bw = "nrd0", adjust = 2, # width=1, 
             kernel = c("gaussian"), n=10000, from=0, to=1)

l <- data.frame(x = rep(NA,10000), y=rep(NA,10000))
l$x <- c(t$x)
l$y <- c(t$y)
l$geo_pos <- 'landlocked' 


# coastal - estiate density kernel -------------------------------------------
t <- density(coastal, bw = "nrd0", adjust = 2, # width=1, 
             kernel = c("gaussian"), n=10000, from=0, to=1)

c <- data.frame(x = rep(NA,10000), y=rep(NA,10000))
c$x <- c(t$x)
c$y <- c(t$y)
c$geo_pos <- 'coastal'


# empirical continuous 
p_coastal <- mcstoc(rempiricalC, n=10000, min=0, max=1, values=c$x, prob=c$y)
p_landlocked <- mcstoc(rempiricalC, n=10000, min=0, max=1, values=l$x, prob=l$y)


lc <- rbind( c, l)


landlocked_mc <- l
coastal_mc <- c


# plot the distributions
source('./consumption/plots/fm_densitykernel_plot.r')

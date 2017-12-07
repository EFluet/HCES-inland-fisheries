
### FWAEs w/ conv.factors - convert consumed weights into whole animal weights ------------------------


# read conversion factors table
consump_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods.csv',
                       stringsAsFactors=F)
# read conversion factors table
conv_fwae <- read.csv('../data/consumption/fwae_conversion_factors/fwae_conversion_factors.csv', 
                      stringsAsFactors=F)



### Process the FWAE conversion factors ------------------------------------------
conv_fwae <- conv_fwae %>% 
  #filter(original_source == 'Hortle et al. 2007') %>%
  select(-one_of('compiled_from','region','species')) %>%
  #spread(original_source, factor_cons_prod_to_fwae) 
  group_by(prod_eng) %>%
  mutate(min_conv_fac = min(factor_cons_prod_to_fwae),
         mean_conv_fac = mean(factor_cons_prod_to_fwae),
         max_conv_fac = max(factor_cons_prod_to_fwae)) %>%
  select(-one_of(c('species','region','compiled_from','original_source','factor_cons_prod_to_fwae','comment'))) %>%
  distinct() %>%
  gather(lang, product, prod_eng:prod_esp) %>%
  select(-lang)  # remove column listing language of product


# create string of words to remove from fish product names
rm_str <- 'fish|Fish|poisson|Poisson|pescado|autres|autros|otros|peixe|outros'
consump_df <- consump_df %>%
  # Remove word fish from strings of both tables to improve match
  mutate(prod_name_mod = gsub(rm_str," ", unique.prod_name)) %>%
  # substitute some punctionation for spaces to improve splits in some cases.
  mutate(prod_name_mod = gsub("[[:punct:]]", "", prod_name_mod)) %>%
  # create empty product column where matches will be written 
  mutate(product = NA)



# loop through products with conv factors
# there is probs a way to make this more efficient with a 
for (y in 1:nrow(conv_fwae)) {
  
  # declare product name of loop as string pattern to look for
  pattern <- conv_fwae[y,'product']
  
  
  # look for string pattern in each of the words of fish type name (split with spaces)
  # allow for a 1 letter change for match to be made
  consump_df$temp_match <-  sapply(seq(nrow(consump_df)),
                                   
                                   function(i){ unlist(agrepl(pattern, 
                                                              strsplit(as.character(consump_df[i,'prod_name_mod']), ' '), 
                                                              ignore.case = TRUE, max = 1), recursive = TRUE)})
  
  # for rows with match in loop, copy the matched product name 
  # to a permanent column (these can get overwritten by later loops) 
  consump_df <- consump_df %>%
    mutate(product=as.character(ifelse(temp_match == TRUE,
                                       conv_fwae[y,'product'],product))) %>%
    select(-temp_match) # remove temporary match column
}


# ------------------------------------------------
# join the other columns of FWAEs conversion factors and apply factors to consumption
consump_df <- consump_df %>% 
  
  left_join(., conv_fwae, by=c('product')) %>% 
  
  mutate(cons_miltonspyr = consump_million.tons.yr) %>%  
  
  mutate(cons_miltonspyr_fm_min = cons_miltonspyr,
         cons_miltonspyr_fm_mean= cons_miltonspyr,
         cons_miltonspyr_fm_max = cons_miltonspyr) %>%
  
  
  mutate(cons_miltonspyr_fm_min = ifelse(prod_src_fm == 'F/M', 0, cons_miltonspyr_fm_min),
         cons_miltonspyr_fm_mean = ifelse(prod_src_fm == 'F/M', cons_miltonspyr/2, cons_miltonspyr_fm_mean),
         cons_miltonspyr_fm_max = ifelse(prod_src_fm == 'F/M', cons_miltonspyr, cons_miltonspyr_fm_max)) %>%
  
  
  mutate(cons_miltonspyr_fm_min = ifelse(prod_src_fm == 'M', 0, cons_miltonspyr_fm_min),
         cons_miltonspyr_fm_mean = ifelse(prod_src_fm =='M', 0, cons_miltonspyr_fm_mean),
         cons_miltonspyr_fm_max = ifelse(prod_src_fm == 'M', 0, cons_miltonspyr_fm_max)) %>%
  
  
  
  # if no match is made, use the fresh fish FWAE conversion factor (1.25)
  mutate(min_conv_fac = ifelse(is.na(min_conv_fac), 1.25, min_conv_fac),
         mean_conv_fac = ifelse(is.na(mean_conv_fac), 2.50, mean_conv_fac),
         max_conv_fac = ifelse(is.na(max_conv_fac), 2.91, max_conv_fac)) %>%
  
  
  # convert consumed weight to FWAEs by mutliplying with consumed weight
  mutate(cons_miltonspyr_fm_min_fwaes = min_conv_fac * cons_miltonspyr_fm_min,
         cons_miltonspyr_fm_mean_fwaes = mean_conv_fac * cons_miltonspyr_fm_mean,
         cons_miltonspyr_fm_max_fwaes = max_conv_fac * cons_miltonspyr_fm_max)


# rename columns after join added some subfix
colnames(consump_df)[colnames(consump_df) == 'country_code.y'] <- 'country_code'
colnames(consump_df)[colnames(consump_df) == 'country.y'] <- 'country'


# remove unneeded object from environment
rm(pattern, y, rm_str, conv_fwae)


# copy country names (for consistency with nat_agg df)
#consump_df$country <- countrycode(consump_df$country_code,'iso3c','country.name',warn=TRUE)
consump_df$country <- consump_df$country.x

### Write the table to csv file ------------------------------------------------
write.csv(consump_df, '../output/consumption/hh_survey_consump_sep_prods_fwaes_uncert.csv')


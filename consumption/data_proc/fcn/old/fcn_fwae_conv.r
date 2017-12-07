# Function converting consumed weights into FWAE weights
# Function input: list of product names
#          output: dataframe of associated conversion factors


### read conversion factors table-----------------------------------------------
conv_fwae <- read.csv('../data/consumption/fwae_conversion_factors/fwae_conversion_factors_sep.csv', 
                      stringsAsFactors=F)


# create function that applies the conversion factors ---------------------------

fwae_conversion <- function(prod_list){
  
  
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
    select(-lang) %>% # remove column listing language of product
    distinct()  # remove duplicate rows, when word is same for 2 languages 
  
  df <- as.data.frame(prod_list)
  names(df) <- 'prod_name'
  
  # create string of words to remove from fish product names
  rm_str <- 'fish|Fish|poisson|Poisson|pescado|autres|autros|otros|peixe|outros|
            Freshwater|freshwater|salmonid'
  
  df <- df %>%
    # Remove word fish from strings of both tables to improve match
    mutate(prod_name_mod = gsub(rm_str," ", prod_name)) %>%
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
    df$temp_match <-  sapply(seq(nrow(df)),
                             
           function(i){ unlist(agrepl(pattern, 
                                      strsplit(as.character(df[i,'prod_name_mod']), ' '), 
                                      ignore.case = TRUE, max = 1), recursive = TRUE)})
    
    # for rows with match in loop, copy the matched product name 
    # to a permanent column (these can get overwritten by later loops) 
    df <- df %>%
      mutate(product=as.character(ifelse(temp_match == TRUE,
                                         conv_fwae[y,'product'],product))) %>%
      select(-temp_match) # remove temporary match column
  }
  
  ### join conv factors to list of processing method for each product ------------------ 
  df <- left_join(df, conv_fwae, by=c('product')) 
  
  # if no match is made, use the fresh fish FWAE conversion factor (1.25)
  df <- df %>%
    mutate(min_conv_fac = ifelse(is.na(min_conv_fac), 1.25, min_conv_fac),
           mean_conv_fac = ifelse(is.na(mean_conv_fac), 2.50, mean_conv_fac),
           max_conv_fac = ifelse(is.na(max_conv_fac), 2.91, max_conv_fac))
    
  # previously:  min, mean, max :  1.25 - 2.50 - 2.91
  
  
  return(df)
}

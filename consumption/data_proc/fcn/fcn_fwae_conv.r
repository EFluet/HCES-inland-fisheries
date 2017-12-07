# Description: Function converting consumed weights into FWAE weights.
# Input: List of product names
# Output: Dataframe of associated conversion factors


### read conversion factors table-----------------------------------------------
conv_fwae <- read.csv('../data/consumption/fwae_conversion_factors/fwae_conversion_factors_sep_v2.csv', 
                      stringsAsFactors=F)


# Quick fix: remove snakes and frogs because of confusion 
# with other types of products -------------------------------------------------
conv_fwae <- conv_fwae %>%
             filter(prod_eng != 'snakes',
                    prod_eng != 'frogs')


# Creat function doing FWAE conversion ----------------------------------------
fwae_conversion <- function(prod_list){
  
  #prod_list <- consump_df$prod_name
  
  ### Prepare df of FWAE factor ------------------------------------------
  conv_fwae <- conv_fwae %>% 
    select(-one_of('compiled_from','region','species','spec')) %>%
    #spread(original_source, factor_cons_prod_to_fwae) 
    group_by(prod_eng) %>%
    
    # calculate min,mean,max of conversion factors
    mutate(min_conv_fac = min(factor_cons_prod_to_fwae),
           mean_conv_fac = mean(factor_cons_prod_to_fwae),
           max_conv_fac = max(factor_cons_prod_to_fwae)) %>%
    select(-one_of('original_source', 'factor_cons_prod_to_fwae','comment')) %>%
    #distinct() %>%
    gather(lang, product, prod_eng:prod_esp) %>%
    select(-lang) %>% # remove column listing language of product
    # remove duplicate rows, when word is same for 2 languages
    distinct(min_conv_fac, mean_conv_fac, max_conv_fac, product)   
  
  df <- as.data.frame(prod_list)
  names(df) <- 'prod_name' 
  
  
  # Clean up product strings -----------------------------
  
  # create string of words to remove from fish product names
  rm_str <- 'fishes|Freshwater|fish|Fish|poissons|poisson|Poisson|pescado|autres|autros|otros|peixe|outros|
            Freshwater|freshwater|salmonid|salmonoids|Salmonoids|Salmons|Salmon|
            salmon|Trouts|chars|and|Pacific|nei|Atlantic|herring|Danube salmons|salmon'
  
  df <- df %>%
    # Remove word fish from strings of both tables to improve match
    mutate(prod_name_mod = gsub(rm_str," ", prod_name)) %>%
    # substitute some punctionation for spaces to improve splits in some cases.
    mutate(prod_name_mod = gsub("[[:punct:]]", "", prod_name_mod)) %>%
    # create empty product column where matches will be written 
    mutate(product = NA)
  
  
  # Loop through products and match FWAE conv factors -------------------------
  # there is probs a way to make this more efficient with an apply
  for (y in 1:nrow(conv_fwae)) {
    
    # get product name of loop as string pattern to look for
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
  
  
  ### join conv factors to list of processing method for each product ---------- 
  df <- left_join(df, conv_fwae, by=c('product')) 
  
  # For products without a match, use the fresh fish FWAE conversion factor (1.25)
  df <- df %>%
    mutate(min_conv_fac = ifelse(is.na(min_conv_fac), 1.0, min_conv_fac),
           mean_conv_fac = ifelse(is.na(mean_conv_fac), 1.227634, mean_conv_fac),
           max_conv_fac = ifelse(is.na(max_conv_fac), 1.612903, max_conv_fac)) 
    
  # Initially was :  min, mean, max :  1.25 - 2.50 - 2.91
  # then changed to fresh=  1.25    smoked= 2.45   average of dried=  2.955
  # Jan 30 is now:  avg fresh= 2.185  &  avg dried/smoked= 2.955  for use in uniform distribution  
  #  1.227634
  return(df)
  
}


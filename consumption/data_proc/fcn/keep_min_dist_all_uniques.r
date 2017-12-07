

### function selecting the smallest number of stringdist join combination 
keep_min_dist_all_uniques <- function(input_df){

  
  # select only useful columns
  combinations <- input_df %>%
                  select(prod_name_mod, SpeciesASFISspecies, distance_ca) 
  
  # create empty output df
  combinations.RN <- combinations[0,]
  
  
  # loop through rows
  for (i in seq(nrow(combinations))){
    
    # get count of each prod_name
    nb_consump_prod <- nrow(subset(consump_df_temp, prod_name==as.character(consump_df_temp[i,'prod_name'])))
    nb_ca_prod <- nrow(subset(consump_df_temp, SpeciesASFISspecies==as.character(consump_df_temp[i,'SpeciesASFISspecies'])))
  
    # if either in unique in combination df, copy to output 
    if (nb_consump_prod == 1 | nb_ca_prod == 1){
      combinations.RN <- rbind(combinations.RN, combinations[i,])}
     
    # if both have duplicates... 
    if (nb_consump_prod > 1 & nb_ca_prod > 1){
  
      # populate temp df of the diff combinations
      temp <- combinations %>% 
              filter(prod_name_mod == as.character(combinations[i,'prod_name_mod']) |
                     SpeciesASFISspecies == as.character(combinations[i,'SpeciesASFISspecies'])) %>% 
        
        filter(!prod_name_mod %in% combinations.RN$prod_name_mod |
                !SpeciesASFISspecies %in% combinations.RN$SpeciesASFISspecies) 
      
      # if temp selection is not empty, select minumum distance and copy to ouput df
      if (nrow(temp)>0){
        temp<- temp %>% filter(distance_ca ==  min(temp$distance_ca))
        combinations.RN <- rbind(combinations.RN, temp)}}

  }  
  
  output_df <- unique(combinations.RN)
  
  output_df <- left_join(output_df, input_df, by=c('prod_name_mod'='prod_name_mod',
                                                   'SpeciesASFISspecies'='SpeciesASFISspecies'))
  
  return(output_df)
  
}





### for multiple columns
consump_df_temp <- consump_df_temp %>%
                   mutate(distance_sum <- distance_ca + distance_aq + distance_tr)


# sum distance columns


# sort by distance



# get count of each prod_name
nb_consump_prod <- nrow(subset(consump_df_temp, prod_name==as.character(consump_df_temp[i,'prod_name'])))
nb_ca_prod <- nrow(subset(consump_df_temp, SpeciesASFISspecies==as.character(consump_df_temp[i,'SpeciesASFISspecies'])))

df <- read.csv('../output/consumption/hh_survey_consump_sep_prods_refuse_fwaes.csv', stringsAsFactors=FALSE)



cols_to_keep<- c("country.x","year","prod_name","prod_src_fm", "pre_refuse_consump","refuse_factor",
                 "Averageediblequantityconsumedgpersonday",  
                 "consump_million.tons.yr", "product",
                 "mean_conv_fac")


df_sp <-df %>%
        select(one_of(cols_to_keep)) %>%
        mutate(product=ifelse(is.na(product),"assumed fresh",product))





new_colnames <- c("country","year","product name","source (F/M)",
                  "pre_refuse_consumption (g/cap/day)","refuse_factor",
                 "post_refuse_consumption (g/cap/day)",   
                 "consump_million.tons.yr", "FWAE processing keyword",
                 "mean_conv_fac")


names(df_sp) <-new_colnames



write.csv(df_sp, '../docs/man/for_submission/SP2_product_table.csv')



# run script that formats the trade data 
#source('./fishstatj_data/read_nat_trade_data_fw.R', print.eval = TRUE)
source('./fishstatj_data/read_nat_catch_data.R', print.eval = TRUE)
rm(ca)#, tr_inl_sel, tr_inl_sel_fwae)  # delete unused df from sourced scripts


ca_sum_bycountry2 <-  ca_sum_bycountry %>%
                      filter(year == 2014) %>%
                      spread()


# # filter aq and ca  to only freshwater catch
# aq_sum_bycountry <- aq_sum_bycountry %>% 
#   filter(source == 'Inland') %>% ungroup() %>% select(-c(source, type))
# ca_sum_bycountry <- ca_sum_bycountry %>% 
#   filter(source == 'Inland') %>% ungroup() %>% select(-c(source, type))

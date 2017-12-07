
consump_df2<- consump_df[c('product','consump_million.tons.yr')]

consump_df2<- consump_df2 %>%
              mutate(product=ifelse(is.na(product),'assumed fresh',product))%>%
              group_by(product) %>%
              summarize(count = n(), sum_consump=sum(consump_million.tons.yr)) %>%
              mutate(prod_regroup= ifelse(product %in% fresh, 'fresh', NA),
                     assumed= ifelse(product == 'assumed fresh', 'assumed', NA),
                     prod_regroup= ifelse(product %in% dried, 'dried', prod_regroup),
                     prod_regroup= ifelse(product %in% smoked, 'smoked', prod_regroup),
                     prod_regroup= ifelse(product %in% salted, 'salted', prod_regroup))



ggplot(consump_df) +
  geom_bar(aes(x=prod_regroup, count))


fresh<-c('fresh','fresco','frais')
dried<-c('dried','seco','seché')
smoked<-c('smoked','ahumado','fumé')
salted<-c('salé', 'salado','salted')
frozen<-c('frozen', 'salado','salted')

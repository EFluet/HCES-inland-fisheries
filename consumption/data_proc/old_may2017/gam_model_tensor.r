data[,c('x','y','z')] <- appcon[,c('clpa', 'meanNY.GDP.PCAP.CD', 'F_tototal')]


sampled_models <- data.frame(nrep = seq_len(50)) %>% 
  group_by(nrep) %>% 
  do(data[data %>% nrow %>% sample.int(replace = TRUE),]) %>% 
  do(model = gam(z ~ s(x)+ s(y), data = .)) 

# creates evenly distributed xs for predictions
xs_evendist <- data$x %>% range %>% (function(rg) seq(rg[1],rg[2],length.out = 500)) 
ys_evendist <- data$y %>% range %>% (function(rg) seq(rg[1],rg[2],length.out = 500)) 



### Make prediction over countries, for use in survey calculation
boot_pred <- sampled_models %>% 
  rowwise %>% 
  do(data.frame(z_hat = predict(.$model,list(x = data$x, y = data$y)))) %>%
  ungroup %>%
  cbind(x=data$x, y=data$y, z=data$z) %>%
  group_by(x, y, z) %>%
  summarize(mean = mean(z_hat),
            up = quantile(z_hat, probs = 0.975),
            lo = quantile(z_hat, probs = 0.025))



# Predictions for the 0-1 
boot_pred_even <- sampled_models %>% 
  rowwise %>% 
  do(data.frame(z_hat = predict(.$model,list(x = xs_evendist, y = ys_evendist)))) %>%
  ungroup %>%
  cbind(xs=xs_evendist, ys=ys_evendist) %>%
  group_by(xs, ys) %>%
  #mutate(run = rep(1:100, each = 500)) %>%
  summarize(mean = mean(z_hat),
            p975 = quantile(z_hat, probs = 0.975),
            p025 = quantile(z_hat, probs = 0.025))




#
ggplot(boot_pred_even) + 
  geom_line(aes(x = xs, y = mean))  +
  geom_point(data=boot_pred, aes(x = x, y = z), color='red')+
  #geom_ribbon(aes(x = xs, ymin=p025, ymax=975), color='grey')#+
  #stat_smooth(method = "loess") +
  geom_line(aes(x = xs, y = p975)) +
  geom_line(aes(x = xs, y = p025))+
  ylim(0,1)





### plot scatter of difference between z and preds
ggplot(boot_pred) + 
  geom_point(aes(x = z, y = mean), color='black') +
  geom_errorbar(aes(x=z, ymin=lo, ymax=up)) +
  geom_abline(intercept=0, slope=1, color='grey') +
  ylim(0,1) + xlim(0,1)

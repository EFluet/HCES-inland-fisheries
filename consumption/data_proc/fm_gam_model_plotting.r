

# rotatable 3D plot of points and spline surface -----------------------------
# create grid on which predictions will be made
grid <- expand.grid(x = seq(0, max(x), length=100), 
                    y = seq(0, max(y), length=100))

# predict value at grids
grid$z <- predict(b1, newdata=grid) 

matrixz <- matrix(grid$z, 
                  nrow=length(unique(grid$x)), 
                  ncol=length(unique(grid$y)))

# make plot
library(rgl)
open3d()
surface3d(unique(grid$x), unique(grid$y),  z= matrixz, alpha=0.5,  front= "lines", lit=F) 
#wire3d(unique(grid$x), unique(grid$y),  z= matrixz) 
points3d(x,y,z, col='red', size=8)
aspect3d(1, 1, 1)

# Put 4 x-axes on the plot
axes3d(c('x+-','y+-','z+-'))
axis3d('x', pos = c(1, NA, NA))

title3d(xlab="clpa",ylab="gdp",zlab="F:M")

rgl.viewpoint(theta=0, phi=15)
rgl.snapshot( "../output/figures/persp3dd.png", fmt = "png", top = TRUE )
#rgl.postscript("../output/figures/persp3dd.pdf","pdfng")


### ----------------------------------------------------------------



# creates evenly distributed xs for predictions
xs_evendist <- data$x %>% range %>% (function(rg) seq(rg[1],rg[2],length.out = 100)) 
ys_evendist <- data$y %>% range %>% (function(rg) seq(rg[1],rg[2],length.out = 100)) 



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



### save plot ------------------------------------------------------------------
ggsave("../Output/Figures/FtoM_gam_clpa_FtoM_boot.png",
       dpi=600, width=210, height=180, units='mm', type = "cairo-png")
dev.off()



### plot scatter of difference between z and preds
ggplot(boot_pred) + 
  geom_point(aes(x = z, y = mean), color='black') +
  geom_errorbar(aes(x=z, ymin=lo, ymax=up)) +
  geom_abline(intercept=0, slope=1, color='grey') +
  ylim(0,1) + xlim(0,1)









# ### Fit LOESS ---------------------------------------------------
# F_tototal<-appcon$F_tototal
# clpa<-appcon$clpa
# 
# 
# lo <- loess(F_tototal ~ clpa, span=0.8)
# 
# lo.b <- loess.boot(lo,  R=10000, rows=TRUE)#, ngrid = 90, new.xpts = NULL, weights = NULL)
# 
# # get fitted values
# f <- as.data.frame(samples(lo.b, name = c("fitted")))
# 
# conf_97.5 <- apply(as.data.frame(f), 1, function(x) quantile(x, .975, na.rm=TRUE))
# conf_2.5 <- apply(as.data.frame(f), 1, function(x) quantile(x, .025, na.rm=TRUE))
# 
# # append the 95% c.i. bounds to the df
# loess_output <- as.data.frame(cbind(predict(lo), conf_2.5, conf_97.5))
# names(loess_output) <- c("loess_pred","loess_conf_2.5", "loess_conf_97.5") 
# 
# 
# # combined colums of appcon and loess
# appcon <- cbind(appcon, loess_output)
# 
# 
# # apply a filter for labels in the plot
# appcon <- appcon %>%
#   mutate(label= ifelse(F_tototal > loess_conf_97.5 * 2 | F_tototal < loess_conf_2.5 / 2 , 
#                        'Y', 'N'))
# 
# 
# 
# 
# ### Scatterplot of countries ---------------------------------------------------
# source('./consumption/plots/themes/custom_scatterplot_theme.r')
# 
# ggplot(appcon, aes(x=clpa, y=F_tototal)) +
#   
#   geom_ribbon(aes(x=appcon$clpa, ymin=appcon$loess_conf_2.5, ymax=appcon$loess_conf_97.5), fill='blue', alpha=0.1) +
#   geom_line(aes(x=appcon$clpa, y=loess_pred), color='blue',size= 1, alpha=0.6) +
#   geom_point(aes(color=continent), size=3, shape=21, stroke=2, alpha=1) +
#   
#   
#   xlab('Coastline length per unit surface water area (GIEMS-D15; MAMax)') +
#   ylab('Percentage freshwater in total fish \napparent consumption FAO (avg:1995-2014)') +
#   coord_cartesian(ylim=c(0,1))+
#   #facet_wrap(~continent) +
#   geom_text_repel(aes(label=Country, x= clpa, y= F_tototal),
#                   data = subset(appcon, label =='Y'),
#                   size = 3,
#                   colour='gray15',
#                   force = 8,
#                   segment.size = 0.25,
#                   segment.color='gray55',
#                   box.padding = unit(0.2, 'lines'),
#                   point.padding = unit(0.2, 'lines'))  + 
#   custom_scatterplot_theme +
#   theme(legend.position = c(0.75, 0.75))
# #annotate("text", x = 150, y = .85, label = "f(x)= 0.69 * e^(-0.037x) \n R^2=0.74")
# 
# 
# ### save plot ------------------------------------------------------------------
# ggsave("../Output/Figures/FtoM_withFAO_apparentconsump_bootloess.png",
#        dpi=600, width=210, height=180, units='mm', type = "cairo-png")
# dev.off()
# 
# 
# 
# ### 
# # because this script is sourced in data processing
# # remove 
# 
# rm(lo.b, loess_output, w_area, coast_length, appcon, F_tototal, 
#    custom_scatterplot_theme, conf_2.5, conf_97.5)

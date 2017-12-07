rm(list = ls())
library(datasets)
library(ggplot2)

data(airquality)


p7 <- ggplot(airquality, aes(x = Ozone)) +
  geom_histogram(binwidth = 50)
p7



consump_df2 <- consump_df2 %>%
  mutate(frac=ifelse(frac==1,0.94,frac))


data <- data.frame(number = c(5, 10, 11 ,12,12,12,13,15,15))
ggplot(data,aes(x = number)) + geom_histogram(binwidth = 0.5)

# Here is a trick with the tick label to get the bar align on the left.. But if you add other data, you need to shift them also

ggplot(data,aes(x = number)) + 
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(
    breaks=seq(0.5, 15.5, 1), 
    #show x-ticks align on the bar (0.25 before the value, half of the binwidth) 
    labels = 1:16 #change tick label to get the bar x-value
  )



#binwidth = 1, breaks=seq(0.5,15.5, 1)

binwidth = .2


ggplot(consump_df2, aes(x = frac)) + 
  geom_histogram(binwidth = binwidth) + 
  scale_x_continuous(
    breaks=seq(-0.2, 1, binwidth), 
    #show x-ticks align on the bar (0.25 before the value, half of the binwidth) 
    labels = seq(-0.2, 1, binwidth) #change tick label to get the bar x-value
  )


breaks<- seq(0,100,20)
  
ggplot(consump_df2, aes(x = frac*100)) + 
  geom_histogram(aes(y=..density..), breaks=breaks) + 
  scale_x_continuous(breaks=breaks)




Y1 <- 100
set.seed(101) ## for reproducibility
x.pois<-rpois(Y1, 20)
hist(x.pois, breaks=100,freq=FALSE)
lines(density(x.pois, bw=0.8), col="red")
library(MASS)
(my.mle<-fitdistr(x.pois, densfun="poisson"))



vec<-rgamma(100,1.5,-2.6)
hist(vec)


library(vcd)
gf<-goodfit(consump_df2$frac,type= "poisson",method= "MinChisq")
gf


samp <- unlist(c(subset(consump_df2,geo_pos=='Landlocked')['frac']))

library(MASS) ## loading package MASS
g <- fitdistr(subsetconsump_df2$frac['geo_pos'], "gamma")

library(fitdistrplus)


fw <- fitdist(samp, "weibull")
fg <- fitdist(samp, "gamma")
fln <- fitdist(samp, "lnorm")
fp <- fitdist(samp, "pois")
fb <- fitdist(samp, "nbinom", method="mme")

par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma")

denscomp(list(fw, fln, fg), legendtext = plot.legend)
qqcomp(list(fw, fln, fg), legendtext = plot.legend)
cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
ppcomp(list(fw, fln, fg), legendtext = plot.legend)






g <- fitdistr(samp,"gamma") ## Fitting
hist(samp, freq=FALSE) ## PLotting the histogram
lines(density(g, bw=0.8))

curve(g,add=TRUE,col="green",lwd=3) ## Plotting the line




chi_df <- fitdistr(samp,"chi-squared") ## Fitting





library(MASS)
nnn <- 1000
set.seed(101)
chii <- rchisq(nnn,4, ncp = 0) ## Generating a chi-sq distribution
chi_df <- fitdistr(chii,"chi-squared",start=list(df=3),method="BFGS") ## Fitting
chi_k <- chi_df[[1]][1] ## Degrees of freedom
chi_hist <- hist(chii,breaks=50,freq=FALSE) ## PLotting the histogram
curve(dchisq(x,df=chi_k),add=TRUE,col="green",lwd=3) ## Plotting the line







library(MASS)
species <- c('acesac', 'acesac', 'acesac', 'acesac', 'acesac', 'acesac',
             'polbif', 'polbif', 'polbif', 'polbif', 'polbif', 'polbif')
tmean <- c(2,3,5,6,6,7,5,6,6,6,8,9) 
Data <- data.frame(species, tmean) 

for (i in unique(Data$species)) {
  subdata <- subset(Data, species ==i)
  dist <- fitdistr(subdata$tmean, "gamma")
  hist(subdata$tmean, main = i)
  curve(dgamma(x, shape = dist$estimate[1], rate = dist$estimate[2]), 
        add = TRUE,
        col = "red")
}





samp <- unlist(c(subset(consump_df2,geo_pos=='Coastal')['frac']))

samp <- unlist(unname(c(subset(consump_df2,geo_pos=='Landlocked')['frac'])))
samp <- sort(1 - samp)

#fitdistr(x,"beta",list(shape1=1,shape2=1))

#subdata <- subset(Data, species ==i)
dist <- fitdistr(samp, "beta")#, list(shape1=1,shape2=1))
hist(samp, main = i)
curve(dbeta(x, shape1 = dist$estimate[1] , dist$estimate[2]), #rate = dist$estimate[2]), 
      add = TRUE,
      col = "red")




#subdata <- subset(Data, species ==i)
dist <- fitdistr(samp, "gamma",  list(shape = 1, rate = 0.1))
hist(samp, main = i)
curve(dgamma(x, shape = dist$estimate[1] , rate = dist$estimate[2]), 
      add = TRUE,
      col = "red")



#subdata <- subset(Data, species ==i)
dist <- fitdistr(samp, "exponential")
hist(samp, main = i)
curve(dexp(x, rate = dist$estimate[1]), #rate = dist$estimate[2]), 
      add = TRUE,
      col = "red")


curve(dexp(x, dist))




x<-rnorm(n=100,mean=0.5,sd=0.1); 
fitdistr(x,dbeta,start=list(shape1=1,shape2=1))
         
Beerwings <- read.csv("~/Documents/math133/datasets/Beerwings.csv")

library(ggplot2)
library(dplyr)

Beerwings %>% 
  ggplot(aes(x=Beer, y=Hotwings, col=Gender))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)

hw_lm = lm(Hotwings~Beer, data=Beerwings)

#predicting
y=Beerwings$Hotwings
x=Beerwings$Beer

yhat = predict(hw_lm, Beerwings)

#install.packages("Metrics")
library(Metrics)

rmse_beer= rmse(y, yhat)
c(rmse_beer)


election2016 <- read.csv("~/Documents/math133/datasets/election2016.csv")

head(election2016)

#load tidyverse
install.packages('ggplot2')
install.packages('dplyr')
library(ggplot2)
library(dplyr)

election2016 %>% 
  ggplot(aes(x=non.white, y=trump.vote))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)

#fitting linear model
nw_lm = lm(trump.vote~non.white, data=election2016)
metro_lm = lm(trump.vote~metro, data=election2016)


#predicting
y=election2016$trump.vote
x=election2016$non.white
yhat = predict(nw_lm, election2016)
yhat2 = predict(metro_lm, election2016)

mse=mean((y-yhat)^2)
mse2 = mean((y-yhat2)^2)
c(mse, mse2)

#more complicated plot
election2016 %>% 
  ggplot(aes(x=non.white, y=trump.vote, col=outcome, size=median.income))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  labs(x="Non White", y="Trump Vote", title="Plot")+
  scale_color_manual(values=c("blue", "red"))

#Filtering and selecting
red = election2016 %>% filter(outcome == "Red") %>%
  select(c("trump.vote", "metro"))
glimpse(red)

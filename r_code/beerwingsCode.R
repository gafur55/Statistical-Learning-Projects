# Load Data
Beerwings <- read.csv("../Datasets/Beerwings.csv")

# Problem 1
Beerwings %>% ggplot(aes(x=Beer,y=Hotwings))+
  geom_point() +
  labs(x="Beer (oz)",y="Hotwings")+
  geom_smooth(method="lm",se=FALSE)

# Problem 2
bw_lm=lm(Hotwings~Beer,data=Beerwings)
bw_lm

# Problem 3
y=Beerwings$Hotwings
yhat=predict(bw_lm,Beerwings)
rmse=sqrt(mean((y-yhat)^2))
rmse

# Problem 4
Beerwings %>% ggplot(aes(x=Beer,y=Hotwings,col=Gender))+
  geom_point() +
  labs(x="Beer (oz)",y="Hotwings")+
  geom_smooth(method="lm",se=FALSE)+
  scale_color_manual(values=c("#E69F00","#009E73"))

